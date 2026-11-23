package histos.splice

import zio.*
import zio.test.*

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream

import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.jar.{JarEntry, JarOutputStream}

object ResolveSpec extends ZIOSpecDefault:

  import SpliceLib.sha256

  def spec =
    suite("Resolve")(
      test("vendor file maps to the path on disk") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _ <- write(foo, "export const x = 1;")
          env = envAt(dir)
          got <- Splice.resolve(Seq(Splice.file("foo", foo.toFile)), env)
        yield assertTrue(got("foo") == foo)
      },
      test("duplicate specifiers fail") {
        for
          dir <- tempDir
          foo = dir.resolve("foo.js")
          _   <- write(foo, "export const x = 1;")
          err <- Splice
            .resolve(
              Seq(Splice.file("foo", foo.toFile), Splice.file("foo", foo.toFile)),
              envAt(dir),
            )
            .flip
        yield assertTrue(err == SpliceError.DuplicateLib("foo"))
      },
      test("CDN without sha256 fails before fetch") {
        for
          dir <- tempDir
          err <- Splice
            .resolve(
              Seq(Splice.lib("foo", "1.0.0", "foo.js")),
              envAt(dir).copy(resolvers = Seq(Splice.jsDelivr)),
            )
            .flip
        yield assertTrue(err == SpliceError.MissingSha256("foo"))
      },
      test("CDN with no CDN resolver fails") {
        for
          dir <- tempDir
          err <- Splice
            .resolve(
              Seq(Splice.lib("foo", "1.0.0", "foo.js").sha256("abcd")),
              envAt(dir).copy(resolvers = Seq(Splice.maven)),
            )
            .flip
        yield assertTrue(err == SpliceError.NoResolver("foo", "cdn"))
      },
      test("jsDelivr-shaped HTTP fetch verifies sha256") {
        val body = """export function greet() { return "ok"; }"""
        val pin  = hex(body.getBytes(StandardCharsets.UTF_8))
        ZIO.scoped {
          for
            port <- serve(body)
            dir  <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localCdn(port)))
            got <- Splice.resolve(
              Seq(Splice.lib("foo", "1.0.0", "foo.js").sha256(pin)),
              env,
            )
            bytes <- ZIO.attempt(Files.readAllBytes(got("foo")))
          yield assertTrue(
            got.contains("foo"),
            String(bytes, StandardCharsets.UTF_8) == body,
          )
        }
      },
      test("wrong sha256 fails and does not fall across CDNs") {
        val body = """export function greet() { return "ok"; }"""
        ZIO.scoped {
          for
            port <- serve(body)
            dir  <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localCdn(port), Splice.unpkg))
            err <- Splice
              .resolve(
                Seq(Splice.lib("foo", "1.0.0", "foo.js").sha256("0" * 64)),
                env,
              )
              .flip
          yield assertTrue(
            err.isInstanceOf[SpliceError.ChecksumMismatch],
            err.asInstanceOf[SpliceError.ChecksumMismatch].specifier == "foo",
          )
        }
      },
      test("cache hit does not need the server (LocalOnly)") {
        val body = """export function greet() { return "ok"; }"""
        val pin  = hex(body.getBytes(StandardCharsets.UTF_8))
        for
          dir  <- tempDir
          port <- Ref.make(0)
          _    <- ZIO.scoped {
            for
              p <- serve(body)
              _ <- port.set(p)
              env = envAt(dir).copy(resolvers = Seq(localCdn(p)))
              _ <- Splice.resolve(Seq(Splice.lib("foo", "1.0.0", "foo.js").sha256(pin)), env)
            yield ()
          }
          p <- port.get
          offline = envAt(dir).copy(resolvers = Seq(localCdn(p)), localOnly = true)
          got <- Splice.resolve(Seq(Splice.lib("foo", "1.0.0", "foo.js").sha256(pin)), offline)
        yield assertTrue(got.contains("foo"), Files.isRegularFile(got("foo")))
      },
      test("WebJar extract reads META-INF/resources/webjars") {
        for
          dir <- tempDir
          jar <- jarWith(dir, "foo", "1.0.0", "foo.js", """export function greet() { return "ok"; }""")
          env = envAt(dir).copy(
            resolvers = Seq(Splice.maven),
            webjars = Map("foo" -> jar),
          )
          got  <- Splice.resolve(Seq(Splice.webjar("foo", "1.0.0", "foo.js")), env)
          text <- ZIO.attempt(Files.readString(got("foo")))
        yield assertTrue(text.contains("greet"))
      },
      test("WebJar missing path fails") {
        for
          dir <- tempDir
          jar <- jarWith(dir, "foo", "1.0.0", "other.js", "export const x = 1;")
          env = envAt(dir).copy(
            resolvers = Seq(Splice.maven),
            webjars = Map("foo" -> jar),
          )
          err <- Splice.resolve(Seq(Splice.webjar("foo", "1.0.0", "foo.js")), env).flip
        yield assertTrue(err.isInstanceOf[SpliceError.MissingJarPath])
      },
      test("WebJar without maven resolver fails") {
        for
          dir <- tempDir
          err <- Splice
            .resolve(
              Seq(Splice.webjar("foo", "1.0.0", "foo.js")),
              envAt(dir).copy(resolvers = Seq.empty),
            )
            .flip
        yield assertTrue(err == SpliceError.NoResolver("foo", "maven"))
      },
      test("GitHub without sha256 fails before fetch") {
        for
          dir <- tempDir
          err <- Splice
            .resolve(
              Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js")),
              envAt(dir).copy(resolvers = Seq(Splice.github)),
            )
            .flip
        yield assertTrue(err == SpliceError.MissingSha256("foo"))
      },
      test("GitHub with no GitHub resolver fails") {
        for
          dir <- tempDir
          err <- Splice
            .resolve(
              Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js").sha256("abcd")),
              envAt(dir).copy(resolvers = Seq(Splice.jsDelivr, Splice.maven)),
            )
            .flip
        yield assertTrue(err == SpliceError.NoResolver("foo", "github"))
      },
      test("GitHub tag tarball extracts the pinned path after stripping the root dir") {
        val body = """export function greet() { return "ok"; }"""
        ZIO.scoped {
          for
            packed <- ZIO.attempt(tarGz("repo-1.0.0", "dist/foo.js", body))
            port   <- serveBytes("/owner/repo/archive/refs/tags/1.0.0.tar.gz", packed)
            dir    <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localGithub(port)))
            got <- Splice.resolve(
              Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js").sha256(hex(packed))),
              env,
            )
            text <- ZIO.attempt(Files.readString(got("foo")))
          yield assertTrue(text == body)
        }
      },
      test("GitHub 404 on the exact tag tries the v-prefixed tag") {
        val body = """export function greet() { return "ok"; }"""
        ZIO.scoped {
          for
            packed <- ZIO.attempt(tarGz("repo-1.0.0", "dist/foo.js", body))
            port   <- serveBytes("/owner/repo/archive/refs/tags/v1.0.0.tar.gz", packed)
            dir    <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localGithub(port)))
            got <- Splice.resolve(
              Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js").sha256(hex(packed))),
              env,
            )
            text <- ZIO.attempt(Files.readString(got("foo")))
          yield assertTrue(text == body)
        }
      },
      test("GitHub wrong sha256 fails and does not fall across CDNs") {
        val body = """export function greet() { return "ok"; }"""
        ZIO.scoped {
          for
            packed <- ZIO.attempt(tarGz("repo-1.0.0", "dist/foo.js", body))
            port   <- serveBytes("/owner/repo/archive/refs/tags/1.0.0.tar.gz", packed)
            dir    <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localGithub(port), Splice.jsDelivr))
            err <- Splice
              .resolve(
                Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js").sha256("0" * 64)),
                env,
              )
              .flip
          yield assertTrue(
            err.isInstanceOf[SpliceError.ChecksumMismatch],
            err.asInstanceOf[SpliceError.ChecksumMismatch].specifier == "foo",
          )
        }
      },
      test("GitHub archive missing path fails") {
        val body = "export const x = 1;"
        ZIO.scoped {
          for
            packed <- ZIO.attempt(tarGz("repo-1.0.0", "dist/other.js", body))
            port   <- serveBytes("/owner/repo/archive/refs/tags/1.0.0.tar.gz", packed)
            dir    <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localGithub(port)))
            err <- Splice
              .resolve(
                Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js").sha256(hex(packed))),
                env,
              )
              .flip
          yield assertTrue(err.isInstanceOf[SpliceError.MissingArchivePath])
        }
      },
      test("GitHub 404 on both tag URLs fails") {
        ZIO.scoped {
          for
            port <- serveBytes("/nope.tar.gz", Array.emptyByteArray)
            dir  <- tempDir
            env = envAt(dir).copy(resolvers = Seq(localGithub(port)))
            err <- Splice
              .resolve(
                Seq(Splice.github("foo", "owner/repo", "1.0.0", "dist/foo.js").sha256("0" * 64)),
                env,
              )
              .flip
          yield assertTrue(err.isInstanceOf[SpliceError.NotFound])
        }
      },
      test("GitHub repository must be owner/repo") {
        for
          dir <- tempDir
          err <- Splice
            .resolve(
              Seq(Splice.github("foo", "not-a-repo", "1.0.0", "dist/foo.js").sha256("0" * 64)),
              envAt(dir).copy(resolvers = Seq(Splice.github)),
            )
            .flip
        yield assertTrue(err.isInstanceOf[SpliceError.Io])
      },
    )

  private def localCdn(port: Int): SpliceResolver =
    Splice.cdn("local") { (n, v, p) =>
      s"http://127.0.0.1:$port/npm/$n@$v/$p"
    }

  private def localGithub(port: Int): SpliceResolver =
    Splice.githubArchive { (owner, repo, tag) =>
      s"http://127.0.0.1:$port/$owner/$repo/archive/refs/tags/$tag.tar.gz"
    }

  private def envAt(dir: Path): ResolveEnv =
    ResolveEnv(
      resolvers = Seq(Splice.maven),
      cacheDir = dir.resolve("csr"),
      localOnly = false,
      webjars = Map.empty,
      extractDir = dir.resolve("extracted"),
    )

  private def serve(body: String): ZIO[Scope, Throwable, Int] =
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    serveBytes("/npm/foo@1.0.0/foo.js", bytes)

  private def serveBytes(path: String, bytes: Array[Byte]): ZIO[Scope, Throwable, Int] =
    ZIO
      .acquireRelease(ZIO.attempt {
        val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext(
          "/",
          (ex: HttpExchange) =>
            try
              if ex.getRequestURI.getPath == path then
                ex.sendResponseHeaders(200, bytes.length)
                ex.getResponseBody.write(bytes)
              else ex.sendResponseHeaders(404, -1)
            finally ex.close(),
        )
        server.setExecutor(null)
        server.start()
        server
      })(s => ZIO.succeed(s.stop(0)))
      .map(_.getAddress.getPort)

  private def tarGz(root: String, path: String, body: String): Array[Byte] =
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    val baos  = new ByteArrayOutputStream
    val tar   = new TarArchiveOutputStream(new GzipCompressorOutputStream(baos))
    try
      tar.setLongFileMode(TarArchiveOutputStream.LONGFILE_POSIX)
      val entry = new TarArchiveEntry(s"$root/$path")
      entry.setSize(bytes.length)
      tar.putArchiveEntry(entry)
      tar.write(bytes)
      tar.closeArchiveEntry()
    finally tar.close()
    baos.toByteArray

  private def jarWith(dir: Path, name: String, version: String, path: String, body: String): Task[Path] =
    ZIO.attempt {
      val jar = dir.resolve(s"$name-$version.jar")
      val out = new JarOutputStream(Files.newOutputStream(jar))
      try
        val entry = new JarEntry(s"META-INF/resources/webjars/$name/$version/$path")
        out.putNextEntry(entry)
        out.write(body.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
      finally out.close()
      jar
    }

  private def hex(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map("%02x".format(_)).mkString

  private def tempDir: UIO[Path] =
    ZIO.attempt(Files.createTempDirectory("sbt-splice-resolve-")).orDie

  private def write(path: Path, body: String): Task[Unit] =
    ZIO.attempt {
      Files.writeString(path, body)
      ()
    }
end ResolveSpec
