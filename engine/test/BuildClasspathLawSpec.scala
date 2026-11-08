package histos.load

import histos.jdk.Sha256
import histos.pack.JarError
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import zio.{Chunk, ZIO}
import zio.test.*

/** An assembly yields the api jar it carries. A classes directory is packed into the cache. Any other file is already
  * that classpath.
  */
object BuildClasspathLawSpec extends ZIOSpecDefault:
  private val apiBytes = Array[Byte](1, 2, 3, 9)

  def spec = suite("build classpath")(
    test("a missing path, a broken zip, and a zip that is not the program are returned as themselves"):
      ZIO.scoped {
        Workspace.work("histos-classpath-plain").flatMap { root =>
          val missing = root.resolve("missing.jar")
          val text    = root.resolve("notes.txt")
          val broken  = root.resolve("broken.jar")
          val program = root.resolve("program.jar")
          val nested  = root.resolve("nested.jar")
          for
            _        <- write(text, "notes")
            _        <- write(broken, "not a zip")
            _        <- zip(program, "histos/cli/Main.class" -> Array[Byte](1))
            _        <- zip(nested, BuildClasspath.Entry -> apiBytes)
            absent   <- BuildClasspath(missing, root.resolve("cache-a"))
            notes    <- BuildClasspath(text, root.resolve("cache-c"))
            corrupt  <- BuildClasspath(broken, root.resolve("cache-d"))
            mainOnly <- BuildClasspath(program, root.resolve("cache-e"))
            apiOnly  <- BuildClasspath(nested, root.resolve("cache-f"))
          yield assertTrue(
            absent == missing,
            notes == text,
            corrupt == broken,
            mainOnly == program,
            apiOnly == nested,
          )
        }
      }
    ,
    test("a classes directory is a jar in the cache, and a changed class is a new jar"):
      ZIO.scoped {
        Workspace.work("histos-classpath-classes").flatMap { root =>
          val classes = root.resolve("classes")
          val cache   = root.resolve("cache")
          val file    = classes.resolve("demo").resolve("A.class")
          val empty   = root.resolve("empty")
          val planted = Array[Byte](9, 9)
          for
            _       <- writeBytes(file, Array[Byte](1, 2, 3))
            _       <- Workspace.createDirectories(empty).catchAll(error => ZIO.dieMessage(error.message))
            packed  <- BuildClasspath(classes, cache)
            first   <- entryBytes(packed, "demo/A.class")
            regular <- ZIO.attemptBlocking(Files.isRegularFile(packed)).orDie
            again   <- BuildClasspath(classes, cache)
            _       <- writeBytes(packed, planted)
            kept    <- BuildClasspath(classes, cache)
            stayed  <- ZIO.attemptBlocking(Files.readAllBytes(kept)).orDie
            _       <- writeBytes(file, Array[Byte](4))
            moved   <- BuildClasspath(classes, cache)
            second  <- entryBytes(moved, "demo/A.class")
            refused <- BuildClasspath(empty, cache).either
            inside  <- BuildClasspath(classes, classes.resolve("cache")).either
          yield
            val nested = inside match
              case Left(LoadError.Jar(JarError.DestinationInside(path))) =>
                path.toAbsolutePath.normalize.startsWith(classes.toAbsolutePath.normalize)
              case _ => false
            assertTrue(
              packed != classes,
              regular,
              packed.startsWith(cache),
              again == packed,
              first.contains(List[Byte](1, 2, 3)),
              kept == packed,
              stayed.toList == planted.toList,
              moved != packed,
              second.contains(List[Byte](4)),
              refused == Left(LoadError.Jar(JarError.Empty(empty))),
              nested,
            )
        }
      }
    ,
    test("an assembly is the api jar inside it, and a jar already at that digest is left in place"):
      ZIO.scoped {
        Workspace.work("histos-classpath-assembly").flatMap { root =>
          val assembly = root.resolve("histos.jar")
          val cache    = root.resolve("cache")
          val sha      = Sha256.of(Chunk.fromArray(apiBytes)).value
          val dest     = cache.resolve("api").resolve(sha).resolve("histos-api.jar")
          val planted  = Array[Byte](9, 9)
          for
            _       <- zip(assembly, "histos/cli/Main.class" -> Array[Byte](1), BuildClasspath.Entry -> apiBytes)
            _       <- writeBytes(dest, planted)
            kept    <- BuildClasspath(assembly, cache)
            stayed  <- ZIO.attemptBlocking(Files.readAllBytes(dest)).orDie
            _       <- ZIO.attemptBlocking(Files.delete(dest)).orDie
            written <- BuildClasspath(assembly, cache)
            fresh   <- ZIO.attemptBlocking(Files.readAllBytes(written)).orDie
            again   <- BuildClasspath(assembly, cache)
          yield assertTrue(
            kept == dest,
            stayed.toList == planted.toList,
            written == dest,
            again == dest,
            fresh.toList == apiBytes.toList,
          )
        }
      }
  )

  private def write(path: Path, text: String) =
    Workspace.writeString(path, text).catchAll(error => ZIO.dieMessage(error.message))

  private def writeBytes(path: Path, bytes: Array[Byte]) =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
      }
      .orDie

  private def entryBytes(jar: Path, name: String) =
    ZIO
      .attemptBlocking {
        val zip = new ZipFile(jar.toFile)
        try
          Option(zip.getEntry(name)).map { entry =>
            val input = zip.getInputStream(entry)
            try input.readAllBytes().toList
            finally input.close()
          }
        finally zip.close()
      }
      .orDie

  private def zip(path: Path, entries: (String, Array[Byte])*) =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          entries.foreach { (name, bytes) =>
            out.putNextEntry(new ZipEntry(name))
            out.write(bytes)
            out.closeEntry()
          }
        finally out.close()
      }
      .orDie
end BuildClasspathLawSpec
