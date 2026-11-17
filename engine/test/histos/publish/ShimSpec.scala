package histos.publish

import histos.jdk.Sha256
import histos.workspace.Workspace

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit

import zio.ZIO
import zio.test.*

object ShimSpec extends ZIOSpecDefault:
  def spec = suite("the install shim")(
    test("a coordinate manifest does not need a dist jar") {
      ZIO.scoped {
        for
          root       <- Workspace.work("histos-shim-spec").orDieWith(error => new java.io.IOException(error.message))
          coordinate <- kind(root, coordinateManifest)
          dist       <- kind(root, distManifest)
          missing    <- kind(root, "{\n  \"version\": \"0.5.0\"\n}\n")
        yield assertTrue(coordinate == "coordinate", dist == "dist", missing == "missing")
      }
    },
    test("the checkout script replaces a shim whose hash differs") {
      val body = LauncherScript.posix(Sha256.trusted("ab" * 32))
      assertTrue(
        body.contains("""[ ! -x "$shim" ] || [ "$(hash_of "$shim")" != "$shim_sha" ]""")
      )
    },
    test("installing coursier does not move the download onto itself") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-shim-mv").orDieWith(error => new java.io.IOException(error.message))
          file <- ZIO.fromEither(script).mapError(message => new java.io.IOException(message))
          text <- ZIO.attemptBlocking(Files.readString(file)).orDie
          sha  <- ZIO
            .fromOption(coursierSha(text))
            .orElseFail(new java.io.IOException("coursier_sha is not in the shim"))
          bin  = root.resolve("bin")
          home = root.resolve("home")
          log  = root.resolve("mv.log")
          _     <- ZIO.attemptBlocking(Files.createDirectories(bin)).orDie
          _     <- executable(bin.resolve("curl"), curlStub)
          _     <- executable(bin.resolve("sha256sum"), shaStub)
          _     <- executable(bin.resolve("mv"), mvStub)
          out   <- runInstall(file, bin, home, log, sha)
          moved <- ZIO.attemptBlocking {
            if Files.isRegularFile(log) then Files.readString(log).split('\n').toList.filter(_.nonEmpty)
            else Nil
          }.orDie
        yield
          val from = moved.headOption.getOrElse("")
          val to   = moved.lift(1).getOrElse("")
          assertTrue(
            moved.length == 2,
            from != to,
            to.endsWith("coursier.jar"),
            !out.contains("are the same file"),
            !out.contains("manifest version is not"),
          )
      }
    },
  )

  private def kind(root: Path, text: String): ZIO[Any, java.io.IOException, String] =
    script match
      case Left(message) => ZIO.fail(new java.io.IOException(message))
      case Right(file)   =>
        ZIO
          .attemptBlocking {
            val manifest = root.resolve("manifest.json")
            Files.writeString(manifest, text)
            val process = new ProcessBuilder(file.toString, "--manifest-kind", manifest.toString)
              .redirectErrorStream(true)
              .start()
            val out  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
            val code = process.waitFor()
            (code, out)
          }
          .mapError(error => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString)))
          .flatMap:
            case (0, out)    => ZIO.succeed(out)
            case (code, out) => ZIO.fail(new java.io.IOException(s"shim exited $code: $out"))

  private def script: Either[String, Path] =
    val start = Path.of("").toAbsolutePath.normalize
    walk(start) match
      case Some(root) => Right(root.resolve("install").resolve("histos"))
      case None       => Left(s"install/histos is not above $start")

  private def walk(current: Path): Option[Path] =
    if Files.isRegularFile(current.resolve("install").resolve("histos")) then Some(current)
    else
      Option(current.getParent) match
        case Some(parent) => walk(parent)
        case None         => None

  private val installVersion = "0.5.0-test-SNAPSHOT"

  private def coursierSha(text: String): Option[String] =
    text.linesIterator.collectFirst {
      case line if line.startsWith("coursier_sha=") => line.stripPrefix("coursier_sha=")
    }

  private def executable(path: Path, body: String): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Files.writeString(path, body)
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x"))
      ()
    }.orDie

  private def runInstall(
      shim: Path,
      bin: Path,
      home: Path,
      log: Path,
      sha: String,
  ): ZIO[Any, java.io.IOException, String] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder(shim.toString, "toolchain", "install", installVersion)
        builder.redirectErrorStream(true)
        val env = builder.environment()
        env.put("CI", "true")
        env.put("HISTOS_HOME", home.toString)
        env.put("MV_LOG", log.toString)
        env.put("EXPECTED_SHA", sha)
        val inherited = Option(env.get("PATH")).getOrElse("")
        env.put("PATH", bin.toString + java.io.File.pathSeparator + inherited)
        val process  = builder.start()
        val finished = process.waitFor(30, TimeUnit.SECONDS)
        if !finished then process.destroyForcibly()
        new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      }
      .mapError(error => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString)))

  private val curlStub =
    """|#!/bin/sh
       |prev=
       |out=
       |url=
       |for arg in "$@"; do
       |  if [ "$prev" = "-o" ]; then out=$arg; fi
       |  case $arg in
       |    http:*|https:*) url=$arg ;;
       |  esac
       |  prev=$arg
       |done
       |mkdir -p "$(dirname "$out")"
       |case $url in
       |  */channel-*)
       |    printf '%s\n' '0.5.0-test-SNAPSHOT' > "$out"
       |    ;;
       |  *-manifest.json)
       |    printf '%s\n' \
       |      '{' \
       |      '  "version": "0.5.0-test-SNAPSHOT",' \
       |      '  "group": "rocks.earlyeffect",' \
       |      '  "artifact": "histos",' \
       |      '  "revision": "0.5.0-test-SNAPSHOT"' \
       |      '}' > "$out"
       |    ;;
       |  *)
       |    printf 'x' > "$out"
       |    ;;
       |esac
       |""".stripMargin

  private val shaStub =
    """|#!/bin/sh
       |printf '%s  %s\n' "$EXPECTED_SHA" "$1"
       |""".stripMargin

  private val mvStub =
    """|#!/bin/sh
       |printf '%s\n%s\n' "$1" "$2" >> "$MV_LOG"
       |if [ "$1" = "$2" ]; then
       |  printf "mv: '%s' and '%s' are the same file\n" "$1" "$2" >&2
       |  exit 1
       |fi
       |/bin/mv "$1" "$2"
       |""".stripMargin

  private val coordinateManifest =
    """{
      |  "version": "0.5.0",
      |  "group": "rocks.earlyeffect",
      |  "artifact": "histos",
      |  "revision": "0.5.0"
      |}
      |""".stripMargin

  private val distManifest =
    """{
      |  "version": "0.4.4",
      |  "jar": "0.4.4-histos.jar",
      |  "jarSha256": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      |}
      |""".stripMargin
end ShimSpec
