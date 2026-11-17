package histos.publish

import histos.jdk.Sha256
import histos.workspace.Workspace

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

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
