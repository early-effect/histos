package histos.publish

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.test.{TestAspect, ZIOSpecDefault, assertTrue}
import zio.{Scope, ZIO}

object DistSpec extends ZIOSpecDefault:
  def spec = suite("dist")(
    test("a bundle names the jar, the two clients, and the source archive"):
      val jar     = "jar".getBytes(StandardCharsets.UTF_8)
      val linux   = "linux".getBytes(StandardCharsets.UTF_8)
      val mac     = "mac".getBytes(StandardCharsets.UTF_8)
      val sources = "sources".getBytes(StandardCharsets.UTF_8)
      val files   = Dist.bundle(
        "0.0.2-20260101.120000-1",
        jar,
        List("linux-x86_64" -> linux, "macos-aarch64" -> mac),
        sources,
      ).toMap
      val manifest = new String(
        files(DistLayout.manifest("0.0.2-20260101.120000-1")),
        StandardCharsets.UTF_8,
      )
      assertTrue(
        files.keySet == Set(
          DistLayout.manifest("0.0.2-20260101.120000-1"),
          DistLayout.jar("0.0.2-20260101.120000-1"),
          DistLayout.client("0.0.2-20260101.120000-1", "linux-x86_64"),
          DistLayout.client("0.0.2-20260101.120000-1", "macos-aarch64"),
          DistLayout.sources("0.0.2-20260101.120000-1"),
        ),
        manifest.contains(""""linux-x86_64""""),
        manifest.contains(""""macos-aarch64""""),
        manifest.contains("sourcesSha256"),
        Dist.clientPlatforms == List("linux-x86_64", "macos-aarch64"),
      )
    ,
    test("client sources unpack to api, wire, client, and the build file"):
      ZIO.scoped:
        for
          root <- histos.workspace.Workspace
            .work("histos-dist")
            .mapError(error => new java.io.IOException(error.message))
          _ <- ZIO.attempt {
            Files.createDirectories(root.resolve("api/src"))
            Files.writeString(root.resolve("api/src/Api.scala"), "package api\n")
            Files.createDirectories(root.resolve("wire/src"))
            Files.writeString(root.resolve("wire/src/Wire.scala"), "package wire\n")
            Files.createDirectories(root.resolve("client/src"))
            Files.writeString(root.resolve("client/src/Main.scala"), "package client\n")
            Files.write(root.resolve("sources.tar"), Dist.sources(root))
          }
          names <- ZIO.attempt {
            val process = new ProcessBuilder("tar", "-tf", root.resolve("sources.tar").toString).start()
            val text    = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
            process.waitFor() -> text
          }.flatMap {
            case (0, text) => ZIO.succeed(text)
            case (code, _) => ZIO.fail(new java.io.IOException(s"tar exited $code"))
          }
        yield assertTrue(
          names.contains("histos.scala"),
          names.contains("api/src/Api.scala"),
          names.contains("wire/src/Wire.scala"),
          names.contains("client/src/Main.scala"),
          Dist.buildFile.contains("module(\"client\", Native)"),
        )
    ,
  ) @@ TestAspect.withLiveSystem
end DistSpec
