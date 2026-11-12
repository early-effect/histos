package histos.scaffold

import histos.central.{KeyServer, ReleaseFeed}
import histos.cli.Stdio
import histos.doctor.GhAuth
import histos.jdk.{Machine, Os}
import histos.load.Loader
import histos.native.NativeOrigin
import histos.resolve.Resolve
import histos.supply.{CommandRunner, GitRead, Net}
import histos.toolchain.Architecture
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import slayer.stubbed

import zio.{Scope, ZIO, ZLayer}
import zio.test.*

object ProvisionLawSpec extends ZIOSpecDefault:
  private val machine =
    Machine(
      Map.empty,
      "/tmp",
      Os.Mac,
      Architecture.Aarch64,
      Path.of("cache"),
      Path.of("repo"),
      None,
      NativeOrigin.Central,
    )

  private val bare =
    Provision.Request(ScaffoldKind.Bare, None, None, ZIO.dieMessage("branch"), Map.empty)

  def spec = suite("provision laws")(
    test("a created project's report names its directory and the next command"):
      Workspace
        .work("histos-provision-bare")
        .flatMap: parent =>
          Provision
            .create(parent, "demo", "0.1.0", Path.of("api.jar"), machine, bare)
            .map: made =>
              assertTrue(
                made.dir == parent.resolve("demo"),
                made.out.startsWith(made.dir.toAbsolutePath.normalize.toString),
                made.out.contains("histos test"),
              )
            .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("sbt init keeps an unmapped plugin, and a missing build file is that import"):
      Workspace
        .work("histos-provision-sbt")
        .flatMap: parent =>
          val demo    = parent.resolve("demo")
          val plugins = demo.resolve("project").resolve("plugins.sbt")
          val marker  = """addSbtPlugin("com.example" % "sbt-demo" % "1.0.0")"""
          val program =
            for
              _    <- write(demo.resolve("build.sbt"), "name := \"demo\"\n")
              _    <- write(plugins, marker + "\n")
              made <- Provision
                .init(demo, "0.1.0", Path.of("api.jar"), machine, bare.copy(source = Some(ImportSource.Sbt)))
              missing <- Provision
                .init(
                  parent.resolve("empty"),
                  "0.1.0",
                  Path.of("api.jar"),
                  machine,
                  bare.copy(source = Some(ImportSource.Sbt)),
                )
                .either
              text <- Workspace.readString(made.dir.resolve("histos.scala"))
            yield assertTrue(
              text.contains(s"// unmapped: $marker"),
              missing == Left(ProvisionError.Import("no build.sbt")),
            )
          program.provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("zipx init reads a module directory, and a missing versions file is that import"):
      Workspace
        .work("histos-provision-zipx")
        .flatMap: parent =>
          val demo    = parent.resolve("demo")
          val spaced  = parent.resolve("not a name")
          val sources = demo.resolve("core").resolve("src")
          val program =
            for
              _    <- write(demo.resolve("project").resolve("ZipxVersions.scala"), "object ZipxVersions\n")
              _    <- write(sources.resolve(".keep"), "")
              made <- Provision.init(
                demo,
                "0.1.0",
                Path.of("api.jar"),
                machine,
                bare.copy(source = Some(ImportSource.Zipx)),
              )
              _   <- write(spaced.resolve("project").resolve("ZipxVersions.scala"), "object ZipxVersions\n")
              _   <- write(spaced.resolve("src").resolve(".keep"), "")
              bad <- Provision
                .init(spaced, "0.1.0", Path.of("api.jar"), machine, bare.copy(source = Some(ImportSource.Zipx)))
                .either
              gone <- Provision
                .init(
                  parent.resolve("plain"),
                  "0.1.0",
                  Path.of("api.jar"),
                  machine,
                  bare.copy(source = Some(ImportSource.Zipx)),
                )
                .either
              text <- Workspace.readString(made.dir.resolve("histos.scala"))
            yield assertTrue(
              text.contains("core"),
              bad == Left(ProvisionError.Files(SkeletonError.Name("not a name"))),
              gone == Left(ProvisionError.Import("no project/ZipxVersions.scala")),
            )
          program.provideSomeLayer[TestEnvironment & Scope](stock),
  )

  /** Every service dies if read. A bare project loads nothing, resolves nothing, and runs no doctor. */
  private val stock =
    stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ReleaseFeed] ++ stubbed[GitRead] ++ stubbed[Net] ++
      stubbed[GhAuth] ++ stubbed[KeyServer] ++ stubbed[CommandRunner] ++ ZLayer.succeed(Stdio.redirected)

  private def write(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text)
        ()
      }
      .catchAll(error => ZIO.dieMessage(error.getClass.getName))
end ProvisionLawSpec
