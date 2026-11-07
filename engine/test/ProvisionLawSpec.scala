package histos.scaffold

import histos.jdk.{Machine, Os}
import histos.load.Loader
import histos.native.NativeOrigin
import histos.resolve.Resolve
import histos.toolchain.Architecture
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import slayer.stubbed

import zio.{Scope, ZIO}
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
    test("a bare project is the build and the sources, and a bad name is refused"):
      Workspace
        .work("histos-provision-bare")
        .flatMap: parent =>
          val program =
            for
              made  <- Provision.create(parent, "demo", "0.1.0", Path.of("api.jar"), machine, bare)
              named <- Provision.create(parent, "not a name", "0.1.0", Path.of("api.jar"), machine, bare).either
            yield
              val dir    = made.dir
              val props  = Files.readString(dir.resolve("build.properties"))
              val build  = Files.readString(dir.resolve("histos.scala"))
              val source = Files.isRegularFile(dir.resolve("app/src/main/scala/demo/Main.scala"))
              val ignore = Files.isRegularFile(dir.resolve(".gitignore"))
              assertTrue(
                props == "histos.version=0.1.0\n",
                build.contains("module"),
                source,
                !ignore,
                made.out.startsWith(dir.toAbsolutePath.normalize.toString),
                made.out.contains("histos test"),
                named == Left(ProvisionError.Files(SkeletonError.Name("not a name"))),
              )
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader] ++ stubbed[Resolve])
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
              _       <- write(demo.resolve("build.sbt"), "name := \"demo\"\n")
              _       <- write(plugins, marker + "\n")
              made    <- Provision.init(demo, "0.1.0", Path.of("api.jar"), machine, bare.copy(source = Some(ImportSource.Sbt)))
              missing <- Provision
                .init(parent.resolve("empty"), "0.1.0", Path.of("api.jar"), machine, bare.copy(source = Some(ImportSource.Sbt)))
                .either
            yield
              val text = Files.readString(made.dir.resolve("histos.scala"))
              assertTrue(
                text.contains(s"// unmapped: $marker"),
                missing == Left(ProvisionError.Import("no build.sbt")),
              )
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader] ++ stubbed[Resolve])
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
              _ <- write(demo.resolve("project").resolve("ZipxVersions.scala"), "object ZipxVersions\n")
              _ <- write(sources.resolve(".keep"), "")
              made <- Provision.init(
                demo,
                "0.1.0",
                Path.of("api.jar"),
                machine,
                bare.copy(source = Some(ImportSource.Zipx)),
              )
              _ <- write(spaced.resolve("project").resolve("ZipxVersions.scala"), "object ZipxVersions\n")
              _ <- write(spaced.resolve("src").resolve(".keep"), "")
              bad <- Provision
                .init(spaced, "0.1.0", Path.of("api.jar"), machine, bare.copy(source = Some(ImportSource.Zipx)))
                .either
              gone <- Provision
                .init(parent.resolve("plain"), "0.1.0", Path.of("api.jar"), machine, bare.copy(source = Some(ImportSource.Zipx)))
                .either
            yield
              val text = Files.readString(made.dir.resolve("histos.scala"))
              assertTrue(
                text.contains("core"),
                bad == Left(ProvisionError.Files(SkeletonError.Name("not a name"))),
                gone == Left(ProvisionError.Import("no project/ZipxVersions.scala")),
              )
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[Loader] ++ stubbed[Resolve])
    ,
  )

  private def write(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text)
        ()
      }
      .catchAll(error => ZIO.dieMessage(error.getClass.getName))
