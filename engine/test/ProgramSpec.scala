package histos.program

import histos.resolve.{
  Resolve,
  ToolRequest,
  ResolveCell,
  Classpath,
  LockedArtifact,
  ResolvedArtifact,
  Repositories,
  Repository,
  DependencyLock,
  LockedCell,
  LockedDirect
}
import histos.zinc.GraphCompile
import histos.{
  ClangMajor,
  Js,
  Jvm,
  MainClass,
  Native,
  Projects,
  Requires,
  ScalaVersion,
  TestMachine,
  Tool,
  module
}
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Promise, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object ProgramSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  private val ran = suite("entry")(
    test(
      "an in-flight run keeps one lock generation across compile and launch"
    ):
      ZIO.scoped:
        for
          root <- temp
          jdk <- GraphCompile.jdkHere
          here <- TestMachine.runtimes(root)
          name <- ZIO.fromEither(histos.ProjectName.from("app"))
          dependency = histos.Dep("dev.fixture:gate:1.0.0")
          beforeGraph <- ZIO.fromEither(
            Projects(
              module("app", Jvm)(scala3)
                .libraryDependencies(dependency)
                .main(MainClass("demo.Check"))
            )
          )
          afterVersion = ScalaVersion("3.9.0")
          afterGraph <- ZIO.fromEither(
            Projects(
              module("app", Jvm)(afterVersion)
                .libraryDependencies(dependency)
                .main(MainClass("demo.Check"))
            )
          )
          _ <- ZIO.attempt(
            write(root, "app/src/main/scala/demo/Check.scala", hi)
          )
          jar = root.resolve("gate.jar")
          _ <- ZIO.attemptBlocking {
            val stream =
              new java.util.jar.JarOutputStream(Files.newOutputStream(jar))
            stream.close()
          }
          hash <- histos.toolchain.RuntimeFiles.sha256(jar)
          artifact = LockedArtifact(
            "dev.fixture",
            "gate",
            "1.0.0",
            hash,
            Repository.Central
          )
          tools <- ZIO.service[histos.resolve.LockedTools]
          before = DependencyLock(
            List(
              LockedCell(
                name,
                scala3,
                Jvm,
                List(
                  LockedDirect(
                    "dev.fixture",
                    "gate",
                    "1.0.0",
                    histos.DepKind.Java
                  )
                ),
                List(artifact),
                Nil
              )
            ),
            Nil,
            tools.lock.tools.filter(cell =>
              cell.id.role == histos.resolve.ToolRole.Compiler && cell.id.scalaVersion == scala3
            )
          )
          after = before.copy(
            cells = before.cells.map(_.copy(scalaVersion = afterVersion)),
            tools = tools.lock.tools.filter(cell =>
              cell.id.role == histos.resolve.ToolRole.Compiler && cell.id.scalaVersion == afterVersion
            )
          )
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(before)
          )
          reached <- Promise.make[Nothing, Unit]
          proceed <- Promise.make[Nothing, Unit]
          resolver = new Resolve:
            def resolveTool(request: ToolRequest) =
              tools.resolve.resolveTool(request)
            def resolve(cell: ResolveCell) = tools.resolve.resolve(cell)
            def fetch(held: LockedArtifact) =
              if held.organization == "dev.fixture" && held.name == "gate" then
                reached.succeed(()).unit *> proceed.await *>
                  ZIO.succeed(
                    ResolvedArtifact(
                      held.organization,
                      held.name,
                      held.revision,
                      jar,
                      hash,
                      held.repository
                    )
                  )
              else tools.resolve.fetch(held)
            def resolvePlugin(
                dependency: histos.Dep,
                repositories: Repositories
            ) = tools.resolve.resolvePlugin(dependency, repositories)
            def sources(
                organization: String,
                name: String,
                revision: String,
                repository: Repository
            ) = tools.resolve.sources(organization, name, revision, repository)
          running <- Program
            .console(root, beforeGraph, "app", jdk, here)
            .runCollect
            .provide(ZLayer.succeed(resolver))
            .forkScoped
          _ <- reached.await
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(after)
          )
          _ <- proceed.succeed(())
          inFlight <- running.join
          next <- Program
            .console(root, afterGraph, "app", jdk, here)
            .runCollect
            .provide(ZLayer.succeed(resolver))
        yield assertTrue(
          inFlight.collect { case ProgramFeed.Exited(_, version, _, code) =>
            (version, code)
          } == List((scala3, 0)),
          next.collect { case ProgramFeed.Exited(_, version, _, code) =>
            (version, code)
          } == List((afterVersion, 0)),
          inFlight
            .collect { case ProgramFeed.Out(text) => text }
            .contains("hi"),
          next.collect { case ProgramFeed.Out(text) => text }.contains("hi")
        )
    ,
    test("a module without a main is not linked"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          here <- TestMachine.runtimes(root)
          out <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "core", jdk, here)
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
        yield assertTrue:
          out match
            case Left(ProgramError.NoMain(name)) => name.value == "core"
            case _                               => false
    ,
    test("a jvm main runs on the main classpath"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/main/scala/demo/Check.scala", hi)
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          here <- TestMachine.runtimes(root)
          feeds <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "app", jdk, here)
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          mainEntries <- ZIO.attempt(
            entries(root.resolve(".histos/classes/app/3.10.0/jvm"))
          )
        yield assertTrue(
          feeds
            .collect { case ProgramFeed.Out(text) => text }
            .mkString("\n")
            .contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) =>
            (platform, code)
          } == List((Jvm, 0)),
          mainEntries.exists(_.endsWith("Check.class"))
        )
    ,
    test("a js main links and starts in node"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Js)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/main/scala/demo/Check.scala", hi)
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 js\n")
          }
          chosen <- TestMachine.chosenNode(root)
          here <- TestMachine.runtimes(root)
          feeds <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "app", jdk, here)
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          ran <- TestMachine.launches(chosen)
        yield assertTrue(
          feeds
            .collect { case ProgramFeed.Out(text) => text }
            .mkString("\n")
            .contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) =>
            (platform, code)
          } == List((Js, 0)),
          ran.nonEmpty
        )
    @@ Requires(Tool.Node),
    test("a multi-platform module does not start until a platform is named"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm, Js)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          here <- TestMachine.runtimes(root)
          out <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "app", jdk, here)
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
          classes <- ZIO.attempt(
            Files.exists(root.resolve(".histos").resolve("classes"))
          )
        yield assertTrue:
          out match
            case Left(error @ ProgramError.Choose(name, platforms)) =>
              name.value == "app" &&
              platforms == List(Jvm, Js) &&
              !classes &&
              error.message.contains("histos run app jvm")
            case _ => false
    ,
    test("a platform the module does not build is refused"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          here <- TestMachine.runtimes(root)
          out <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "app", jdk, here, Some(Native))
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
          classes <- ZIO.attempt(
            Files.exists(root.resolve(".histos").resolve("classes"))
          )
        yield assertTrue:
          out match
            case Left(ProgramError.Unbuilt(name, platform, builtOn)) =>
              name.value == "app" && platform == Native && builtOn == List(
                Jvm
              ) && !classes
            case _ => false
    ,
    test("an explicit platform runs only that platform"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm, Js)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/main/scala/demo/Check.scala", hi)
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          here <- TestMachine.runtimes(root)
          feeds <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "app", jdk, here, Some(Jvm))
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          jsClasses <- ZIO.attempt(
            Files.exists(root.resolve(".histos/classes/app/3.10.0/js"))
          )
        yield assertTrue(
          feeds
            .collect { case ProgramFeed.Out(text) => text }
            .mkString("\n")
            .contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) =>
            (platform, code)
          } == List((Jvm, 0)),
          !jsClasses
        )
    ,
    test("a native main links and runs the binary"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Native)(ScalaVersion("3.9.0"))
            .clang(ClangMajor(21))
            .main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/main/scala/demo/Check.scala", hi)
            write(root, ".histos/dependencies.lock", "cell app 3.9.0 native\n")
          }
          here <- TestMachine.runtimes(root)
          feeds <- histos.ToolFixture
            .streamAt(root, built)(
              Program
                .console(root, built, "app", jdk, here)
            )
            .runCollect
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue(
          feeds
            .collect { case ProgramFeed.Out(text) => text }
            .mkString("\n")
            .contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) =>
            (platform, code)
          } == List((Native, 0))
        )
    @@ Requires(Tool.Clang(ClangMajor(21)))
  )

  private val hi =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("hi")
      |""".stripMargin

  private def entries(classes: Path): List[String] =
    if !Files.isDirectory(classes) then Nil
    else
      val walk = Files.walk(classes)
      try
        walk.iterator.asScala
          .filter(Files.isRegularFile(_))
          .map(path => classes.relativize(path).toString.replace('\\', '/'))
          .toList
      finally walk.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace
      .work("histos-program")
      .mapError(error => new java.io.IOException(error.message))

  def spec = ran.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
    histos.ToolFixture.layer(
      histos.Projects(
        histos.module("jvm", histos.Jvm)(histos.ScalaVersion("3.10.0")),
        histos.module("js", histos.Js)(histos.ScalaVersion("3.10.0")),
        histos
          .module("native", histos.Native)(histos.ScalaVersion("3.9.0"))
          .clang(histos.ClangMajor(21))
      )
    )
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(12.minutes)
