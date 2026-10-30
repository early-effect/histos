package histos.zinc

import histos.cache.ContentStore
import histos.native.NativeOrigin
import histos.resolve.{DependencyLock, LockedCell, LockedTools, Resolve, ResolveError}
import histos.testrun.{ModuleTest, ModuleTestError, StampLaw, StoreLaw, TestCompile, TestMode, TestRun}
import histos.toolchain.BuildCompiler
import histos.workspace.Workspace
import histos.TestMachine
import histos.{
  Jvm,
  Module,
  Platform,
  ProjectGraph,
  ProjectName,
  Projects,
  ScalaVersion,
  TestMain,
  TestProvider,
  module,
  path,
}

import java.nio.file.{Files, Path}

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{NonEmptyChunk, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

/** Layout, refusals, the test store, the suite stamp, scheduling, and the classpath. The compiler is a stub. The
  * runner's process is a stub. A call on a refusal is a failed law.
  */
object BoundaryLawSpec extends histos.SuiteClock:
  private val app    = ProjectName("app")
  private val source = "object App\n"

  def spec =
    suite("zinc boundaries")(
      suite("layout")(
        rooted,
        named,
        relocated,
        unlocked,
        unrooted,
        empty,
        vacant,
        foreign,
        unprovided,
        unnamed,
      ).provideSomeLayerShared[TestEnvironment & Scope](running),
      suite("store")(
        StoreLaw.load,
        StoreLaw.runner.provideSomeLayerShared[TestEnvironment & Scope](running),
      ),
      suite("stamp")(
        StampLaw.laws,
        StampLaw.gaps.provideSomeLayerShared[TestEnvironment & Scope](running),
      ),
      suite("schedule")(
        ScheduleLaw.laws.provideSomeLayerShared[TestEnvironment & Scope](running)
      ),
      suite("classpath")(
        ClasspathLaw.paths.provideSomeLayerShared[TestEnvironment & Scope](running),
        ClasspathLaw.key,
      ),
      suiteFiles,
      repoTest,
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(60.seconds)

  private val running =
    ZLayer.scoped:
      BuildCompiler.version.flatMap: version =>
        ZIO.service[LockedTools].provideSomeLayer[Scope](histos.ToolFixture.jvm(version))

  private val resolveLayer =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve))

  private def rooted =
    test("one module takes sources at the repository root"):
      BuildCompiler.version.flatMap: version =>
        open(
          Projects(module("app", Jvm)(version)),
          List("src/main/scala/App.scala" -> source),
          lock = true,
          tools = true,
        )
          .flatMap: (jdk, root, graph) =>
            traced:
              drive(jdk, root, graph).map: (result, names) =>
                val detail = result match
                  case Left(error) => error.message
                  case Right(_)    => ""
                assertTrue(names == List("App.scala"), detail.isEmpty)

  private def named =
    test("a module directory wins over the shared root"):
      BuildCompiler.version.flatMap: version =>
        open(
          Projects(module("app", Jvm)(version)),
          List(
            "src/main/scala/Root.scala"    -> "object Root\n",
            "app/src/main/scala/App.scala" -> source,
          ),
          lock = true,
          tools = true,
        ).flatMap: (jdk, root, graph) =>
          traced:
            drive(jdk, root, graph).map: (result, names) =>
              val detail = result match
                case Left(error) => error.message
                case Right(_)    => ""
              assertTrue(names == List("App.scala"), detail.isEmpty)

  private def relocated =
    test("sources may live in a directory that is not the module name"):
      BuildCompiler.version.flatMap: version =>
        open(
          Projects(module("app", Jvm)(version).in(path"elsewhere")),
          List(
            "src/main/scala/Root.scala"                -> "object Root\n",
            "app/src/main/scala/App.scala"             -> source,
            "elsewhere/src/main/scala/Elsewhere.scala" -> "object Elsewhere\n",
          ),
          lock = true,
          tools = true,
        ).flatMap: (jdk, root, graph) =>
          traced:
            drive(jdk, root, graph).map: (result, names) =>
              val detail = result match
                case Left(error) => error.message
                case Right(_)    => ""
              assertTrue(names == List("Elsewhere.scala"), detail.isEmpty)

  private def unlocked =
    test("a missing lock is refused"):
      BuildCompiler.version.flatMap: version =>
        open(Projects(module("app", Jvm)(version)), Nil, lock = false, tools = false).flatMap: (jdk, root, graph) =>
          traced:
            drive(jdk, root, graph).map: (result, names) =>
              assertTrue(
                names.isEmpty,
                result == Left(GraphCompileError.Lock(ResolveError.NoLock(Resolve.lockPath(root)))),
              )

  private def unrooted =
    test("a module with no source root is refused"):
      refused(version => Projects(module("app", Jvm)(version)), Nil, create = false)((_, _) =>
        GraphCompileError.Cells(NonEmptyChunk(GraphCompileError.NoRoot(app)))
      )

  private def empty =
    test("an empty source root is refused"):
      refused(version => Projects(module("app", Jvm)(version)), Nil, create = true)((_, root) =>
        GraphCompileError.Cells(
          NonEmptyChunk(GraphCompileError.NoSources(app, root.resolve("app").resolve("src")))
        )
      )

  private def vacant =
    test("tests alone compile an empty main"):
      BuildCompiler.version.flatMap: version =>
        open(
          Projects(module("app", Jvm)(version)),
          List("app/src/test/scala/Check.scala" -> "object Check\n"),
          lock = true,
          tools = false,
        ).flatMap: (jdk, root, graph) =>
          traced:
            drive(jdk, root, graph).flatMap: (result, names) =>
              val classes = GraphCompile.classesDir(root, app, version)
              ZIO
                .attemptBlocking(Files.isDirectory(classes))
                .map: there =>
                  val detail = result match
                    case Left(error) => error.message
                    case Right(_)    => ""
                  val emptyMain = result match
                    case Right(cell :: Nil) =>
                      cell.classFiles == 0 && cell.classesWritten.contains(0) && cell.key.isEmpty
                    case _ => false
                  assertTrue(names.isEmpty, detail.isEmpty, there, emptyMain)

  private def foreign =
    test("another platform's tests are not an empty main"):
      refused(
        version => Projects(module("app", Jvm)(version)),
        List("app/src/test/js/scala/Check.scala" -> "object Check\n"),
        create = false,
      )((_, root) =>
        GraphCompileError.Cells(
          NonEmptyChunk(GraphCompileError.NoSources(app, root.resolve("app").resolve("src")))
        )
      )

  private def unprovided =
    test("test sources without a provider library are refused"):
      BuildCompiler.version.flatMap: version =>
        val built = Projects(module("app", Jvm)(version))
        open(built, List("app/src/test/scala/Check.scala" -> "object Check\n"), lock = true, tools = false).flatMap:
          (jdk, root, graph) =>
            one(graph).flatMap: item =>
              val cell = compiled(root, item.name, version)
              for
                found <- TestCompile.testSources(root, item, List(item), version, Platform.Jvm)
                here  <- TestMachine.runtimes(root)
                ran   <- ModuleTest
                  .suites(TestRun(root, graph, Nil, jdk, here, TestMode.Run), List(cell))
                  .runCollect
                  .either
                  .provideSomeLayer[LockedTools](resolveLayer)
              yield assertTrue(
                found.map(fileName) == List("Check.scala"),
                ran == Left(ModuleTestError.ProviderDependency(app, TestProvider.Zio)),
              )

  private def unnamed =
    test("a named main without test sources is refused"):
      BuildCompiler.version.flatMap: version =>
        val built = Projects(module("app", Jvm)(version).test(TestMain("demo.Check")))
        open(built, Nil, lock = true, tools = false).flatMap: (jdk, root, graph) =>
          one(graph).flatMap: item =>
            val cell = compiled(root, item.name, version)
            for
              found <- TestCompile.testSources(root, item, List(item), version, Platform.Jvm)
              here  <- TestMachine.runtimes(root)
              ran   <- ModuleTest
                .suites(TestRun(root, graph, Nil, jdk, here, TestMode.Run), List(cell))
                .runCollect
                .either
                .provideSomeLayer[LockedTools](resolveLayer)
            yield assertTrue(
              found.isEmpty,
              ran == Left(ModuleTestError.MissingSources(app, TestMain("demo.Check"))),
            )

  private def suiteFiles =
    test("src/test is the suite when the module has no main"):
      BuildCompiler.version.flatMap: version =>
        checkout(List("app/src/test/scala/Check.scala" -> "object Check\n")).flatMap: root =>
          ZIO
            .fromEither(Projects(module("app", Jvm)(version)))
            .flatMap: graph =>
              one(graph).flatMap: item =>
                val base = root.resolve("app").resolve("src")
                for
                  found <- TestCompile.testSources(root, item, List(item), version, Platform.Jvm)
                  main  <- Sources.select(base, version, Platform.Jvm)
                  tests <- Sources.tests(base, version, Platform.Jvm)
                yield assertTrue(
                  main.isEmpty,
                  tests.map(fileName) == List("Check.scala"),
                  found.map(fileName) == List("Check.scala"),
                )

  private def repoTest =
    test("a single module uses test at the repository root"):
      BuildCompiler.version.flatMap: version =>
        val appDef   = module("app", Jvm)(version)
        val otherDef = module("other", Jvm)(version)
        checkout(List("test/scala/Check.scala" -> "object Check\n")).flatMap: root =>
          for
            alone         <- ZIO.fromEither(Projects(appDef))
            both          <- ZIO.fromEither(Projects(appDef, otherDef))
            oneApp        <- one(alone)
            twoApp        <- namedModule(both, app)
            aloneSources  <- TestCompile.testSources(root, oneApp, List(oneApp), version, Platform.Jvm)
            sharedSources <- TestCompile.testSources(root, twoApp, modulesOf(both), version, Platform.Jvm)
          yield assertTrue(
            aloneSources.map(fileName) == List("Check.scala"),
            sharedSources.isEmpty,
          )

  private def refused(
      built: ScalaVersion => Either[histos.BuildError, ProjectGraph],
      files: List[(String, String)],
      create: Boolean,
  )(
      expected: (ScalaVersion, Path) => GraphCompileError
  ) =
    BuildCompiler.version.flatMap: version =>
      open(built(version), files, lock = true, tools = false).flatMap: (jdk, root, graph) =>
        val made =
          if create then Workspace.createDirectories(root.resolve("app").resolve("src"))
          else ZIO.unit
        made *> traced:
          drive(jdk, root, graph).map: (result, names) =>
            assertTrue(names.isEmpty, result == Left(expected(version, root)))

  private def open(
      built: Either[histos.BuildError, ProjectGraph],
      files: List[(String, String)],
      lock: Boolean,
      tools: Boolean,
  ) =
    for
      jdk   <- GraphCompile.jdkHere
      root  <- Workspace.work("histos-boundary")
      graph <- ZIO.fromEither(built)
      _     <- ZIO.foreachDiscard(files): (name, body) =>
        Workspace.writeString(root.resolve(name), body)
      _ <-
        if lock then Workspace.writeString(Resolve.lockPath(root), lockText(graph))
        else ZIO.unit
      _ <-
        if tools then histos.ToolFixture.seed(root, graph)
        else ZIO.unit
    yield (jdk, root, graph)

  private def checkout(files: List[(String, String)]) =
    for
      root <- Workspace.work("histos-boundary")
      _    <- ZIO.foreachDiscard(files): (name, body) =>
        Workspace.writeString(root.resolve(name), body)
    yield root

  private def drive(jdk: histos.jdk.JdkRuntime, root: Path, graph: ProjectGraph) =
    for
      _      <- silence
      result <- GraphCompile.runCounted(root, graph, Nil, jdk, NativeOrigin.Central, None).either
      names  <- asked
    yield (result, names)

  private def silence =
    stub[Compiler](_.compile(any[CompileCall]))((call: CompileCall) =>
      ZIO.succeed(
        CompileOutput(
          call.request.classes,
          0,
          call.key,
          changed = true,
          sources = Some(SourceCount(call.request.sources.size, 0)),
          classesWritten = Some(0),
          miss = None,
        )
      )
    )

  private def traced[A](
      use: ZIO[LockedTools & Stubbed[Compiler] & Resolve & ContentStore & Compiler, Any, A]
  ) =
    Workspace
      .work("histos-boundary-store")
      .flatMap: store =>
        use.provideSomeLayer[LockedTools](
          stubbedTraced[Compiler] ++ ZLayer.succeed(ContentStore.in(store)) ++ resolveLayer
        )

  private def asked: ZIO[Stubbed[Compiler], Nothing, List[String]] =
    ZIO.serviceWith[Stubbed[Compiler]](_.calls.flatMap(fileOf).toList)

  private def fileOf(call: slayer.Call): List[String] =
    call.args match
      case (found: CompileCall) :: _ => found.request.sources.map(fileName)
      case _                         => Nil

  private def fileName(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("source")

  private def compiled(root: Path, name: ProjectName, version: ScalaVersion) =
    CompiledCell(
      name,
      version,
      Platform.Jvm,
      GraphCompile.classesDir(root, name, version),
      0,
      None,
      changed = false,
      sources = None,
      classesWritten = None,
    )

  private def one(graph: ProjectGraph): ZIO[Any, Nothing, Module] =
    modulesOf(graph) match
      case item :: Nil => ZIO.succeed(item)
      case _           => ZIO.dieMessage("boundary graph is one module")

  private def namedModule(graph: ProjectGraph, name: ProjectName): ZIO[Any, Nothing, Module] =
    modulesOf(graph).filter(_.name == name) match
      case item :: Nil => ZIO.succeed(item)
      case _           => ZIO.dieMessage("boundary graph is missing its module")

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def lockText(graph: ProjectGraph): String =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          LockedCell(item.name, version, platform, Nil, Nil, Nil)
    DependencyLock.render(DependencyLock(cells))
end BoundaryLawSpec
