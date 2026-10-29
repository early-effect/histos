package histos.testrun

import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.pack.MainClass
import histos.plugin.Need
import histos.process.{ChildClasspath, ChildEvent, ChildRequest, Process, ProcessExit}
import histos.resolve.{BuildResolvers, DependencyLock, LockedCell, LockedTools, Resolve}
import histos.toolchain.BuildCompiler
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompiledCell, GraphCompile}
import histos.{BuildError, Jvm, Module, Platform, ProjectGraph, ProjectName, Projects, ScalaVersion, TestStore, module}

import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Ref, Scope, ZIO}
import zio.stream.ZStream
import zio.test.{assertTrue, suite, test}

/** Test store. `Process` is a stub. No compiler and no child.
  */
object StoreLaw:
  private val app        = ProjectName("app")
  private val suiteName  = SuiteName("keep.Suite")
  private val suiteClass = suiteName.value

  def load =
    suite("load")(defaultStore, needed)

  def runner =
    suite("runner")(off, local, remote)

  private def defaultStore =
    test("Off is the default"):
      BuildCompiler.version.flatMap: version =>
        ZIO
          .fromEither(Projects(module("app", Jvm)(version)))
          .map: graph =>
            assertTrue(modulesOf(graph).map(_.testStore) == List(TestStore.Off))

  private def needed =
    test("Remote with a need does not load, and Local with a need does"):
      BuildCompiler.version.map: version =>
        val remote = Projects(
          module("app", Jvm)(version).testStore(TestStore.Remote).needs(Need.Network)
        )
        val local = Projects(
          module("app", Jvm)(version).testStore(TestStore.Local).needs(Need.Docker)
        )
        val refused = remote match
          case Left(BuildError.RemoteNeeds(name, needs, _)) => (name.value, needs)
          case Left(other)                                  => (other.message, Set.empty[Need])
          case Right(_)                                     => ("loaded", Set.empty[Need])
        val accepted = local match
          case Right(graph) => Right(modulesOf(graph).map(item => (item.testStore, item.needs)))
          case Left(error)  => Left(error.message)
        assertTrue(
          refused == ("app", Set(Need.Network)),
          accepted == Right(List((TestStore.Local, Set(Need.Docker)))),
        )

  private def off =
    test("test on Off launches again after a passing run"):
      twice(TestStore.Off).map: seen =>
        assertTrue(
          seen.ready.module.testStore == TestStore.Off,
          seen.first == launched(seen.ready),
          seen.second == launched(seen.ready),
          seen.called == List("events", "events"),
          seen.held,
        )

  private def local =
    test("test on Local skips when the stored suite digest still matches"):
      twice(TestStore.Local).map: seen =>
        assertTrue(
          seen.ready.module.testStore == TestStore.Local,
          seen.first == launched(seen.ready),
          seen.second == List(TestFeed.Cached(app, seen.ready.version, Platform.Jvm, suiteClass)),
          seen.called == List("events"),
          seen.held,
        )

  private def remote =
    test("test on Remote launches again and does not skip"):
      twice(TestStore.Remote).map: seen =>
        assertTrue(
          seen.ready.module.testStore == TestStore.Remote,
          seen.first == launched(seen.ready),
          seen.second == launched(seen.ready),
          seen.called == List("events", "events"),
          seen.held,
        )

  private def twice(store: TestStore) =
    BuildCompiler.version.flatMap: version =>
      prepared(version, store).flatMap: ready =>
        checkout.flatMap: (repo, classes) =>
          coverOf(repo, classes, ready).flatMap: digest =>
            traced:
              for
                exit   <- Ref.make(0)
                _      <- spoken(exit)
                first  <- run(repo, classes, ready)
                second <- run(repo, classes, ready)
                called <- calls
                held   <- stored(repo, ready, digest)
              yield Twice(first.toList, second.toList, called, held, ready)

  private def traced[A](effect: ZIO[Process & Stubbed[Process], Any, A]) =
    effect.provide(stubbedTraced[Process])

  private def spoken(exit: Ref[Int]) =
    stub[Process](_.events(any[ChildRequest]))(
      ZStream.fromZIO(exit.get.map(code => ChildEvent.Exited(ProcessExit(code))))
    )

  private def calls: ZIO[Stubbed[Process], Nothing, List[String]] =
    ZIO.serviceWith[Stubbed[Process]](_.calls.map(_.id.jvmName).toList)

  private def run(
      repo: Path,
      classes: Path,
      ready: Ready,
  ) =
    SuiteLaunch
      .remember(
        repo,
        compiled(ready, classes),
        List(suiteClass),
        List(classes),
        ready.ask,
        TestMode.Run,
        ready.module.testStore,
        _ =>
          SuiteLaunch.session(
            ready.module.name,
            ready.version,
            None,
            request(classes),
            Platform.Jvm,
          ),
      )
      .flatMap(_.runCollect)

  private def coverOf(
      repo: Path,
      classes: Path,
      ready: Ready,
  ): ZIO[Any, CoverGap | StampError, Option[Sha256]] =
    ZIO
      .fromEither(SuiteCover.stored(repo, ready.ask))
      .flatMap(cover => SuiteStamp.digest(suiteClass, List(classes), cover))

  private def stored(repo: Path, ready: Ready, digest: Option[Sha256]) =
    digest match
      case Some(sha) =>
        QuickRecord.hit(repo, ready.module.name, ready.version, Platform.Jvm, suiteName, sha)
      case None => ZIO.succeed(false)

  private def prepared(version: ScalaVersion, store: TestStore) =
    val defined =
      if store == TestStore.Off then module("app", Jvm)(version)
      else module("app", Jvm)(version).testStore(store)
    for
      jdk   <- GraphCompile.jdkHere
      graph <- ZIO.fromEither(Projects(defined))
      root  <- Workspace.work("histos-store-law")
      _     <- Workspace.writeString(Resolve.lockPath(root), lockText(graph))
      _     <- histos.ToolFixture.seed(root, graph)
      lock  <- Resolve.readLock(root)
      found <- one(graph)
    yield Ready(
      found,
      version,
      SuiteAsk(
        found,
        List(found),
        version,
        Platform.Jvm,
        lock,
        BuildResolvers(graph.resolvers),
        graph.tools,
        NativeOrigin.Central,
        jdk.identity,
        None,
      ),
    )

  private def one(graph: ProjectGraph): ZIO[Any, Nothing, Module] =
    modulesOf(graph) match
      case found :: Nil => ZIO.succeed(found)
      case _            => ZIO.dieMessage("store graph is one module")

  private def checkout: ZIO[Scope, IoFailure, (Path, Path)] =
    for
      repo    <- Workspace.work("histos-store-law")
      classes <- Workspace.work("histos-store-law-classes")
      _       <- plant(classes)
    yield (repo, classes)

  private def plant(classes: Path): ZIO[Any, IoFailure, Unit] =
    val path = classes.resolve("keep").resolve("Suite.class")
    val made = Option(path.getParent) match
      case Some(parent) => Workspace.createDirectories(parent)
      case None         => ZIO.dieMessage("suite class has no parent")
    made *> ZIO
      .attemptBlocking(Files.write(path, classBytes))
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  private def classBytes: Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "keep/Suite", null, "java/lang/Object", null)
    writer.visitEnd()
    writer.toByteArray

  private def compiled(ready: Ready, classes: Path) =
    CompiledCell(ready.module.name, ready.version, Platform.Jvm, classes, 1, None, false, None, None)

  private def request(classes: Path) =
    ChildRequest(
      Platform.Jvm,
      Path.of("java"),
      ChildClasspath(classes, Nil),
      MainClass.trusted(suiteClass),
      Path.of("work"),
    )

  private def launched(ready: Ready) =
    List(
      TestFeed.Begin(app, ready.version, Platform.Jvm),
      TestFeed.Done(TestedCell(app, ready.version, Platform.Jvm, TestStatus.Passed)),
    )

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def lockText(graph: ProjectGraph): String =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          LockedCell(item.name, version, platform, Nil, Nil, Nil)
    DependencyLock.render(DependencyLock(cells))

  private final case class Ready(module: Module, version: ScalaVersion, ask: SuiteAsk)

  private final case class Twice(
      first: List[TestFeed],
      second: List[TestFeed],
      called: List[String],
      held: Boolean,
      ready: Ready,
  )
end StoreLaw
