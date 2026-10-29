package histos.testrun

import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildEvent, ChildRequest, Process, ProcessExit}
import histos.resolve.{BuildResolvers, DependencyLock, LockedTools, Resolve}
import histos.toolchain.BuildCompiler
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompiledCell, GraphCompile}
import histos.{Jvm, Module, Platform, ProjectGraph, Projects, ScalaVersion, TestStore, module}

import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.stream.ZStream
import zio.test.*

/** The test runner is the service. `Process` is traced. No compiler and no child.
  */
object TestLawSpec extends histos.SuiteClock:
  private val suiteName = SuiteName("keep.Suite")
  private val suite     = suiteName.value

  def spec =
    suite("test laws")(
      suite("runner")(
        recorded,
        separate,
        forgotten,
      ).provideSomeLayerShared[TestEnvironment & Scope](running) @@ TestAspect.withLiveSystem @@
        TestAspect.timeout(60.seconds),
      selected,
    )

  private val running =
    ZLayer.scoped:
      BuildCompiler.version.flatMap: version =>
        ZIO.service[LockedTools].provideSomeLayer[Scope](histos.ToolFixture.jvm(version))

  private def recorded =
    test("a quick record lives in the checkout and the next quick run does not call the process"):
      BuildCompiler.version.flatMap: version =>
        prepared(version).flatMap: ready =>
          checkout.flatMap: (repo, classes) =>
            coverOf(repo, classes, ready).flatMap: (_, digest) =>
              traced:
                for
                  exit   <- Ref.make(0)
                  _      <- spoken(exit)
                  first  <- run(repo, classes, ready, TestMode.Quick)
                  second <- run(repo, classes, ready, TestMode.Quick)
                  called <- calls
                  held   <- stored(repo, ready, digest)
                  path = record(repo, ready)
                yield assertTrue(
                  sameCheckout(path, repo),
                  Files.isRegularFile(path),
                  held,
                  first.toList == List(began(ready), TestFeed.Done(passed(ready))),
                  second.toList == List(cached(ready)),
                  called == List("events"),
                )

  private def separate =
    test("a second checkout of the same bytes is not a hit"):
      BuildCompiler.version.flatMap: version =>
        prepared(version).flatMap: ready =>
          checkout.flatMap: (leftRepo, leftClasses) =>
            checkout.flatMap: (rightRepo, rightClasses) =>
              coverOf(leftRepo, leftClasses, ready).flatMap: (leftCover, leftDigest) =>
                coverOf(rightRepo, rightClasses, ready).flatMap: (rightCover, rightDigest) =>
                  traced:
                    for
                      exit   <- Ref.make(0)
                      _      <- spoken(exit)
                      _      <- run(leftRepo, leftClasses, ready, TestMode.Quick)
                      _      <- run(rightRepo, rightClasses, ready, TestMode.Quick)
                      called <- calls
                      left  = record(leftRepo, ready)
                      right = record(rightRepo, ready)
                    yield assertTrue(
                      leftCover == rightCover,
                      leftDigest.exists(sha => rightDigest.contains(sha)),
                      called == List("events", "events"),
                      sameCheckout(left, leftRepo),
                      sameCheckout(right, rightRepo),
                      left != right,
                      Files.isRegularFile(left),
                      Files.isRegularFile(right),
                    )

  private def forgotten =
    test("a failure forgets the record and the next quick run calls the process"):
      BuildCompiler.version.flatMap: version =>
        prepared(version).flatMap: ready =>
          checkout.flatMap: (repo, classes) =>
            coverOf(repo, classes, ready).flatMap: (_, digest) =>
              traced:
                for
                  exit   <- Ref.make(0)
                  _      <- spoken(exit)
                  passed <- run(repo, classes, ready, TestMode.Quick)
                  kept   <- stored(repo, ready, digest)
                  _      <- exit.set(1)
                  failed <- run(repo, classes, ready, TestMode.Run)
                  missed <- stored(repo, ready, digest).map(!_)
                  path = record(repo, ready)
                  gone = !Files.isRegularFile(path)
                  _      <- exit.set(0)
                  again  <- run(repo, classes, ready, TestMode.Quick)
                  called <- calls
                yield assertTrue(
                  kept,
                  passed.toList == List(began(ready), TestFeed.Done(cell(ready, TestStatus.Passed))),
                  failed.toList == List(began(ready), TestFeed.Done(cell(ready, TestStatus.Failed(1)))),
                  missed,
                  gone,
                  again.toList == List(began(ready), TestFeed.Done(cell(ready, TestStatus.Passed))),
                  called == List("events", "events", "events"),
                )

  private def selected =
    test("a pattern keeps the named suite and matches nothing else"):
      val names  = List(suite, "drop.Suite")
      val kept   = SuitePattern.from("keep.*").map(pattern => SuiteLaunch.pick(names, List(pattern)))
      val other  = SuitePattern.from("drop.*").map(pattern => SuiteLaunch.pick(names, List(pattern)))
      val missed = SuitePattern.from("other.*").map(pattern => SuiteLaunch.pick(names, List(pattern)))
      assertTrue(
        kept == Right(SuiteLaunch.Picked.Launch(List(suite))),
        other == Right(SuiteLaunch.Picked.Launch(List("drop.Suite"))),
        missed == Right(SuiteLaunch.Picked.NoneMatched),
      )

  private def traced[E, A](effect: ZIO[Process & Stubbed[Process], E, A]) =
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
      mode: TestMode,
  ): ZIO[Process, ModuleTestError, Chunk[TestFeed]] =
    SuiteLaunch
      .remember(
        repo,
        compiled(ready, classes),
        List(suite),
        List(classes),
        ready.ask,
        mode,
        TestStore.Off,
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
  ): ZIO[Any, CoverGap | StampError, (SuiteStamp.Cover, Option[Sha256])] =
    ZIO
      .fromEither(SuiteCover.stored(repo, ready.ask))
      .flatMap: cover =>
        SuiteStamp.digest(suite, List(classes), cover).map(digest => (cover, digest))

  private def stored(repo: Path, ready: Ready, digest: Option[Sha256]) =
    digest match
      case Some(sha) =>
        QuickRecord.hit(repo, ready.module.name, ready.version, Platform.Jvm, suiteName, sha)
      case None => ZIO.succeed(false)

  private def prepared(version: ScalaVersion) =
    val defined = module("core", Jvm)(version)
    for
      jdk   <- GraphCompile.jdkHere
      graph <- ZIO.fromEither(Projects(defined))
      root  <- Workspace.work("histos-test-law-lock")
      _     <- Workspace.writeString(Resolve.lockPath(root), lockText(graph))
      _     <- histos.ToolFixture.seed(root, graph)
      lock  <- Resolve.readLock(root)
      core  <- one(graph)
    yield Ready(
      core,
      version,
      SuiteAsk(
        core,
        List(core),
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
      case core :: Nil => ZIO.succeed(core)
      case found       => ZIO.dieMessage(s"expected one module, saw ${found.size}")

  private def checkout: ZIO[Scope, IoFailure, (Path, Path)] =
    for
      repo    <- Workspace.work("histos-test-law")
      classes <- Workspace.work("histos-test-law-classes")
      _       <- plant(classes)
    yield (repo, classes)

  private def plant(classes: Path): ZIO[Any, IoFailure, Unit] =
    val path = classes.resolve("keep").resolve("Suite.class")
    ZIO
      .attemptBlocking:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, classBytes)
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
      MainClass.trusted(suite),
      Path.of("work"),
    )

  private def record(repo: Path, ready: Ready) =
    QuickRecord.file(repo, ready.module.name, ready.version, Platform.Jvm, suiteName)

  private def sameCheckout(path: Path, repo: Path) =
    path.normalize.startsWith(repo.resolve(".histos").resolve("test-quick").normalize)

  private def began(ready: Ready) =
    TestFeed.Begin(ready.module.name, ready.version, Platform.Jvm)

  private def cached(ready: Ready) =
    TestFeed.Cached(ready.module.name, ready.version, Platform.Jvm, suite)

  private def passed(ready: Ready) =
    cell(ready, TestStatus.Passed)

  private def cell(ready: Ready, status: TestStatus) =
    TestedCell(ready.module.name, ready.version, Platform.Jvm, status)

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def lockText(graph: ProjectGraph): String =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          histos.resolve.LockedCell(item.name, version, platform, Nil, Nil, Nil)
    DependencyLock.render(DependencyLock(cells))

  private final case class Ready(module: Module, version: ScalaVersion, ask: SuiteAsk)
end TestLawSpec
