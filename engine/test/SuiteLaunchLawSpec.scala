package histos.testrun

import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.node.{NodeMiss, NodeSource, NodeToolchain}
import histos.resolve.{
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  LockedTool,
  Repositories,
  ResolveError,
  ToolRequest,
}
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{ApiDigest, CompiledCell, CompilerRequests, GraphCompile}
import histos.{
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  TestStore,
  ToolCatalog,
}

import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{Chunk, Ref, ZIO}
import zio.stream.ZStream
import zio.test.*

object SuiteLaunchLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion.trusted("3.10.0")
  private val repos   = Repositories.central
  private val toolSha = Sha256.trusted("11")
  private val compile = CompilerRequests.compiler(scala3, repos)
  private val jdk     = RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab"))
  private val names   = List("demo.C", "demo.A", "demo.B", "demo.Missing")

  def spec = suite("suite launch decisions")(
    test("quick mode and a local store skip a stored digest and run the rest newest first"):
      Workspace
        .work("histos-launch-quick")
        .flatMap: repo =>
          staged(repo).flatMap: ready =>
            for
              seen  <- Ref.make(List.empty[String])
              quick <- launch(repo, ready, TestMode.Quick, TestStore.Off, seen)
              local <- launch(repo, ready, TestMode.Run, TestStore.Local, seen)
            yield assertTrue(quick == decided, local == decided)
    ,
    test("run on an off store or a remote store launches every suite"):
      Workspace
        .work("histos-launch-run")
        .flatMap: repo =>
          staged(repo).flatMap: ready =>
            for
              seen   <- Ref.make(List.empty[String])
              off    <- launch(repo, ready, TestMode.Run, TestStore.Off, seen)
              remote <- launch(repo, ready, TestMode.Run, TestStore.Remote, seen)
            yield assertTrue(
              off == Launched(names, Chunk.empty),
              remote == Launched(names, Chunk.empty),
            )
    ,
    test("a cover gap refuses before a suite is launched"):
      Workspace
        .work("histos-launch-gap")
        .flatMap: repo =>
          val name    = ProjectName.trusted("app")
          val core    = ProjectName.trusted("core")
          val blocked = dieLaunch
          for
            node <- SuiteLaunch
              .remember(
                repo,
                compiled(name, Platform.Js, repo.resolve("js")),
                names,
                Nil,
                ask(bare(name), List(bare(name)), Platform.Js, DependencyLock(Nil), None),
                TestMode.Run,
                TestStore.Off,
                blocked,
              )
              .either
            drift <- SuiteLaunch
              .remember(
                repo,
                compiled(name, Platform.Jvm, repo.resolve("jvm")),
                names,
                Nil,
                ask(tested(name), List(tested(name)), Platform.Jvm, DependencyLock(Nil), None),
                TestMode.Run,
                TestStore.Off,
                blocked,
              )
              .either
            api <- unreadable(repo, core, name, blocked)
          yield assertTrue(
            node == Left(
              ModuleTestError.NoNode(name, scala3, NodeMiss.Absent(NodeToolchain.Floor, NodeSource.Path))
            ),
            drift == Left(
              ModuleTestError.Lock(ResolveError.LockDrift(name, scala3, Platform.Jvm, ClasspathScope.Test))
            ),
            api == Left(ModuleTestError.Api(core, scala3, Platform.Jvm)),
          )
    ,
    test("an empty suite name is a record error and a blank name is launched"):
      Workspace
        .work("histos-launch-name")
        .flatMap: repo =>
          val name  = ProjectName.trusted("bare")
          val mod   = bare(name)
          val asked = ask(mod, List(mod), Platform.Jvm, toolLock, None)
          val cell  = compiled(name, Platform.Jvm, repo.resolve("classes"))
          for
            empty <- SuiteLaunch
              .remember(repo, cell, List(""), Nil, asked, TestMode.Quick, TestStore.Off, dieLaunch)
              .either
            seen <- Ref.make(List.empty[String])
            _ <- SuiteLaunch.remember(repo, cell, List(" "), Nil, asked, TestMode.Quick, TestStore.Off, recording(seen))
            ran <- seen.get
          yield assertTrue(
            empty == Left(ModuleTestError.Record(QuickError.Suite(SuiteNameError.Empty))),
            ran == List(" "),
          ),
  )

  private val decided =
    Launched(List("demo.Missing", "demo.B"), Chunk(cached("demo.A"), cached("demo.C")))

  private def launch(
      repo: Path,
      ready: Ready,
      mode: TestMode,
      store: TestStore,
      seen: Ref[List[String]],
  ): ZIO[Any, ModuleTestError, Launched] =
    for
      _      <- seen.set(Nil)
      stream <- SuiteLaunch.remember(
        repo,
        ready.cell,
        names,
        List(ready.classes),
        ready.ask,
        mode,
        store,
        recording(seen),
      )
      events <- stream.runCollect
      ran    <- seen.get
    yield Launched(ran, events)

  private def staged(repo: Path): ZIO[Any, CoverGap | IoFailure | StampError | QuickError, Ready] =
    val name    = ProjectName.trusted("core")
    val mod     = bare(name)
    val classes = repo.resolve("classes")
    val asked   = ask(mod, List(mod), Platform.Jvm, toolLock, None)
    ZIO
      .fromEither(SuiteCover.stored(repo, asked))
      .flatMap: cover =>
        for
          _       <- writeBytes(classes.resolve("demo").resolve("A.class"), classBytes("demo/A"))
          _       <- writeBytes(classes.resolve("demo").resolve("B.class"), classBytes("demo/B"))
          _       <- writeBytes(classes.resolve("demo").resolve("C.class"), classBytes("demo/C"))
          stamped <- digestOf("demo.A", classes, cover)
          other   <- digestOf("demo.C", classes, cover)
          _       <- QuickRecord.save(repo, name, scala3, Platform.Jvm, SuiteName("demo.A"), stamped)
          _       <- QuickRecord.save(repo, name, scala3, Platform.Jvm, SuiteName("demo.C"), other)
        yield Ready(compiled(name, Platform.Jvm, classes), classes, asked)

  private def digestOf(
      suite: String,
      classes: Path,
      cover: SuiteStamp.Cover,
  ): ZIO[Any, StampError, Sha256] =
    SuiteStamp
      .digest(suite, List(classes), cover)
      .flatMap:
        case Some(sha) => ZIO.succeed(sha)
        case None      => ZIO.fail(StampError.Missing(suite))

  private def unreadable(
      repo: Path,
      core: ProjectName,
      app: ProjectName,
      run: List[String] => ZIO[Any, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]],
  ): ZIO[Any, ModuleTestError | IoFailure, Either[ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]]] =
    val classes  = GraphCompile.classesDir(repo, core, scala3, Platform.Jvm)
    val upstream = bare(core)
    val current  = bare(app).copy(dependsOn = List(ProjectDep(core, None)))
    val asked    = ask(current, List(upstream, current), Platform.Jvm, toolLock, None)
    GraphCompile.analysisDirOf(classes) match
      case None =>
        ZIO.succeed(Left(ModuleTestError.Api(ProjectName.trusted("missing"), scala3, Platform.Jvm)))
      case Some(analysis) =>
        Workspace.writeString(analysis.resolve(ApiDigest.fileName), "nope\n") *>
          SuiteLaunch
            .remember(
              repo,
              compiled(app, Platform.Jvm, repo.resolve("out")),
              names,
              Nil,
              asked,
              TestMode.Run,
              TestStore.Off,
              run,
            )
            .either

  private def recording(
      seen: Ref[List[String]]
  ): List[String] => ZIO[Any, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    names => seen.set(names) *> ZIO.succeed(ZStream.empty)

  private def dieLaunch: List[String] => ZIO[Any, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    _ => ZIO.die(new IllegalStateException("launched"))

  private def cached(suite: String): TestFeed =
    TestFeed.Cached(ProjectName.trusted("core"), scala3, Platform.Jvm, suite)

  private def compiled(name: ProjectName, platform: Platform, classes: Path): CompiledCell =
    CompiledCell(name, scala3, platform, classes, 0, None, false, None, None)

  private def ask(
      module: Module,
      modules: List[Module],
      platform: Platform,
      lock: DependencyLock,
      node: Option[RuntimeIdentity],
  ): SuiteAsk =
    SuiteAsk(module, modules, scala3, platform, lock, repos, ToolCatalog.default, NativeOrigin.Central, jdk, node)

  private def bare(name: ProjectName): Module =
    Module(
      name,
      Platforms(Platform.Jvm, List(Platform.Js)),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )

  private def tested(name: ProjectName): Module =
    bare(name).copy(testDependencies = List(Dep.trusted("org.example", "kit", "1", DepKind.Java)))

  private def toolLock: DependencyLock =
    DependencyLock(Nil, Nil, List(tool(compile, toolSha)))

  private def tool(request: ToolRequest, sha: Sha256): LockedTool =
    LockedTool(
      request.id,
      request.roots,
      Repositories.ordered(request.repositories),
      request.roots.map(root =>
        LockedArtifact(root.organization, root.name, root.revision, sha, histos.resolve.Repository.Central)
      ),
      Nil,
    )

  private def classBytes(name: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, "", "java/lang/Object", Array.empty[String])
    val init = writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", "", Array.empty[String])
    init.visitCode()
    init.visitVarInsn(Opcodes.ALOAD, 0)
    init.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    init.visitInsn(Opcodes.RETURN)
    init.visitMaxs(1, 1)
    init.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  private final case class Ready(cell: CompiledCell, classes: Path, ask: SuiteAsk)
  private final case class Launched(ran: List[String], events: Chunk[TestFeed])
end SuiteLaunchLawSpec
