package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.native.NativeOrigin
import histos.resolve.{
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  LockedTools,
  Repositories,
  ResolutionService,
  Resolve,
}
import histos.schedule.Digest
import histos.select.SelectError
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  ToolCatalog,
}

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue

import slayer.{Stubbed, any, stub, stubbed}

import zio.*
import zio.test.*

object GraphCompileLawSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion.trusted("3.10.0")
  private val scala213 = ScalaVersion.trusted("2.13.18")
  private val scala212 = ScalaVersion.trusted("2.12.20")
  private val jdk      = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )

  def spec = suite("graph compile refusals")(
    test("an unknown project name fails the feed before a compiler"):
      Workspace
        .work("histos-graph-missing")
        .flatMap: repo =>
          val missing = ProjectName.trusted("other")
          val known   = module("core")
          val session =
            CompileSession(repo, ProjectGraph.trusted(List(known)), jdk, NativeOrigin.Central, DependencyLock(Nil))
          val program =
            for
              _         <- silence
              collected <- GraphCompile.cellFeed(session, List(missing -> scala3), Platform.Jvm).runCollect.either
            yield collected match
              case Left(GraphCompileError.Target(SelectError.Unknown(found, _))) =>
                assertTrue(found == missing)
              case other =>
                assertTrue(other == Left(GraphCompileError.Target(SelectError.Unknown(missing, Nil))))
          program.provideSomeLayer[TestEnvironment & Scope](selected)
    ,
    test("a repeated project name is a duplicate target"):
      Workspace
        .work("histos-graph-duplicate")
        .flatMap: repo =>
          val known   = module("core")
          val session =
            CompileSession(repo, ProjectGraph.trusted(List(known)), jdk, NativeOrigin.Central, DependencyLock(Nil))
          val program =
            for
              _         <- silence
              collected <- GraphCompile
                .cellFeed(session, List(known.name -> scala3, known.name -> scala3), Platform.Jvm)
                .runCollect
                .either
            yield assertTrue(collected == Left(GraphCompileError.Target(SelectError.Duplicate(known.name))))
          program.provideSomeLayer[TestEnvironment & Scope](selected)
    ,
    test("a module without a source root refuses with NoRoot and does not begin"):
      Workspace
        .work("histos-graph-root")
        .flatMap: repo =>
          val mod     = module("core")
          val program =
            for
              _       <- silence
              resolve <- ZIO.service[Resolve]
              log = FeedLog()
              text   <- GraphCompile.compileCell(cell(repo, mod, scala3, Platform.Jvm, resolve, log))
              starts <- ZIO.serviceWithZIO[CompilerStarts](_.get)
            yield assertTrue(
              text.isEmpty,
              log.listed == List(CompileFeed.Failed(GraphCompileError.NoRoot(mod.name))),
              starts == 0,
            )
          program.provideSomeLayer[TestEnvironment & Scope](remembered)
    ,
    test("a source root with no scala or java file refuses with NoSources"):
      Workspace
        .work("histos-graph-empty")
        .flatMap: repo =>
          val mod     = module("core")
          val root    = sourceRoot(repo, mod)
          val program =
            for
              _       <- silence
              _       <- Workspace.writeString(root.resolve("notes.txt"), "note")
              resolve <- ZIO.service[Resolve]
              log = FeedLog()
              text   <- GraphCompile.compileCell(cell(repo, mod, scala3, Platform.Jvm, resolve, log))
              starts <- ZIO.serviceWithZIO[CompilerStarts](_.get)
            yield assertTrue(
              text.isEmpty,
              log.listed == List(
                CompileFeed.Began(mod.name, scala3, Platform.Jvm),
                CompileFeed.Failed(GraphCompileError.NoSources(mod.name, root)),
              ),
              starts == 0,
            )
          program.provideSomeLayer[TestEnvironment & Scope](remembered)
    ,
    test("test sources alone finish as an empty cell"):
      Workspace
        .work("histos-graph-vacant")
        .flatMap: repo =>
          val mod      = module("core")
          val root     = sourceRoot(repo, mod)
          val classes  = GraphCompile.classesDir(repo, mod.name, scala3)
          val analysis = classesDirAnalysis(repo, mod.name, scala3)
          val program  =
            for
              _       <- silence
              _       <- Workspace.writeString(root.resolve("test").resolve("A.scala"), "class A")
              resolve <- ZIO.service[Resolve]
              log = FeedLog()
              text    <- GraphCompile.compileCell(cell(repo, mod, scala3, Platform.Jvm, resolve, log))
              starts  <- ZIO.serviceWithZIO[CompilerStarts](_.get)
              api     <- ZIO.fromEither(ApiDigest.read(analysis))
              product <- ZIO.fromEither(ProductDigest.read(analysis))
              hashed  <- ZIO.fromEither(ProductDigest.ofDirectory(classes, repo))
            yield assertTrue(
              text == ScheduleCells.render(Map((mod.name, scala3, Platform.Jvm) -> classes)),
              log.listed == List(
                CompileFeed.Began(mod.name, scala3, Platform.Jvm),
                CompileFeed.Done(
                  CompiledCell(
                    mod.name,
                    scala3,
                    Platform.Jvm,
                    classes,
                    0,
                    None,
                    false,
                    Some(SourceCount(0, 0)),
                    Some(0),
                  )
                ),
              ),
              starts == 0,
              api == ApiDigest.empty,
              product == hashed,
            )
          program.provideSomeLayer[TestEnvironment & Scope](remembered)
    ,
    test("a histos jar on the classpath is banned before the compiler"):
      Workspace
        .work("histos-graph-ban")
        .flatMap: repo =>
          val mod     = module("core")
          val program =
            for
              _       <- silence
              jarPath <- jar(repo.resolve("histos.jar"), "histos/HistosBuild.class")
              found   <- refused(repo, mod, scala3, Platform.Jvm, List(jarPath))
            yield assertTrue(
              found.feeds == List(
                CompileFeed.Began(mod.name, scala3, Platform.Jvm),
                CompileFeed.Failed(
                  GraphCompileError.Failed(
                    mod.name,
                    scala3,
                    Platform.Jvm,
                    CompileError.Banned(jarPath, ClasspathBan.Histos),
                  )
                ),
              ),
              found.text.isEmpty,
              found.starts == 0,
            )
          program.provideSomeLayer[TestEnvironment & Scope](scripted)
    ,
    test("scala 2.12 is refused before that banned jar is opened"):
      Workspace
        .work("histos-graph-212")
        .flatMap: repo =>
          val mod     = module("core")
          val program =
            for
              _       <- silence
              jarPath <- jar(repo.resolve("histos.jar"), "histos/HistosBuild.class")
              found   <- refused(repo, mod, scala212, Platform.Jvm, List(jarPath))
            yield assertTrue(
              found.feeds == List(
                CompileFeed.Began(mod.name, scala212, Platform.Jvm),
                CompileFeed.Failed(
                  GraphCompileError.Failed(mod.name, scala212, Platform.Jvm, CompileError.UnsupportedScala(scala212))
                ),
              ),
              found.text.isEmpty,
              found.starts == 0,
            )
          program.provideSomeLayer[TestEnvironment & Scope](scripted)
    ,
    test("scala 2.13 on javascript is refused before the compiler"):
      Workspace
        .work("histos-graph-js")
        .flatMap: repo =>
          val mod     = module("core")
          val program =
            for
              _       <- silence
              jarPath <- jar(repo.resolve("histos.jar"), "notes.txt")
              found   <- refused(repo, mod, scala213, Platform.Js, List(jarPath))
            yield assertTrue(
              found.feeds == List(
                CompileFeed.Began(mod.name, scala213, Platform.Js),
                CompileFeed.Failed(
                  GraphCompileError.Failed(mod.name, scala213, Platform.Js, CompileError.UnsupportedScala(scala213))
                ),
              ),
              found.text.isEmpty,
              found.starts == 0,
            )
          program.provideSomeLayer[TestEnvironment & Scope](scripted)
    ,
    test("coverage test classes map onto test analysis, and other product paths do not"):
      val repo    = Path.of("/repo")
      val name    = ProjectName.trusted("core")
      val version = scala3
      val covered = repo
        .resolve(".histos")
        .resolve("coverage")
        .resolve("test-classes")
        .resolve(name.value)
        .resolve(version.value)
        .resolve("native")
      val mapped = repo
        .resolve(".histos")
        .resolve("coverage")
        .resolve("test-analysis")
        .resolve(name.value)
        .resolve(version.value)
        .resolve("native")
      val analysis =
        repo.resolve(".histos").resolve("analysis").resolve(name.value).resolve(version.value).resolve("jvm")
      val short = repo.resolve(".histos").resolve("classes").resolve(name.value).resolve(version.value)
      val bare  = repo.resolve("coverage").resolve("classes").resolve(name.value).resolve(version.value).resolve("jvm")
      assertTrue(
        GraphCompile.analysisDirOf(covered).contains(mapped),
        GraphCompile.analysisDirOf(analysis).isEmpty,
        GraphCompile.analysisDirOf(short).isEmpty,
        GraphCompile.analysisDirOf(bare).isEmpty,
        GraphCompile.classesDir(repo, name, version) == GraphCompile.classesDir(repo, name, version, Platform.Jvm),
        GraphCompile.classesDir(repo, name, version, Platform.Jvm) !=
          GraphCompile.classesDir(repo, name, version, Platform.Js),
      ),
  )

  private val drivers =
    stubbed[Resolve] ++
      stubbed[ContentStore] ++
      stubbed[Compilers] ++
      stubbed[ResolutionService] ++
      CompilerStarts.live

  private val remembered = drivers ++ CompileMemo.live
  private val scripted   = drivers ++ stubbed[CompileMemo]
  private val selected   =
    stubbed[Resolve] ++
      stubbed[ContentStore] ++
      stubbed[Compilers] ++
      stubbed[CompileMemo] ++
      stubbed[ResolutionService]

  private def silence: ZIO[
    Stubbed[Resolve] & Stubbed[ContentStore] & Stubbed[Compilers] & Stubbed[ResolutionService],
    Nothing,
    Unit,
  ] =
    stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.die(new IllegalStateException("fetched"))) *>
      stub[ContentStore](_.read(any[Digest]))(ZIO.die(new IllegalStateException("read"))) *>
      stub[Compilers](
        _.compile(
          any[CompileRequest],
          any[CompilerArtifacts],
          any[List[String]],
          any[CompileProblem => Unit],
          any[Sha256],
          any[UIO[Unit]],
        )
      )(ZIO.die(new IllegalStateException("compiled"))) *>
      stub[ResolutionService](_.current)(ZIO.die(new IllegalStateException("resolved")))

  private final case class Refused(feeds: List[CompileFeed], text: String, starts: Int)

  private def refused(
      repo: Path,
      mod: Module,
      version: ScalaVersion,
      platform: Platform,
      jars: List[Path],
  ): ZIO[GraphCompile.Fed & Stubbed[CompileMemo], IoFailure | GraphCompileError, Refused] =
    for
      resolve <- ZIO.service[Resolve]
      root   = sourceRoot(repo, mod)
      source = root.resolve("A.scala")
      _ <- Workspace.writeString(source, "class A")
      _ <- stub[CompileMemo](
        _.hashes(
          any[DependencyLock],
          any[Module],
          any[List[UpstreamCell]],
          any[ScalaVersion],
          any[Platform],
          any[ClasspathScope],
          any[LockStamp],
        )
      )(ZIO.none)
      _ <- stub[CompileMemo](
        _.classpath(
          any[Path],
          any[Module],
          any[List[UpstreamCell]],
          any[ScalaVersion],
          any[Platform],
          any[DependencyLock],
          any[Repositories],
          any[ClasspathScope],
          any[LockStamp],
        )
      )(ZIO.succeed(ScalacClasspath(List.empty[Path], jars, List.empty[Sha256], List.empty[Path])))
      before <- ZIO.serviceWithZIO[CompilerStarts](_.get)
      log = FeedLog()
      text  <- GraphCompile.compileCell(cell(repo, mod, version, platform, resolve, log))
      after <- ZIO.serviceWithZIO[CompilerStarts](_.get)
    yield Refused(log.listed, text, after - before)

  private def cell(
      repo: Path,
      mod: Module,
      version: ScalaVersion,
      platform: Platform,
      resolve: Resolve,
      log: FeedLog,
  ): CellRun =
    val tools = LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve)
    val plan  = CompilePlan(
      repo,
      List(mod),
      DependencyLock(Nil),
      jdk,
      NativeOrigin.Central,
      Repositories.central,
      tools,
      LockStamp("law"),
    )
    CellRun(plan, mod, version, platform, Map.empty, _ => ZIO.unit, log.offer, ZIO.unit)

  private def module(name: String): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(Platform.Jvm, List(Platform.Js)),
      ScalaVersions(scala3, List(scala213, scala212)),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )

  private def sourceRoot(repo: Path, mod: Module): Path =
    repo.resolve(mod.baseDirectory).resolve("src")

  private def classesDirAnalysis(repo: Path, name: ProjectName, version: ScalaVersion): Path =
    repo.resolve(".histos").resolve("analysis").resolve(name.value).resolve(version.value).resolve(Platform.Jvm.token)

  private def jar(path: Path, entry: String): ZIO[Any, IoFailure, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new java.util.zip.ZipOutputStream(Files.newOutputStream(path))
        try
          out.putNextEntry(new java.util.zip.ZipEntry(entry))
          out.write(Array.emptyByteArray)
          out.closeEntry()
        finally out.close()
        path
      }
      .mapError(_ => IoFailure(path, IoFault.Write))

  private final class FeedLog:
    private val queue                  = new ConcurrentLinkedQueue[CompileFeed]
    def offer(feed: CompileFeed): Unit =
      queue.add(feed)
      ()
    def listed: List[CompileFeed] =
      queue.toArray.toList.collect { case feed: CompileFeed => feed }
end GraphCompileLawSpec
