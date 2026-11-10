package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.native.NativeOrigin
import histos.resolve.{
  ClasspathScope,
  DependencyLock,
  LockedCell,
  LockedTools,
  Repositories,
  ResolutionService,
  Resolve,
}
import histos.toolchain.RuntimeIdentity
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  ScalacFlags,
  SourcePos,
  Splits,
  ToolCatalog,
}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** Compiles with the Scala 3 compiler the lock already names. Offline resolve reads those jars and does not download.
  */
object ZincCompileLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val jdk    = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )
  private val lawName = ProjectName.trusted("law")

  def spec = suite("zinc compile laws")(
    test(
      "a one-file program compiles, the same request does not start the compiler again, and a type error is that diagnostic"
    ):
      Workspace
        .work("histos-zinc-compile")
        .flatMap: dir =>
          val hello = dir.resolve("Hello.scala")
          val bad   = dir.resolve("Bad.scala")
          val ran   =
            for
              _      <- Workspace.writeString(hello, "object Hello:\n  def ready: Int = 1\n")
              _      <- Workspace.writeString(bad, "object Bad:\n  val n: Int = \"no\"\n")
              first  <- compile(hello, dir.resolve("classes"), dir.resolve("analysis"))
              after  <- ZIO.serviceWithZIO[CompilerStarts](_.get)
              second <- compile(hello, dir.resolve("classes"), dir.resolve("analysis"))
              held   <- ZIO.serviceWithZIO[CompilerStarts](_.get)
              broken <- compile(bad, dir.resolve("bad-classes"), dir.resolve("bad-analysis"))
              spent  <- ZIO.serviceWithZIO[CompilerStarts](_.get)
              missed  = first.fold(_.message, _ => "")
              refused = broken.fold(_.message, _ => "")
            yield assertTrue(
              missed == "",
              first.exists(out => out.classFiles >= 1 && out.changed && out.miss.isDefined),
              Files.isRegularFile(dir.resolve("classes").resolve("Hello.class")),
              after == 1,
              second.exists(out => !out.changed && out.miss.isEmpty),
              held == 1,
              refused.contains("Required: Int"),
              spent == 2,
            )
          ran.provideSomeLayer[TestEnvironment & Scope & Resolve & DependencyLock & Compilers](
            compileEnv ++ ZLayer.succeed(ContentStore.in(dir.resolve("store")))
          )
    ,
    test("a module with no dependencies compiles to that class file"):
      Workspace
        .work("histos-graph-compile")
        .flatMap: dir =>
          ZIO.serviceWithZIO[DependencyLock]: lock =>
            val source = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val ran    =
              for
                _         <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                collected <- GraphCompile
                  .cellFeed(session(dir, lock), List(lawName -> scala3), Platform.Jvm)
                  .runCollect
                  .either
                detail = collected.fold(_.message, chunk => failedText(chunk))
                done   = collected.toOption.toList
                  .flatMap(chunk => chunk.collect { case CompileFeed.Done(cell) => cell }.toList)
              yield done match
                case cell :: Nil =>
                  assertTrue(
                    detail == "",
                    cell.module == lawName,
                    cell.version == scala3,
                    cell.platform == Platform.Jvm,
                    cell.classFiles >= 1,
                    cell.changed,
                    Files.isRegularFile(cell.classes.resolve("Hello.class")),
                  )
                case _ =>
                  assertTrue(detail == "", done.length == 1)
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & Compilers](
              graphEnv(lock, dir.resolve("store"))
            )
    ,
    test("a resource lands in classes, and deleting it removes that file without a scala change"):
      Workspace
        .work("histos-resource-sync")
        .flatMap: dir =>
          ZIO.serviceWithZIO[DependencyLock]: lock =>
            val source = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val note   = dir.resolve("src").resolve("main").resolve("resources").resolve("note.txt")
            val ran    =
              for
                _      <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                _      <- Workspace.writeString(note, "plain")
                first  <- feed(dir, lock)
                copied <- readNote(cellOf(first))
                _      <- ZIO.attemptBlocking(Files.delete(note)).orDie
                second <- feed(dir, lock)
                after  <- ZIO.succeed(cellOf(second))
                still  <- exists(after.map(_.classes.resolve("note.txt")))
                klass  <- existsFile(after.map(_.classes.resolve("Hello.class")))
                recalled   = after.exists(!_.changed)
                secondText = after.fold(identity, _ => "")
                text       = copied
                present    = still
                classFile  = klass
              yield assertTrue(text == "plain", secondText == "", !present, classFile, recalled)
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & Compilers](
              graphEnv(lock, dir.resolve("store"))
            )
    ,
    test("a resource that is already a class file fails and leaves that class"):
      Workspace
        .work("histos-resource-class")
        .flatMap: dir =>
          ZIO.serviceWithZIO[DependencyLock]: lock =>
            val source = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val klass  = dir.resolve("src").resolve("main").resolve("resources").resolve("Hello.class")
            val ran    =
              for
                _         <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                _         <- Workspace.writeString(klass, "not a class")
                collected <- feed(dir, lock)
                detail    = collected.fold(_.message, chunk => failedText(chunk))
                classPath = GraphCompile.classesDir(dir, lawName, scala3).resolve("Hello.class")
                bytes <- ZIO.attemptBlocking {
                  if Files.isRegularFile(classPath) then Files.readAllBytes(classPath) else Array.emptyByteArray
                }.orDie
                present = bytes.nonEmpty
                planted = bytes.sameElements("not a class".getBytes(java.nio.charset.StandardCharsets.UTF_8))
                said    = detail
              yield assertTrue(said.contains("already a class file"), present, !planted)
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & Compilers](
              graphEnv(lock, dir.resolve("store"))
            ),
  ).provideSomeLayerShared[TestEnvironment & Scope](opened)

  private def compile(source: Path, classes: Path, analysis: Path) =
    JvmCompile
      .compilePooled(
        CompileRequest(
          CompileToolchain(scala3, Platform.Jvm, jdk, ScalacFlags.empty),
          List(source),
          Nil,
          classes,
          analysis,
          Nil,
          Nil,
        ),
        Nil,
        _ => (),
      )
      .either

  private def session(repo: Path, lock: DependencyLock): CompileSession =
    CompileSession(repo, graph, jdk, NativeOrigin.Central, pinned(lock))

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  /** The checkout's compiler tools, plus an empty cell for the module under test. */
  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

  private def feed(dir: Path, lock: DependencyLock) =
    GraphCompile
      .cellFeed(session(dir, lock), List(lawName -> scala3), Platform.Jvm)
      .runCollect
      .either

  /** The one finished cell. The text is the failure when the feed did not finish one cell. */
  private def cellOf(collected: Either[GraphCompileError, Chunk[CompileFeed]]): Either[String, CompiledCell] =
    collected match
      case Left(error)  => Left(error.message)
      case Right(chunk) =>
        val done = chunk.collect { case CompileFeed.Done(cell) => cell }.toList
        done match
          case cell :: Nil if failedText(chunk).isEmpty => Right(cell)
          case _                                        => Left(failedText(chunk))

  private def readNote(found: Either[String, CompiledCell]) =
    found match
      case Right(cell) => Workspace.readString(cell.classes.resolve("note.txt"))
      case Left(text)  => ZIO.succeed(text)

  private def exists(found: Either[String, Path]) =
    found match
      case Right(path) => ZIO.attemptBlocking(Files.exists(path)).orDie
      case Left(_)     => ZIO.succeed(true)

  private def existsFile(found: Either[String, Path]) =
    found match
      case Right(path) => ZIO.attemptBlocking(Files.isRegularFile(path)).orDie
      case Left(_)     => ZIO.succeed(false)

  private def failedText(feeds: Chunk[CompileFeed]): String =
    feeds.collect { case CompileFeed.Failed(error) => error.message }.mkString("\n")

  private def compileEnv =
    ZLayer.fromFunction((resolve: Resolve, lock: DependencyLock) =>
      LockedTools(lock, ToolCatalog.default, Repositories.central, resolve, None)
    ) ++ CompilerStarts.live

  private def graphEnv(lock: DependencyLock, store: Path) =
    ResolutionService.pinned(pinned(lock)) ++
      CompileMemo.live ++
      ZLayer.succeed(ContentStore.in(store))

  private def opened =
    val cache = _root_.coursier.cache.CacheDefaults.location.toPath
    Resolve.offline(cache) ++ ZLayer.fromZIO(Resolve.readLock(repository)) ++ Compilers.live(1)

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end ZincCompileLawSpec
