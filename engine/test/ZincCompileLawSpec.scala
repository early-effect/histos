package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.native.{NativeOrigin, NativeToolchain}
import histos.resolve.{LockedTools, Repositories, ResolutionService, Resolve}
import histos.toolchain.RuntimeIdentity
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  CompilerClosure,
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
  private val jdk = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )
  private val lawName = ProjectName.trusted("law")

  def spec = suite("zinc compile laws")(
    test("the source row is the path relative to the checkout, then the bytes"):
      Workspace
        .work("histos-source-row")
        .flatMap: dir =>
          val left    = dir.resolve("left")
          val right   = dir.resolve("right")
          val outside = dir.resolve("outside.scala")
          val text    = "object Core\n"
          val core    = "core/src/main/scala/Core.scala"
          val other   = "core/src/main/scala/Other.scala"
          for
            _       <- Workspace.writeString(left.resolve(core), text)
            _       <- Workspace.writeString(right.resolve(core), text)
            _       <- Workspace.writeString(left.resolve(other), text)
            _       <- Workspace.writeString(outside, text)
            same    <- JvmCompile.sourceDigest(left, List(left.resolve(core)))
            twin    <- JvmCompile.sourceDigest(right, List(right.resolve(core)))
            swapped <- JvmCompile.sourceDigest(left, List(left.resolve(other), left.resolve(core)))
            ordered <- JvmCompile.sourceDigest(left, List(left.resolve(core), left.resolve(other)))
            moved   <- JvmCompile.sourceDigest(left, List(left.resolve(other)))
            refused <- JvmCompile.sourceDigest(left, List(outside)).either
          yield assertTrue(
            same == twin,
            same != moved,
            swapped == ordered,
            refused == Left(
              CompileError.Outside(outside.toAbsolutePath.normalize, left.toAbsolutePath.normalize)
            ),
          )
    ,
    test(
      "a one-file program compiles, the same request does not start the compiler again, and a type error is that diagnostic"
    ):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-zinc-compile")
          .flatMap: dir =>
            val hello = dir.resolve("Hello.scala")
            val bad   = dir.resolve("Bad.scala")
            val ran   =
              for
                _      <- Workspace.writeString(hello, "object Hello:\n  def ready: Int = 1\n")
                _      <- Workspace.writeString(bad, "object Bad:\n  val n: Int = \"no\"\n")
                first  <- compile(closure.scalaVersion, hello, dir.resolve("classes"), dir.resolve("analysis"), dir)
                after  <- ZIO.serviceWithZIO[CompilerStarts](_.get)
                second <- compile(closure.scalaVersion, hello, dir.resolve("classes"), dir.resolve("analysis"), dir)
                held   <- ZIO.serviceWithZIO[CompilerStarts](_.get)
                broken <- compile(
                  closure.scalaVersion,
                  bad,
                  dir.resolve("bad-classes"),
                  dir.resolve("bad-analysis"),
                  dir,
                ).either
                spent <- ZIO.serviceWithZIO[CompilerStarts](_.get)
                wrote <- ZIO.attemptBlocking(Files.isRegularFile(dir.resolve("classes").resolve("Hello.class"))).orDie
              yield assertTrue(
                first.classFiles >= 1 && first.changed && first.miss.isDefined,
                wrote,
                after == 1,
                !second.changed && second.miss.isEmpty,
                held == 1,
                broken.fold(_.message, _ => "").contains("Required: Int"),
                spent == 2,
              )
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & CompilerClosure & Compilers](
              ZLayer.fromFunction((resolve: Resolve, closure: CompilerClosure) =>
                LockedTools(closure.lock, ToolCatalog.default, Repositories.central, resolve, None)
              ) ++ CompilerStarts.live ++ ZLayer.succeed(ContentStore.in(dir.resolve("store")))
            )
    ,
    test("a module with no dependencies compiles to that class file"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-graph-compile")
          .flatMap: dir =>
            val source = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val ran    =
              for
                _         <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                collected <- feed(dir, closure)
                done = collected.collect { case CompileFeed.Done(cell) => cell }.toList
                wrote <- ZIO
                  .attemptBlocking(done.map(cell => Files.isRegularFile(cell.classes.resolve("Hello.class"))))
                  .orDie
              yield assertTrue(
                failedText(collected) == "",
                done.map(cell => (cell.module, cell.version, cell.platform, cell.changed)) ==
                  List((lawName, closure.scalaVersion, Platform.Jvm, true)),
                done.forall(_.classFiles >= 1),
                wrote == List(true),
              )
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & Compilers](graphEnv(closure, dir))
    ,
    test("a resource lands in classes, and deleting it removes that file without a scala change"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-resource-sync")
          .flatMap: dir =>
            val source = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val note   = dir.resolve("src").resolve("main").resolve("resources").resolve("note.txt")
            val ran    =
              for
                _      <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                _      <- Workspace.writeString(note, "plain")
                first  <- feed(dir, closure).flatMap(only)
                copied <- Workspace.readString(first.classes.resolve("note.txt"))
                _      <- ZIO.attemptBlocking(Files.delete(note)).orDie
                second <- feed(dir, closure).flatMap(only)
                still  <- ZIO.attemptBlocking(Files.exists(second.classes.resolve("note.txt"))).orDie
                klass  <- ZIO.attemptBlocking(Files.isRegularFile(second.classes.resolve("Hello.class"))).orDie
              yield assertTrue(copied == "plain", !still, klass, !second.changed)
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & Compilers](graphEnv(closure, dir))
    ,
    test("a resource that is already a class file fails and leaves that class"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-resource-class")
          .flatMap: dir =>
            val source    = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val klass     = dir.resolve("src").resolve("main").resolve("resources").resolve("Hello.class")
            val classPath = GraphCompile.classesDir(dir, lawName, closure.scalaVersion).resolve("Hello.class")
            val ran       =
              for
                _         <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                _         <- Workspace.writeString(klass, "not a class")
                collected <- feed(dir, closure).either
                bytes     <- ZIO.attemptBlocking {
                  if Files.isRegularFile(classPath) then Files.readAllBytes(classPath) else Array.emptyByteArray
                }.orDie
              yield assertTrue(
                collected.fold(_.message, failedText).contains("already a class file"),
                bytes.nonEmpty,
                !bytes.sameElements("not a class".getBytes(java.nio.charset.StandardCharsets.UTF_8)),
              )
            ran.provideSomeLayer[TestEnvironment & Scope & Resolve & Compilers](graphEnv(closure, dir))
    ,
    test("a native compile that produced no nir for a source fails and stores nothing"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        closure.nativeScala.flatMap: version =>
          Workspace
            .work("histos-native-nir")
            .flatMap: dir =>
              val source = dir.resolve("A.scala")
              val dummy  = dir.resolve("dummy.jar")
              val store  = ContentStore.in(dir.resolve("store"))
              val ran    =
                for
                  _        <- Workspace.writeString(source, "object A:\n  def x: Int = 1\n")
                  _        <- emptyJar(dummy)
                  compiled <- JvmCompile
                    .compilePrepared(
                      CompileRequest(
                        CompileToolchain(version, Platform.Native, jdk, ScalacFlags.empty),
                        List(source),
                        Nil,
                        dir.resolve("classes"),
                        dir.resolve("analysis"),
                        Nil,
                        Nil,
                        checkout = dir,
                      ),
                      CellPlatform.Native(NativeToolchain.Release),
                      List(dummy),
                    )
                    .either
                  entries <- store.list.mapError(CompileError.Stored(_))
                yield assertTrue(
                  compiled == Left(CompileError.NoNir(source.toAbsolutePath.normalize)),
                  entries.isEmpty,
                )
              ran.provideSomeLayer[TestEnvironment & Scope & Resolve & CompilerClosure & Compilers](
                ZLayer.fromFunction((resolve: Resolve, closure: CompilerClosure) =>
                  LockedTools(closure.lock, ToolCatalog.default, Repositories.central, resolve, None)
                ) ++ CompilerStarts.live ++ ZLayer.succeed(store)
              ),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer ++ Compilers.live(1))

  private def emptyJar(path: Path): UIO[Unit] =
    ZIO.attemptBlocking {
      val out = new java.util.zip.ZipOutputStream(Files.newOutputStream(path))
      out.close()
    }.orDie

  private def compile(scala3: ScalaVersion, source: Path, classes: Path, analysis: Path, checkout: Path) =
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
          checkout = checkout,
        ),
        Nil,
        _ => (),
      )

  private def graph(scala3: ScalaVersion): ProjectGraph =
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

  private def feed(dir: Path, closure: CompilerClosure) =
    val session = CompileSession(dir, graph(closure.scalaVersion), jdk, NativeOrigin.Central, closure.pinning(lawName))
    GraphCompile.cellFeed(session, List(lawName -> closure.scalaVersion), Platform.Jvm).runCollect

  /** The one cell a feed finished, or the feed's failure text as a defect of the law. */
  private def only(collected: Chunk[CompileFeed]): UIO[CompiledCell] =
    collected.collect { case CompileFeed.Done(cell) => cell }.toList match
      case cell :: Nil if failedText(collected).isEmpty => ZIO.succeed(cell)
      case _                                            => ZIO.dieMessage(failedText(collected))

  private def failedText(feeds: Chunk[CompileFeed]): String =
    feeds.collect { case CompileFeed.Failed(error) => error.message }.mkString("\n")

  private def graphEnv(closure: CompilerClosure, dir: Path) =
    ResolutionService.pinned(closure.pinning(lawName)) ++
      CompileMemo.live ++
      ZLayer.succeed(ContentStore.in(dir.resolve("store")))
end ZincCompileLawSpec
