package histos.zinc

import histos.cache.ContentStore
import histos.native.{NativeFetch, NativeOrigin, NativeToolchainError}
import histos.resolve.{DependencyLock, LockedCell, ResolveError}
import histos.schedule.CacheError
import histos.toolchain.BuildCompiler
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{Jvm, Module, Platform, ProjectGraph, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.Path

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object CompilerSpec extends ZIOSpecDefault:
  private val name    = ProjectName("core")
  private val refused = ScalaVersion("2.12.0")

  def spec =
    suite("compiler")(
      miss,
      hit,
      failed,
    ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](running) @@ TestAspect.withLiveSystem @@
      TestAspect.timeout(2.minutes)

  private val running =
    ZLayer.scoped:
      BuildCompiler.version.flatMap: version =>
        histos.ToolFixture.jvm(version).build.map(_.get[histos.resolve.LockedTools])

  private def miss =
    test("a miss records one compiler call"):
      BuildCompiler.version.flatMap: version =>
        prepared(version).flatMap: (jdk, root, built, found, store) =>
          counted(store)(
            for
              _     <- stub[Compiler](_.compile(any[CompileCall]))((call: CompileCall) => ZIO.succeed(outputOf(call)))
              cells <- GraphCompile.runCounted(root, built, Nil, jdk, NativeOrigin.Central, None)
              calls <- ZIO.serviceWith[Stubbed[Compiler]](_.calls)
            yield assertTrue(
              recorded(cells, found, changed = true),
              calls.size == 1,
              calls.map(_.id.jvmName).toList == List("compile"),
            )
          )

  private def hit =
    test("a hit records no compiler call"):
      BuildCompiler.version.flatMap: version =>
        prepared(version).flatMap: (jdk, root, built, found, store) =>
          counted(store)(
            for
              _ <- stub[Compiler](_.compile(any[CompileCall]))((call: CompileCall) =>
                ZIO.serviceWithZIO[ContentStore](content => CompileReplay.seed(content, call))
              )
              first     <- GraphCompile.runCounted(root, built, Nil, jdk, NativeOrigin.Central, None)
              afterMiss <- ZIO.serviceWith[Stubbed[Compiler]](_.calls)
              second    <- GraphCompile.runCounted(root, built, Nil, jdk, NativeOrigin.Central, None)
              afterHit  <- ZIO.serviceWith[Stubbed[Compiler]](_.calls)
            yield assertTrue(
              recorded(first, found, changed = true),
              recorded(second, found, changed = false),
              afterMiss.size == 1,
              afterMiss.map(_.id.jvmName).toList == List("compile"),
              afterHit.size == 1,
            )
          )

  private def failed =
    outlines match
      case head :: tail =>
        (test("a compile error is the cell failure"):
          BuildCompiler.version.flatMap: pin =>
            prepared(pin).flatMap: (jdk, root, built, found, store) =>
              check(Gen.elements((head :: tail)*)): build =>
                val error = build(pin)
                counted(store)(
                  for
                    _      <- stub[Compiler](_.compile(any[CompileCall]))(ZIO.fail(error))
                    result <- GraphCompile.runCounted(root, built, Nil, jdk, NativeOrigin.Central, None).either
                  yield assertTrue(surfaces(result, error, found))
                )
        ) @@ TestAspect.samples(outlines.size)
      case Nil =>
        test("compile errors have a case"):
          assertTrue(outlines.nonEmpty)

  private def prepared(version: ScalaVersion) =
    for
      jdk   <- GraphCompile.jdkHere
      root  <- Workspace.work("histos-compiler")
      cas   <- Workspace.work("histos-compiler-cas")
      built <- ZIO.fromEither(Projects(module(name.value, Jvm)(version)))
      found <- ZIO.fromOption(moduleOf(built)).orElseFail(GraphCompileError.NoRoot(name))
      _     <- Workspace.writeString(sourceOf(root, found), found.name.value)
      _     <- Workspace.writeString(histos.resolve.Resolve.lockPath(root), lockOf(found))
      _     <- histos.ToolFixture.seed(root, built)
    yield (jdk, root, built, found, ContentStore.in(cas))

  private def counted[E, A](store: ContentStore)(
      effect: ZIO[histos.resolve.Resolve & ContentStore & Compiler & Stubbed[Compiler], E, A]
  ) =
    effect.provideSomeLayer[histos.resolve.LockedTools](
      stubbedTraced[Compiler] ++
        ZLayer.succeed(store) ++
        ZLayer.fromZIO(ZIO.serviceWith[histos.resolve.LockedTools](_.resolve))
    )

  private def outputOf(call: CompileCall): CompileOutput =
    CompileOutput(
      call.request.classes,
      0,
      call.key,
      changed = true,
      sources = Some(SourceCount(0, call.request.sources.size)),
      classesWritten = Some(0),
      miss = None,
    )

  private def recorded(cells: List[CompiledCell], module: Module, changed: Boolean): Boolean =
    cells match
      case cell :: Nil =>
        cell.module == module.name &&
        module.versions.all.contains(cell.version) &&
        module.platforms.all.contains(cell.platform) &&
        cell.changed == changed
      case _ => false

  private def surfaces(result: Either[GraphCompileError, List[CompiledCell]], error: CompileError, module: Module) =
    result match
      case Left(GraphCompileError.Cells(errors)) =>
        val found = errors.collect:
          case GraphCompileError.Failed(failedModule, version, platform, failed) =>
            (failedModule, version, platform, failed)
        found.toList match
          case (failedModule, version, platform, failed) :: Nil =>
            failed == error && failedModule == module.name && module.versions.all.contains(version) &&
            module.platforms.all.contains(platform)
          case _ => false
      case _ => false

  private def moduleOf(graph: ProjectGraph): Option[Module] =
    graph.projects.collect { case found: Module => found } match
      case found :: Nil => Some(found)
      case _            => None

  private def sourceOf(root: Path, module: Module): Path =
    Sources.mainFile(root.resolve(module.baseDirectory), s"${module.name.value}.scala")

  private def lockOf(module: Module): String =
    val cells = module.versions.all.flatMap: version =>
      module.platforms.all.map: platform =>
        LockedCell(module.name, version, platform, Nil, Nil, Nil)
    DependencyLock.render(DependencyLock(cells))

  /** One inhabitant per `CompileError` constructor. The pin is the running compiler, used only by `Toolchain`. */
  private val outlines: List[ScalaVersion => CompileError] =
    val path = Path.of("absent")
    List(
      _ => CompileError.Resolved(ResolveError.NotFound("org.example", "missing", "0")),
      _ => CompileError.UnsupportedPlatform(Platform.Js),
      _ => CompileError.UnsupportedScala(refused),
      _ => CompileError.NoSources,
      _ => CompileError.Banned(path, ClasspathBan.Histos),
      _ => CompileError.Failed(List(CompileDiagnostic.fromCompiler("boom"))),
      pin => CompileError.Toolchain(ToolchainMiss.Fetch(pin)),
      _ => CompileError.Io(IoFailure(path, IoFault.Read)),
      _ => CompileError.JsLibrary(JsLibraryMiss.Empty),
      _ => CompileError.NativeFetch(NativeFetch.Missing("nativelib", "0")),
      _ => CompileError.NativeOrigin(NativeToolchainError.Empty),
      _ => CompileError.NativeLibrary("absent"),
      _ => CompileError.Stored(CacheError.Io(IoFailure(path, IoFault.Read))),
    )
end CompilerSpec
