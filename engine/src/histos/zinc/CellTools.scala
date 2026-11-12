package histos.zinc

import histos.jdk.Sha256
import histos.js.JsRequests
import histos.native.{NativeClasspath, NativeOrigin, NativeRequests, NativeToolchain}
import histos.resolve.{LockedTools, ResolveError, ToolInput, ToolRequest}
import histos.{JsVersion, Module, Platform, ScalaVersion, ToolCatalog}

import zio.{IO, UIO, ZIO}

/** A cell's platform with what its compile asks of the lock: the Scala.js revision, or the Native toolchain. */
enum CellPlatform:
  case Jvm
  case Js(revision: JsVersion)
  case Native(toolchain: NativeToolchain)

object CellPlatform:
  /** The module's revision, or the catalog's when the module names none. */
  def of(
      module: Module,
      platform: Platform,
      catalog: ToolCatalog,
      native: NativeOrigin,
  ): Either[CompileError, CellPlatform] =
    platform match
      case Platform.Jvm    => Right(Jvm)
      case Platform.Js     => Right(Js(module.jsVersion.getOrElse(catalog.js.revision)))
      case Platform.Native =>
        NativeToolchain
          .of(module.nativeVersion.getOrElse(catalog.native.revision), native)
          .map(Native(_))
          .left
          .map(CompileError.NativeOrigin(_))

/** The plugin a Scala series puts in the compiler row. */
enum SeriesTools:
  case Scala3
  case Scala213(semanticdb: ToolInput)

  def plugins: List[ToolInput] = this match
    case Scala3               => Nil
    case Scala213(semanticdb) => List(semanticdb)
end SeriesTools

/** What a platform adds to a compile: libraries beside the compiler, and a plugin in the compiler row. */
enum PlatformTools:
  case Jvm
  case Js(libraries: ToolInput)
  case Native(libraries: ToolInput, plugin: ToolInput)

  /** The libraries that join the request's compilation tools. */
  def compilation: List[ToolInput] = this match
    case Jvm                  => Nil
    case Js(libraries)        => List(libraries)
    case Native(libraries, _) => List(libraries)

  def plugins: List[ToolInput] = this match
    case Jvm               => Nil
    case Js(_)             => Nil
    case Native(_, plugin) => List(plugin)
end PlatformTools

/** The request a cell compiles, its named input rows, and its key. */
final case class KeyedRequest(request: CompileRequest, rows: List[CompileRow], key: Sha256)

/** The tools a cell's key names besides the request's own rows. A compile fetches them and recall pins them from the
  * lock. Both gather them with one function and build the key with [[CellTools.key]], so the key recall checks is the
  * key the compile stored, for every platform and Scala series.
  */
final case class CellTools(compiler: ToolInput, series: SeriesTools, platform: PlatformTools)

object CellTools:
  /** The tools fetched, so the compiler can load them, and the key the compile stores. */
  def compile(
      platform: CellPlatform,
      base: CompileRequest,
      sources: Sha256,
  ): ZIO[LockedTools, CompileError, (CellTools, KeyedRequest)] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      gather(tools, base.toolchain.version, platform)(request =>
        tools.selected(request).mapError(CompileError.Resolved(_))
      ).map(cell => (cell, key(base, sources, cell)))

  /** The key recall checks. Nothing is fetched. `None` when the lock does not pin one of the tools. */
  def recall(
      tools: LockedTools,
      platform: CellPlatform,
      base: CompileRequest,
      sources: Sha256,
  ): UIO[Option[KeyedRequest]] =
    gather(tools, base.toolchain.version, platform)(pin(tools)).option.map(_.map(key(base, sources, _)))

  /** The platform's libraries join the compilation tools and its plugin joins the request's plugins. The compiler row
    * is the compiler, the series plugin, then the request's plugins.
    */
  def key(base: CompileRequest, sources: Sha256, cell: CellTools): KeyedRequest =
    val request = base.copy(
      compilationTools = base.compilationTools ++ cell.platform.compilation,
      compilerPlugins = base.compilerPlugins ++ cell.platform.plugins,
    )
    val prints = (cell.compiler :: cell.series.plugins ++ request.compilerPlugins).map(_.fingerprint)
    KeyedRequest(
      request,
      CompileInputs.withCompiler(CompileInputs.declared(request, sources), prints),
      CompileKey.withTools(CompileKey.of(request, sources), prints),
    )

  /** Every tool a cell's key names, for its series and platform, each obtained the caller's way. */
  private def gather(
      tools: LockedTools,
      version: ScalaVersion,
      platform: CellPlatform,
  )(obtain: ToolRequest => IO[CompileError, ToolInput]): IO[CompileError, CellTools] =
    for
      compiler <- obtain(CompilerRequests.compiler(version, tools.repositories))
      series   <- ScalaSeries.of(version) match
        case Some(ScalaSeries.Scala3)     => ZIO.succeed(SeriesTools.Scala3)
        case Some(ScalaSeries.Scala2(13)) =>
          obtain(CompilerRequests.semanticdb(version, tools.catalog, tools.repositories)).map(SeriesTools.Scala213(_))
        case _ => ZIO.fail(CompileError.UnsupportedScala(version))
      added <- platform match
        case CellPlatform.Jvm          => ZIO.succeed(PlatformTools.Jvm)
        case CellPlatform.Js(revision) =>
          obtain(JsRequests.libraries(version, revision, tools.repositories, tools.catalog)).map(PlatformTools.Js(_))
        case CellPlatform.Native(toolchain) =>
          obtain(NativeRequests.libraries(version, toolchain, tools.repositories)).map(libraries =>
            PlatformTools.Native(libraries, NativeClasspath.plugin(libraries, toolchain, version))
          )
    yield CellTools(compiler, series, added)

  /** The lock's pin of `request`, or the typed miss that recall reads as no hit. */
  private def pin(tools: LockedTools)(request: ToolRequest): IO[CompileError, ToolInput] =
    ZIO.fromOption(tools.pinned(request)).orElseFail(CompileError.Resolved(ResolveError.NoTool(request.id)))
end CellTools
