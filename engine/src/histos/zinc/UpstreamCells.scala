package histos.zinc

import histos.resolve.LockedTools
import histos.{Module, Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.ZIO

/** The upstream cell a depender reads: the module, that cell's Scala version, and its classes directory. */
private[histos] final case class UpstreamCell(module: Module, version: ScalaVersion, classes: Path)

/** Which declared cell a depender reads. An exact cell wins. */
private[histos] object UpstreamCells:
  type Cell = (ProjectName, ScalaVersion, Platform)

  def isScala3(version: ScalaVersion): Boolean =
    version.value.startsWith("3.")

  def is213(version: ScalaVersion): Boolean =
    version.value.startsWith("2.13.")

  def rank(version: ScalaVersion): (Int, Int, Int) =
    val head                = version.value.takeWhile(ch => ch.isDigit || ch == '.')
    val parts               = head.split('.')
    def at(index: Int): Int =
      if index >= parts.length then 0
      else parts(index).toIntOption.getOrElse(0)
    (at(0), at(1), at(2))

  def required(
      depender: ScalaVersion,
      upstream: Module,
      platform: Platform,
      declared: Set[Cell],
  ): List[Cell] =
    val cells = upstream.versions.all.flatMap: item =>
      val key = (upstream.name, item, platform)
      if declared.contains(key) then List(key) else Nil
    val exact = (upstream.name, depender, platform)
    if declared.contains(exact) then List(exact)
    else if is213(depender) then
      cells.filter((_, item, _) => is213(item)).sortBy((_, item, _) => rank(item)).takeRight(1)
    else if isScala3(depender) then cells.filter((_, item, _) => isScala3(item))
    else Nil

  /** The cell compile would put on the classpath. Exact wins. Otherwise the newest declared version this compiler can
    * read: same series, not a newer minor.
    */
  def chosen(
      depender: ScalaVersion,
      upstream: Module,
      platform: Platform,
      modules: List[Module],
  ): Option[ScalaVersion] =
    val declared = modules.flatMap(item => item.versions.all.map(version => (item.name, version, platform))).toSet
    newestAtMost(depender, required(depender, upstream, platform, declared))

  private def newestAtMost(depender: ScalaVersion, candidates: List[Cell]): Option[ScalaVersion] =
    candidates
      .filter((_, item, _) => atMost(item, depender))
      .sortBy((_, item, _) => rank(item))
      .lastOption
      .map((_, version, _) => version)

  /** Every declared cell of `modules` on `platform`, at the classes directory its compile writes. */
  def declared(repo: Path, modules: List[Module], platform: Platform): Map[Cell, Path] =
    modules
      .filter(_.platforms.all.contains(platform))
      .flatMap: module =>
        module.versions.all.map: version =>
          (module.name, version, platform) -> GraphCompile.classesDir(repo, module.name, version, platform)
      .toMap

  /** The cell of each upstream in `chain` that the `version` cell of `module` reads, from `cells`. Compile, its recall,
    * and every classpath built for the cell read this, so they read the same cells.
    *
    * An exact cell wins. A 2.13 cell reads the newest declared 2.13 cell. A Scala 3 cell reads the newest TASTy header
    * `dotty.tools.tasty.TastyFormat.isVersionCompatible` accepts. The three compiler numbers come from this cell's
    * `tasty-core`, not from the compiler that built Histos. While no candidate holds TASTy yet, as for an editor import
    * before the first compile or an upstream with no main sources, it reads [[chosen]]'s cell.
    */
  def read(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      cells: Map[Cell, Path],
  ): ZIO[LockedTools, GraphCompileError, List[UpstreamCell]] =
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    val exact  = chain.forall(dependency => cells.contains((dependency.name, version, platform)))
    if !isScala3(version) || exact then each(module, chain, version, platform, cells, None)
    else
      ZIO.scoped:
        ZIO
          .acquireRelease(
            Scala3
              .jars(version)
              .mapError(failed)
              .flatMap(toolchain => ZIO.fromEither(TastyCompat.open(version, toolchain.loaderJars)).mapError(failed))
          )(reader => ZIO.succeed(reader.close()))
          .flatMap(reader => each(module, chain, version, platform, cells, Some(reader)))

  private def each(
      module: Module,
      chain: List[Module],
      version: ScalaVersion,
      platform: Platform,
      cells: Map[Cell, Path],
      reader: Option[TastyCompat.Reader],
  ): ZIO[Any, GraphCompileError, List[UpstreamCell]] =
    ZIO.foreach(chain): dependency =>
      one(module, dependency, version, platform, cells, reader).flatMap: picked =>
        cells.get((dependency.name, picked, platform)) match
          case Some(dir) => ZIO.succeed(UpstreamCell(dependency, picked, dir))
          case None      => ZIO.fail(missing(module, dependency, version, platform))

  private def one(
      module: Module,
      dependency: Module,
      version: ScalaVersion,
      platform: Platform,
      cells: Map[Cell, Path],
      reader: Option[TastyCompat.Reader],
  ): ZIO[Any, GraphCompileError, ScalaVersion] =
    required(version, dependency, platform, cells.keySet) match
      case Nil                                      => ZIO.fail(missing(module, dependency, version, platform))
      case (_, exact, _) :: Nil if exact == version => ZIO.succeed(exact)
      case (_, patch, _) :: Nil if is213(version)   => ZIO.succeed(patch)
      case many if isScala3(version)                =>
        reader match
          case Some(open) => readable(module, dependency, version, platform, cells, many, open)
          case None       =>
            ZIO.fail(
              GraphCompileError
                .Failed(module.name, version, platform, CompileError.Toolchain(ToolchainMiss.Unexpected(version, Nil)))
            )
      case _ => ZIO.fail(missing(module, dependency, version, platform))

  private def readable(
      module: Module,
      dependency: Module,
      version: ScalaVersion,
      platform: Platform,
      cells: Map[Cell, Path],
      candidates: List[Cell],
      reader: TastyCompat.Reader,
  ): ZIO[Any, GraphCompileError, ScalaVersion] =
    val failed = (error: CompileError) => GraphCompileError.Failed(module.name, version, platform, error)
    ZIO
      .foreach(candidates):
        case (_, upstream, _) =>
          cells.get((dependency.name, upstream, platform)) match
            case None      => ZIO.fail(missing(module, dependency, version, platform))
            case Some(dir) => TastyCompat.header(dir).mapError(failed).map(header => (upstream, header))
      .flatMap: judged =>
        val present = judged.collect:
          case (upstream, Some(numbers)) => (upstream, numbers)
        if present.isEmpty then
          ZIO.fromOption(newestAtMost(version, candidates)).orElseFail(missing(module, dependency, version, platform))
        else
          ZIO
            .foreach(present): (upstream, numbers) =>
              ZIO.fromEither(reader.accepts(numbers)).mapError(failed).map(ok => (upstream, numbers, ok))
            .flatMap: decided =>
              val accepted = decided.collect:
                case (upstream, numbers, true) => (upstream, numbers)
              accepted.sortBy((upstream, numbers) => (numbers.minor, rank(upstream))).lastOption match
                case Some((upstream, _)) => ZIO.succeed(upstream)
                case None                =>
                  decided.sortBy((upstream, numbers, _) => (numbers.minor, rank(upstream))).lastOption match
                    case Some((upstream, _, _)) =>
                      ZIO.fail(
                        GraphCompileError
                          .IncompatibleUpstream(module.name, dependency.name, version, upstream, platform)
                      )
                    case None => ZIO.fail(missing(module, dependency, version, platform))

  private def missing(
      module: Module,
      dependency: Module,
      version: ScalaVersion,
      platform: Platform,
  ): GraphCompileError =
    GraphCompileError.MissingUpstream(module.name, dependency.name, version, platform)

  private def atMost(version: ScalaVersion, limit: ScalaVersion): Boolean =
    val (major, minor, patch)                = rank(version)
    val (limitMajor, limitMinor, limitPatch) = rank(limit)
    major < limitMajor || (major == limitMajor && (minor < limitMinor || (minor == limitMinor && patch <= limitPatch)))
end UpstreamCells
