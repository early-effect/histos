package histos.preview

import histos.select.Selection
import histos.toolchain.VersionOrder
import histos.workflow.CiError
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, Sources}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Why a compile unit is in the plan. A module outside the closure is
  * [[PlanUnit.NotSelected]], and the printed plan omits it.
  */
enum Reason:
  case Selected
  case Upstream

  def word: String = this match
    case Selected => "selected"
    case Upstream => "upstream"

/** Zinc reports invalid sources from `compile`, which writes class files and
  * `inc.zip`. This plan does not call that, so file-level staleness stays
  * unknown.
  */
enum FileStaleness:
  case Unknown

enum PlanUnit:
  case Chosen(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      reason: Reason,
      upstreams: List[ProjectName],
      sources: List[Path]
  )
  case NotSelected(module: ProjectName)

final case class CompilePlan(units: List[PlanUnit], staleness: FileStaleness)

enum PreviewError:
  case Target(error: CiError)
  case Cycle(module: ProjectName)
  case Sources(failure: IoFailure)

  def message: String = this match
    case Target(error)    => error.message
    case Cycle(module)    => s"'${module.value}' is in a compile cycle"
    case Sources(failure) => failure.message

object Plan:
  def show(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String]
  ): IO[PreviewError, String] =
    compile(repo, graph, projects).map(render(repo, _))

  def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String]
  ): IO[PreviewError, CompilePlan] =
    val modules = graph.projects.collect { case module: Module => module }
    for
      selected <- ZIO
        .fromEither(Selection.select(graph, projects))
        .mapError(PreviewError.Target(_))
      roots = selected.modules.map(_.name).toSet
      closure = Module.closure(modules, roots)
      _ <- ZIO.fromEither(acyclic(closure)).mapError(PreviewError.Cycle(_))
      bases <- ZIO.foreach(closure): module =>
        GraphCompile
          .sourceBase(repo, module, modules)
          .mapBoth(rootFault(repo, _), base => module.name -> base)
      listed <- ZIO.foreach(modules): module =>
        cells(module, closure, roots, bases.toMap)
    yield CompilePlan(listed.flatten, FileStaleness.Unknown)

  def render(repo: Path, plan: CompilePlan): String =
    val body = plan.units.flatMap:
      case chosen: PlanUnit.Chosen => renderChosen(repo, chosen)
      case PlanUnit.NotSelected(_) => Nil
    (stalenessLine(plan.staleness) :: body).mkString("\n")

  private def acyclic(modules: List[Module]): Either[ProjectName, Unit] =
    val names = modules.map(_.name).toSet
    def step(
        done: Set[ProjectName],
        rest: List[Module]
    ): Either[ProjectName, Unit] =
      val (ready, later) = rest.partition: module =>
        module.upstreamNames.forall: dependency =>
          !names.contains(dependency) || done.contains(dependency)
      ready match
        case Nil =>
          later match
            case module :: _ => Left(module.name)
            case Nil         => Right(())
        case _ =>
          step(done ++ ready.map(_.name), later)
    step(Set.empty, modules)

  private def cells(
      module: Module,
      closure: List[Module],
      selected: Set[ProjectName],
      bases: Map[ProjectName, Option[Path]]
  ): IO[PreviewError, List[PlanUnit]] =
    if !closure.exists(_.name == module.name) then
      ZIO.succeed(List(PlanUnit.NotSelected(module.name)))
    else
      val reason =
        if selected.contains(module.name) then Reason.Selected
        else Reason.Upstream
      val versions =
        module.versions.all.sortWith: (left, right) =>
          VersionOrder.compare(left.value, right.value) < 0
      val platforms = List(Platform.Jvm, Platform.Js, Platform.Native).filter(
        module.platforms.all.contains
      )
      ZIO
        .foreach(versions): version =>
          ZIO.foreach(platforms): platform =>
            val upstreams = Module.chain(module, closure, platform).map(_.name)
            bases.get(module.name) match
              case Some(Some(root)) =>
                Sources
                  .select(root, version, platform)
                  .mapBoth(
                    sourceFault(root, _),
                    paths =>
                      PlanUnit.Chosen(
                        module.name,
                        version,
                        platform,
                        reason,
                        upstreams,
                        paths
                      )
                  )
              case Some(None) | None =>
                ZIO.succeed(
                  PlanUnit.Chosen(
                    module.name,
                    version,
                    platform,
                    reason,
                    upstreams,
                    Nil
                  )
                )
        .map(_.flatten)

  private def renderChosen(repo: Path, unit: PlanUnit.Chosen): List[String] =
    val names = unit.upstreams.map(_.value).mkString(" ")
    val upstreams = if names.isEmpty then "upstreams" else s"upstreams $names"
    val sources =
      if unit.sources.isEmpty then List("sources")
      else unit.sources.map(path => s"sources ${relative(repo, path)}")
    List(
      s"${unit.module.value} ${unit.version.value} ${unit.platform.token} ${unit.reason.word}",
      upstreams
    ) ++ sources

  private def stalenessLine(staleness: FileStaleness): String =
    staleness match
      case FileStaleness.Unknown => "file-level staleness: unknown"

  private def relative(repo: Path, path: Path): String =
    val root = repo.toAbsolutePath.normalize
    val file = path.toAbsolutePath.normalize
    val shown = if file.startsWith(root) then root.relativize(file) else file
    shown.iterator.asScala.map(_.toString).mkString("/")

  private def rootFault(repo: Path, error: GraphCompileError): PreviewError =
    error match
      case GraphCompileError.RootRead(path) =>
        PreviewError.Sources(IoFailure(path, IoFault.Stat))
      case GraphCompileError.Runtime(_) | GraphCompileError.Target(_) |
          GraphCompileError.UnknownCell(_, _) | GraphCompileError.NoRoot(_) |
          GraphCompileError.NoSources(_, _) |
          GraphCompileError.MissingUpstream(_, _, _, _) |
          GraphCompileError.Cycle(_) | GraphCompileError.Lock(_) |
          GraphCompileError.LockRead(_) | GraphCompileError.Output(_) |
          GraphCompileError.Failed(_, _, _, _) | GraphCompileError.Cells(_) =>
        PreviewError.Sources(IoFailure(repo, IoFault.Stat))

  private def sourceFault(root: Path, error: CompileError): PreviewError =
    error match
      case CompileError.Io(failure) => PreviewError.Sources(failure)
      case CompileError.Resolved(_) | CompileError.UnsupportedPlatform(_) |
          CompileError.UnsupportedScala(_) | CompileError.NoSources |
          CompileError.Banned(_, _) | CompileError.Failed(_) |
          CompileError.Toolchain(_) | CompileError.JsLibrary(_) |
          CompileError.NativeFetch(_) | CompileError.NativeOrigin(_) |
          CompileError.NativeLibrary(_) =>
        PreviewError.Sources(IoFailure(root, IoFault.Walk))
