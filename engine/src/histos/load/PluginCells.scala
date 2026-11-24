package histos.load

import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{DeclaredTask, PluginDetail, PluginRunError, TaskDep, SourceSet}
import histos.workspace.Workspace
import histos.{JsOptimize, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

/** Selects scoped compile dependencies and effective link modes for runtime products and dependency hashing. */
private[load] object PluginCells:
  final case class Cell(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      scope: SourceSet,
  )

  final case class Link(cell: Cell, optimize: JsOptimize)

  def scopeToken(scope: SourceSet): String = scope match
    case SourceSet.Main => "main"
    case SourceSet.Test => "test"

  def selected(
      graph: ProjectGraph,
      task: DeclaredTask,
  ): Either[PluginRunError, List[Cell]] =
    task.dependsOn
      .foldLeft[Either[PluginRunError, List[Cell]]](Right(Nil)):
        case (Left(error), _)                                       => Left(error)
        case (Right(acc), TaskDep.Compile(name, version, platform)) =>
          named(graph, name, version, platform, SourceSet.Test).map(acc ++ _)
        case (Right(acc), TaskDep.CompileScoped(name, version, platform, scope)) =>
          named(graph, name, version, platform, scope).map(acc ++ _)
        case (Right(acc), TaskDep.Link(_, _, _))             => Right(acc)
        case (Right(acc), TaskDep.Same(_))                   => Right(acc)
        case (Right(acc), TaskDep.Generate(_))               => Right(acc)
        case (Right(acc), TaskDep.Emit(_))                   => Right(acc)
        case (Right(acc), TaskDep.EmitScoped(_, _, _, _, _)) => Right(acc)
      .map: found =>
        found.distinct.filterNot: cell =>
          cell.scope == SourceSet.Main && found.exists(other => sameCell(cell, other) && other.scope == SourceSet.Test)

  def links(
      graph: ProjectGraph,
      task: DeclaredTask,
  ): Either[PluginRunError, List[Link]] =
    task.dependsOn.foldLeft[Either[PluginRunError, List[Link]]](Right(Nil)):
      case (Left(error), _)                                    => Left(error)
      case (Right(acc), TaskDep.Link(name, version, optimize)) =>
        named(graph, name, version, Some(Platform.Js), SourceSet.Main).map: found =>
          acc ++ found.map(cell =>
            Link(cell, JsOptimize.effective(graph.tools.js.optimize, cell.module.jsOptimize, optimize))
          )
      case (Right(acc), TaskDep.Compile(_, _, _))          => Right(acc)
      case (Right(acc), TaskDep.CompileScoped(_, _, _, _)) => Right(acc)
      case (Right(acc), TaskDep.Same(_))                   => Right(acc)
      case (Right(acc), TaskDep.Generate(_))               => Right(acc)
      case (Right(acc), TaskDep.Emit(_))                   => Right(acc)
      case (Right(acc), TaskDep.EmitScoped(_, _, _, _, _)) => Right(acc)

  private def sameCell(left: Cell, right: Cell): Boolean =
    left.module.name == right.module.name && left.version == right.version && left.platform == right.platform

  private def named(
      graph: ProjectGraph,
      name: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform],
      scope: SourceSet,
  ): Either[PluginRunError, List[Cell]] =
    module(graph, name) match
      case None        => Left(failed(s"module ${name.value} is not in the graph"))
      case Some(found) =>
        cells(found, version, platform, scope) match
          case Nil  => Left(failed(s"'${name.value}' does not build the requested cell"))
          case some => Right(some)

  def scriptDigest(repo: Path, request: Link): IO[PluginRunError, Sha256] =
    scriptPath(repo, request).flatMap: script =>
      ZIO
        .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(script))))
        .orElseFail(failed(s"could not read $script"))

  private def scriptPath(repo: Path, request: Link): IO[PluginRunError, Path] =
    val Link(cell, optimize) = request
    Workspace
      .readString(scriptRecord(repo, cell.module.name, cell.version, optimize))
      .mapError(_ => failed(s"link of ${cell.module.name.value} ${cell.version.value} was not written"))
      .flatMap(text => ZIO.attempt(Path.of(text.trim)).orElseFail(failed("invalid linked script path")))

  private def scriptRecord(repo: Path, module: ProjectName, version: ScalaVersion, optimize: JsOptimize): Path =
    repo
      .resolve(".histos")
      .resolve("link")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(s"script-${optimize.token}")

  private def cells(
      module: Module,
      version: Option[ScalaVersion],
      platform: Option[Platform],
      scope: SourceSet,
  ): List[Cell] =
    val versions =
      version.fold(module.versions.all)(one => if module.versions.all.contains(one) then List(one) else Nil)
    val platforms =
      platform.fold(module.platforms.all)(one => if module.platforms.all.contains(one) then List(one) else Nil)
    versions
      .flatMap(item => platforms.map(target => Cell(module, item, target, scope)))
      .sortBy(cell => (cell.version.value, cell.platform.token))

  private def module(graph: ProjectGraph, name: ProjectName): Option[Module] =
    graph.projects.collectFirst:
      case found: Module if found.name == name => found

  private def failed(message: String): PluginRunError =
    PluginRunError.Failed(PluginDetail.trusted(message))
end PluginCells
