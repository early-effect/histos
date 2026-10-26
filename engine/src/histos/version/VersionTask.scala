package histos.version

import histos.task.{TaskError, TaskResult}
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{ConfigProvider, IO, ZIO}

/** The `versions` task. The registry file is a stand-in for the registry read Central will own. A missing file is
  * [[Evidence.Unchecked]], not an empty registry.
  */
object VersionTask:
  val fileName: String = "releases"

  def decide(
      graph: ProjectGraph,
      repo: Path,
      env: Map[String, String],
  ): IO[VersionError, Decision] =
    evidence(graph, repo).flatMap: found =>
      load(env).flatMap: source =>
        ZIO.fromEither(Versions.assess(graph.catalog, source, found))

  def fromRepo(
      graph: ProjectGraph,
      repo: Path,
      env: Map[String, String],
  ): IO[TaskError, TaskResult] =
    decide(graph, repo, env).mapError(TaskError.Version(_)).flatMap(complete)

  def effect(
      catalog: Catalog,
      env: Map[String, String],
      evidence: Evidence,
  ): IO[TaskError, TaskResult] =
    load(env)
      .mapError(TaskError.Version(_))
      .flatMap: source =>
        ZIO
          .fromEither(Versions.assess(catalog, source, evidence))
          .mapError(TaskError.Version(_))
          .flatMap(complete)

  def path(repo: Path): Path =
    repo.resolve(".histos").resolve(fileName)

  private def complete(decision: Decision): IO[TaskError, TaskResult] =
    decision match
      case refused: Decision.Refused =>
        ZIO.fail(TaskError.Version(VersionError.Refused(refused.refusals)))
      case other => ZIO.succeed(TaskResult.Versions(other))

  private def load(env: Map[String, String]): IO[VersionError, SessionSource] =
    ConfigProvider.fromMap(env).load(SessionSource.config).mapError(VersionError.Config(_))

  private def evidence(
      graph: ProjectGraph,
      repo: Path,
  ): IO[VersionError, Evidence] =
    graph.catalog match
      case Catalog.Empty | Catalog.Manual(_) => ZIO.succeed(Evidence.Unchecked)
      case Catalog.Lines(_)                  => read(repo)

  private def read(repo: Path): IO[VersionError, Evidence] =
    val file = path(repo)
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(file) then Some(Files.readAllLines(file).asScala.toList)
        else None
      }
      .mapError(_ => VersionError.Unreadable)
      .flatMap:
        case None        => ZIO.succeed(Evidence.Unchecked)
        case Some(lines) =>
          ZIO
            .fromEither(Registry.parse(lines).map(Evidence.Checked(_)))
            .mapError(VersionError.Registry(_))
end VersionTask
