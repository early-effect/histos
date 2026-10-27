package histos.publish

import histos.jdk.JdkRuntime
import histos.zinc.CompileFeed
import histos.ProjectGraph

import java.nio.file.Path

import zio.{IO, System, UIO, ZIO}

/** One publish, either into a directory or up to Central snapshots. */
object PublishRun:
  final class Report(val where: PublishWhere, val output: PublishOutput):
    def lines: List[String] =
      val root = where match
        case PublishWhere.Directory(path) => Some(path)
        case PublishWhere.Snapshots(_, _) => None
      output.skipped ++ output.refused ++ output.held ++ output.modules.map(_.rendered(root))

  def home: IO[LocalPublishError, Path] =
    System
      .property("user.home")
      .orElseFail(LocalPublishError.NoHome)
      .flatMap:
        case Some(dir) if dir.nonEmpty => ZIO.succeed(Path.of(dir))
        case _                         => ZIO.fail(LocalPublishError.NoHome)

  def local(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      directory: Option[String],
      report: CompileFeed => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[histos.resolve.Resolve, LocalPublishError, Report] =
    ZIO
      .fromEither(PublishSelect.resolve(graph, tokens, directory, allowDirectory = true))
      .flatMap: request =>
        val found = request.directory match
          case Some(_) => ZIO.succeed(repo)
          case None    => home
        found.flatMap: place =>
          ZIO
            .fromEither(PublishWhere.directory(repo, request.directory, place))
            .flatMap(where => go(repo, graph, request.names, jdk, env, where, PublishWrite.Local, report))

  def snapshots(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      report: CompileFeed => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[histos.resolve.Resolve, LocalPublishError, Report] =
    ZIO
      .fromEither(PublishSelect.resolve(graph, tokens, None, allowDirectory = false))
      .flatMap: request =>
        ZIO
          .fromEither(PublishWhere.snapshots(env))
          .flatMap(where => go(repo, graph, request.names, jdk, env, where, PublishWrite.Snapshots, report))

  private def go(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      jdk: JdkRuntime,
      env: Map[String, String],
      where: PublishWhere,
      write: PublishWrite,
      report: CompileFeed => UIO[Unit],
  ): ZIO[histos.resolve.Resolve, LocalPublishError, Report] =
    val body =
      LocalPublish
        .run(repo, graph, names, jdk, env, write, report)
        .map(done => Report(where, done))
    body.provideSome[histos.resolve.Resolve](
      where.layer.mapError(LocalPublishError.Publish(_))
    )
end PublishRun
