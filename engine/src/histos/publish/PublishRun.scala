package histos.publish

import histos.jdk.JdkRuntime
import histos.zinc.{CompileFeed}
import histos.ProjectGraph

import java.nio.file.Path

import zio.{UIO, ZIO}

/** One publish, either into a directory or up to Central snapshots. */
object PublishRun:
  final class Report(val where: PublishWhere, val output: PublishOutput):
    def lines: List[String] =
      val root = where match
        case PublishWhere.Directory(path) => Some(path)
        case PublishWhere.Snapshots(_, _) => None
      output.skipped ++ output.modules.map(_.rendered(root))

  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      repository: String,
      report: CompileFeed => UIO[Unit] = _ => ZIO.unit
  ): ZIO[histos.resolve.Resolve, LocalPublishError, Report] =
    ZIO
      .fromEither(PublishWhere.resolve(repo, repository, env))
      .flatMap: where =>
        val body =
          for
            _ <- ZIO.fromEither(LauncherPublish.ready(repo, graph))
            written <- LocalPublish.run(repo, graph, names, jdk, env, report)
            done <- LauncherPublish.upload(repo, graph, written)
          yield Report(where, done)
        body.provideSome[histos.resolve.Resolve](
          where.layer.mapError(LocalPublishError.Publish(_))
        )
