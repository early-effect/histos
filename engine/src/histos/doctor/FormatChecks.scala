package histos.doctor

import histos.format.{FormatError, ScalafmtTools, ScalafmtVersion}
import histos.resolve.{LockedTools, Resolve, ResolveError}
import histos.{Formatting, ProjectGraph, Resolution}

import java.nio.file.Path

import zio.{UIO, ZIO}

/** The scalafmt configuration a formatting build declares, and whether a locked build pins the core it names. */
object FormatChecks:
  val configId: CheckId = CheckId.trusted("format.config")
  val lockId: CheckId   = CheckId.trusted("format.lock")

  def checks(repo: Path, graph: ProjectGraph): List[DoctorCheck] =
    List(check(configId)(config(repo, graph)), check(lockId)(lock(repo, graph)))

  def config(repo: Path, graph: ProjectGraph): UIO[List[Outcome]] =
    graph.formatting match
      case Formatting.Off            => ZIO.succeed(List(Outcome.Ok(configId, "the build does not format")))
      case Formatting.Scalafmt(conf) =>
        ScalafmtVersion
          .of(repo, conf)
          .fold(
            error => List(Outcome.Found(FormatError.Conf(error).diagnose)),
            version => List(Outcome.Ok(configId, s"${conf.relative} names scalafmt ${version.value}")),
          )

  /** A configuration problem is `format.config`'s finding, so this check stays quiet about it. */
  def lock(repo: Path, graph: ProjectGraph): UIO[List[Outcome]] =
    ScalafmtTools
      .requests(repo, graph)
      .foldZIO(
        _ => ZIO.succeed(List(Outcome.Ok(lockId, "format.config names the configuration problem"))),
        {
          case None           => ZIO.succeed(List(Outcome.Ok(lockId, "the build does not format")))
          case Some(requests) =>
            graph.resolution match
              case Resolution.Solve => ZIO.succeed(List(Outcome.Ok(lockId, "the build solves its tools")))
              case Resolution.Lock  =>
                Resolve
                  .readLock(repo)
                  .fold(error => List(Outcome.Found(error.diagnose)), held => pinned(requests, held))
        },
      )

  private def pinned(requests: histos.format.ScalafmtRequests, held: histos.resolve.DependencyLock): List[Outcome] =
    val faults = List(requests.dynamic, requests.core).flatMap: request =>
      held.tools.find(_.id == request.id) match
        case None       => List(ResolveError.NoTool(request.id))
        case Some(cell) =>
          LockedTools.validate(request, cell).left.map(_ => ResolveError.ToolDrift(request.id)).left.toSeq
    faults match
      case Nil   => List(Outcome.Ok(lockId, "the lock pins the scalafmt the configuration names"))
      case found => found.map(error => Outcome.Found(error.diagnose))

  private def check(checkId: CheckId)(body: UIO[List[Outcome]]): DoctorCheck =
    new DoctorCheck:
      def id                      = checkId
      def area                    = Area.BuildDefinition
      def needs                   = Set(Probe.Local)
      def run(ctx: DoctorContext) = body
end FormatChecks
