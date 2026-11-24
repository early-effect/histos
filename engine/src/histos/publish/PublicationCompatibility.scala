package histos.publish

import histos.{Module, Organization, Platform, ProjectGraph, ScalaVersion}
import histos.central.{ArtifactId, ArtifactIdError, FeedError, MavenModule, ReleaseFeed}
import histos.doctor.{
  BaselineError,
  BinaryCheck,
  CompatibilityAnalyzer,
  CompatibilityBaseline,
  CompatibilityError,
  CompatibilityEvidence,
  CompatibilityIncomplete,
  CompatibilityTarget,
  CompatibilityVerification,
}
import histos.resolve.ArtifactName
import histos.version.Ship
import histos.zinc.CompiledCell
import zio.{IO, Layer, ZIO, ZLayer}

/** The complete ship is checked before any artifact is handed to a publisher. */
final class PublicationCompatibility(verification: CompatibilityVerification):
  def check(
      graph: ProjectGraph,
      ships: List[Ship],
      group: Organization,
      cells: List[CompiledCell],
  ): IO[LocalPublishError, Unit] =
    ZIO
      .foreach(PublicationCompatibility.targets(graph, ships)): (ship, member, version) =>
        val artifact                                          = ArtifactName.module(member, version, Platform.Jvm)
        val coordinate                                        = PublicationCoordinate(group, artifact)
        def failure(outcome: PublicationCompatibilityOutcome) =
          PublicationCompatibilityFailure(ship.name, ship.release, member.name, version, coordinate, outcome)
        for
          id <- ZIO
            .fromEither(ArtifactId.from(artifact))
            .mapError(_ => LocalPublishError.BadDependency(group.value, artifact, ship.release.render))
          cell <- ZIO
            .fromOption(
              cells.find(cell => cell.module == member.name && cell.version == version && cell.platform == Platform.Jvm)
            )
            .orElseFail(LocalPublishError.Compile(histos.zinc.GraphCompileError.UnknownCell(member.name, version)))
          target = CompatibilityTarget(member.name, version, MavenModule(group, id), cell.classes)
          evidence <- verification.verify(target)
        yield evidence match
          case CompatibilityEvidence.FirstPublication   => None
          case CompatibilityEvidence.Incomplete(reason) =>
            Some(failure(PublicationCompatibilityOutcome.Incomplete(PublicationCompatibility.gap(reason))))
          case CompatibilityEvidence.Compared(previous, result) =>
            BinaryCheck.difference(ship.release, result) match
              case BinaryCheck.Difference.None                                                               => None
              case BinaryCheck.Difference.Break(part, _) if BinaryCheck.covers(previous, ship.release, part) => None
              case BinaryCheck.Difference.Break(part, count)                                                 =>
                Some(
                  failure(PublicationCompatibilityOutcome.Policy(previous, part, count))
                )
      .flatMap: results =>
        results.flatten match
          case Nil          => ZIO.unit
          case head :: tail => ZIO.fail(LocalPublishError.Compatibility(::(head, tail)))

object PublicationCompatibility:
  val layer: ZLayer[CompatibilityVerification, Nothing, PublicationCompatibility] =
    ZLayer.fromFunction(PublicationCompatibility(_))

  val live: Layer[CompatibilityError, PublicationCompatibility] =
    (ReleaseFeed.live ++ CompatibilityBaseline.live ++ CompatibilityAnalyzer.jvm) >>> CompatibilityVerification.layer >>> layer

  private[publish] def verify(
      graph: ProjectGraph,
      ships: List[Ship],
      group: Organization,
      cells: List[CompiledCell],
      compatibility: Layer[CompatibilityError, PublicationCompatibility],
  ): IO[LocalPublishError, Unit] =
    targets(graph, ships) match
      case Nil          => ZIO.unit
      case head :: tail =>
        def failed(target: (Ship, Module, ScalaVersion), error: CompatibilityError): PublicationCompatibilityFailure =
          val (ship, member, version) = target
          PublicationCompatibilityFailure(
            ship.name,
            ship.release,
            member.name,
            version,
            PublicationCoordinate(group, ArtifactName.module(member, version, Platform.Jvm)),
            PublicationCompatibilityOutcome.Incomplete(gap(CompatibilityIncomplete.AnalysisFailed(error))),
          )
        ZIO
          .serviceWithZIO[PublicationCompatibility](_.check(graph, ships, group, cells))
          .provideLayer(compatibility)
          .mapError:
            case error: LocalPublishError  => error
            case error: CompatibilityError =>
              LocalPublishError.Compatibility(::(failed(head, error), tail.map(failed(_, error))))

  private def targets(graph: ProjectGraph, ships: List[Ship]): List[(Ship, Module, ScalaVersion)] =
    ships.flatMap: ship =>
      ship.members
        .flatMap(name => graph.get(name).collect { case member: Module => member })
        .flatMap: member =>
          if !member.platforms.all.contains(Platform.Jvm) then Nil
          else member.versions.all.map(version => (ship, member, version))

  private def gap(reason: CompatibilityIncomplete): PublicationCompatibilityGap = reason match
    case CompatibilityIncomplete.MissingOrganization(member) => PublicationCompatibilityGap.MissingOrganization(member)
    case CompatibilityIncomplete.InvalidArtifact(member, ArtifactIdError.Bad(raw)) =>
      PublicationCompatibilityGap.InvalidArtifact(member, raw)
    case CompatibilityIncomplete.MissingCurrent(path)       => PublicationCompatibilityGap.MissingCurrent(path)
    case CompatibilityIncomplete.ProductReadFailed(path, _) => PublicationCompatibilityGap.ProductRead(path)
    case CompatibilityIncomplete.ListingFailed(FeedError.Unreachable(url)) =>
      PublicationCompatibilityGap.ListingUnavailable(url)
    case CompatibilityIncomplete.ListingFailed(FeedError.Status(url, code)) =>
      PublicationCompatibilityGap.ListingStatus(url, code)
    case CompatibilityIncomplete.BaselineFailed(BaselineError.Unavailable(coordinate, _)) =>
      PublicationCompatibilityGap.BaselineUnavailable(coordinate.revision)
    case CompatibilityIncomplete.BaselineFailed(BaselineError.MissingJar(coordinate)) =>
      PublicationCompatibilityGap.BaselineMissing(coordinate.revision)
    case CompatibilityIncomplete.AnalysisFailed(error) =>
      error match
        case CompatibilityError.InitializationFailed(_)     => PublicationCompatibilityGap.AnalyzerInitialization
        case CompatibilityError.UnsupportedBackend(backend) => PublicationCompatibilityGap.UnsupportedBackend(backend)
        case CompatibilityError.MissingPrevious(path)       => PublicationCompatibilityGap.MissingPrevious(path)
        case CompatibilityError.InvalidPrevious(path)       => PublicationCompatibilityGap.InvalidPrevious(path)
        case CompatibilityError.MissingCurrent(path)        => PublicationCompatibilityGap.MissingCurrent(path)
        case CompatibilityError.InvalidBaselineEntry(path, entry) =>
          PublicationCompatibilityGap.InvalidBaselineEntry(path, entry)
        case CompatibilityError.PreparationFailed(input, error) =>
          PublicationCompatibilityGap.Preparation(input.previous, input.current, error)
        case CompatibilityError.AnalysisFailed(input, _) =>
          PublicationCompatibilityGap.Analysis(input.previous, input.current)
end PublicationCompatibility
