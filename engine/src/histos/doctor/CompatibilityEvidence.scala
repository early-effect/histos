package histos.doctor

import histos.central.{ArtifactIdError, FeedError, MavenModule, ReleaseFeed}
import histos.version.ShipNumber
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{UIO, ZIO, ZLayer}

final case class CompatibilityTarget(member: ProjectName, version: ScalaVersion, module: MavenModule, classes: Path):
  def label: String = s"${module.group.value}:${module.artifact.value} (${member.value}, Scala ${version.value})"

enum CompatibilityIncomplete:
  case MissingOrganization(member: ProjectName)
  case InvalidArtifact(member: ProjectName, error: ArtifactIdError)
  case MissingCurrent(path: Path)
  case ProductReadFailed(path: Path, cause: Throwable)
  case ListingFailed(error: FeedError)
  case BaselineFailed(error: BaselineError)
  case AnalysisFailed(error: CompatibilityError)

enum CompatibilityEvidence:
  case FirstPublication
  case Compared(previous: ShipNumber, result: CompatibilityResult)
  case Incomplete(reason: CompatibilityIncomplete)

/** Collects evidence for an exact coordinate, without deciding whether the catalog covers a break. */
final class CompatibilityVerification(
    feed: ReleaseFeed,
    baseline: CompatibilityBaseline,
    analyzer: CompatibilityAnalyzer,
):
  def verify(target: CompatibilityTarget): UIO[CompatibilityEvidence] =
    ZIO
      .attemptBlocking(Files.isDirectory(target.classes))
      .mapError(CompatibilityIncomplete.ProductReadFailed(target.classes, _))
      .flatMap: present =>
        if !present then ZIO.fail(CompatibilityIncomplete.MissingCurrent(target.classes))
        else
          feed
            .newest(target.module)
            .mapError(CompatibilityIncomplete.ListingFailed(_))
            .flatMap:
              case None           => ZIO.succeed(CompatibilityEvidence.FirstPublication)
              case Some(previous) =>
                baseline
                  .fetch(BaselineCoordinate(target.module, previous))
                  .mapError(CompatibilityIncomplete.BaselineFailed(_))
                  .flatMap: jar =>
                    analyzer
                      .analyze(CompatibilityInput(Platform.Jvm, jar, target.classes))
                      .mapError(CompatibilityIncomplete.AnalysisFailed(_))
                      .map(CompatibilityEvidence.Compared(previous, _))
      .fold(CompatibilityEvidence.Incomplete(_), identity)

object CompatibilityVerification:
  val layer: ZLayer[ReleaseFeed & CompatibilityBaseline & CompatibilityAnalyzer, Nothing, CompatibilityVerification] =
    ZLayer.fromFunction(CompatibilityVerification(_, _, _))

/** Incomplete evidence fails doctor and its existing CI/preflight wiring, rather than becoming a skipped green check.
  */
final case class IncompleteCompatibility(label: String, reason: CompatibilityIncomplete) extends Diagnostic:
  def check   = ShipChecks.binaryId
  def level   = Level.Error
  def title   = "JVM compatibility verification is incomplete"
  def summary = s"$label: $reason"
  def explain = "Every published JVM member and Scala version needs current classes and its own released baseline."
  def fixes   = ::(
    Fix.Steps(
      ::("Compile all published JVM members, restore registry access, then run histos doctor --area release.", Nil)
    ),
    Nil,
  )
