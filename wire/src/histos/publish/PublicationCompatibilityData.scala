package histos.publish

import histos.{Organization, ProjectName, ScalaVersion}
import histos.version.{BumpPart, ShipName, ShipNumber}
import java.nio.file.Path

final case class PublicationCoordinate(group: Organization, artifact: String)

enum PublicationCompatibilityGap:
  case MissingOrganization(member: ProjectName)
  case InvalidArtifact(member: ProjectName, raw: String)
  case MissingCurrent(path: Path)
  case ProductRead(path: Path)
  case ListingUnavailable(url: String)
  case ListingStatus(url: String, code: Int)
  case BaselineUnavailable(previous: ShipNumber)
  case BaselineMissing(previous: ShipNumber)
  case AnalyzerInitialization
  case UnsupportedBackend(backend: histos.Platform)
  case MissingPrevious(path: Path)
  case InvalidPrevious(path: Path)
  case InvalidBaselineEntry(path: Path, entry: String)
  case Preparation(previous: Path, current: Path, error: histos.workspace.IoFailure)
  case Analysis(previous: Path, current: Path)

enum PublicationCompatibilityOutcome:
  case Incomplete(reason: PublicationCompatibilityGap, baseline: Option[ShipNumber])
  case Policy(previous: ShipNumber, required: BumpPart, problems: Int)

final case class PublicationCompatibilityFailure(
    ship: ShipName,
    number: ShipNumber,
    member: ProjectName,
    version: ScalaVersion,
    coordinate: PublicationCoordinate,
    outcome: PublicationCompatibilityOutcome,
):
  def message: String =
    s"${ship.value} ${number.render}: ${coordinate.group.value}:${coordinate.artifact} (${member.value}, Scala ${version.value}): $outcome"
