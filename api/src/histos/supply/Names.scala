package histos.supply

import _root_.scala.annotation.publicInBinary

import java.time.ZoneId

/** An IANA zone. The literal embeds the id, so native code never opens a tzdb. */
opaque type TimeZone = String

object TimeZone:
  given CanEqual[TimeZone, TimeZone] = CanEqual.derived

  inline def apply(inline raw: String): TimeZone = ${ SupplyMacros.timeZone('raw) }

  def from(raw: String): Either[TimeZoneError, TimeZone] =
    try
      ZoneId.of(raw)
      Right(trusted(raw))
    catch case _: java.time.DateTimeException | _: java.time.zone.ZoneRulesException => Left(TimeZoneError.Unknown(raw))

  extension (zone: TimeZone) def value: String = zone

  @publicInBinary private[histos] def trusted(raw: String): TimeZone = raw
end TimeZone

enum TimeZoneError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a time zone"
end TimeZoneError

/** A git ref that `git check-ref-format --branch` would accept. */
opaque type BranchName = String

object BranchName:
  given CanEqual[BranchName, BranchName] = CanEqual.derived

  inline def apply(inline raw: String): BranchName = ${ SupplyMacros.branch('raw) }

  def from(raw: String): Either[BranchNameError, BranchName] =
    if valid(raw) then Right(trusted(raw)) else Left(BranchNameError.Bad(raw))

  extension (name: BranchName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): BranchName = raw

  private def valid(raw: String): Boolean =
    raw.nonEmpty &&
      !raw.endsWith("/") &&
      !raw.endsWith(".") &&
      !raw.startsWith("/") &&
      !raw.contains("..") &&
      !raw.contains("@{") &&
      !raw.contains("//") &&
      raw != "@" &&
      raw.split("/", -1).toList.forall(component)

  private def component(raw: String): Boolean =
    raw.nonEmpty &&
      !raw.endsWith(".lock") &&
      !raw.startsWith(".") &&
      raw.forall(char =>
        char > ' ' && char != '~' && char != '^' && char != ':' && char != '?' && char != '*' &&
          char != '[' && char != '\\' && char != 127.toChar
      )
end BranchName

enum BranchNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a branch name"
end BranchNameError

/** A calendar day count. Zero is allowed. */
opaque type Days = Int

object Days:
  given CanEqual[Days, Days] = CanEqual.derived

  inline def apply(inline raw: Int): Days = ${ SupplyMacros.days('raw) }

  def from(raw: Int): Either[SpanError, Days] =
    if raw < 0 then Left(SpanError.Negative(raw)) else Right(trusted(raw))

  extension (days: Days) def value: Int = days

  @publicInBinary private[histos] def trusted(raw: Int): Days = raw
end Days

/** An hour count. Zero is allowed. */
opaque type Hours = Int

object Hours:
  given CanEqual[Hours, Hours] = CanEqual.derived

  inline def apply(inline raw: Int): Hours = ${ SupplyMacros.hours('raw) }

  def from(raw: Int): Either[SpanError, Hours] =
    if raw < 0 then Left(SpanError.Negative(raw)) else Right(trusted(raw))

  extension (hours: Hours) def value: Int = hours

  @publicInBinary private[histos] def trusted(raw: Int): Hours = raw
end Hours

enum SpanError:
  case Negative(raw: Int)

  def message: String = this match
    case Negative(raw) => s"$raw is negative"
end SpanError

/** `GHSA-xxxx-xxxx-xxxx`. */
opaque type AdvisoryId = String

object AdvisoryId:
  given CanEqual[AdvisoryId, AdvisoryId] = CanEqual.derived

  inline def apply(inline raw: String): AdvisoryId = ${ SupplyMacros.ghsa('raw) }

  def from(raw: String): Either[AdvisoryError, AdvisoryId] =
    if valid(raw) then Right(trusted(raw)) else Left(AdvisoryError.Bad(raw))

  extension (id: AdvisoryId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): AdvisoryId = raw

  private def valid(raw: String): Boolean =
    val parts = raw.split("-", -1).toList
    parts match
      case "GHSA" :: a :: b :: c :: Nil =>
        List(a, b, c).forall(part =>
          part.length == 4 && part.forall(char => char.isDigit || (char >= 'a' && char <= 'z'))
        )
      case _ => false
end AdvisoryId

enum AdvisoryError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a GHSA id"
end AdvisoryError

/** A non-empty reason with no control characters. */
opaque type Reason = String

object Reason:
  given CanEqual[Reason, Reason] = CanEqual.derived

  inline def apply(inline raw: String): Reason = ${ SupplyMacros.reason('raw) }

  def from(raw: String): Either[ReasonError, Reason] =
    if raw.nonEmpty && !raw.exists(_.isControl) then Right(trusted(raw)) else Left(ReasonError.Bad)

  extension (reason: Reason) def value: String = reason

  @publicInBinary private[histos] def trusted(raw: String): Reason = raw
end Reason

enum ReasonError:
  case Bad

  def message: String = this match
    case Bad => "a reason is one non-empty line"
end ReasonError

/** `YYYY-MM-DD`, a day on the Gregorian calendar. */
opaque type Date = String

object Date:
  given CanEqual[Date, Date] = CanEqual.derived

  private val Day = """(\d{4})-(\d{2})-(\d{2})""".r

  inline def apply(inline raw: String): Date = ${ SupplyMacros.date('raw) }

  def from(raw: String): Either[DateError, Date] =
    val real = raw match
      case Day(yearText, monthText, dayText) =>
        for
          year  <- yearText.toIntOption
          month <- monthText.toIntOption
          day   <- dayText.toIntOption
        yield month >= 1 && month <= 12 && day >= 1 && day <= days(year, month)
      case _ => None
    if real.contains(true) then Right(trusted(raw)) else Left(DateError.Bad(raw))

  private def days(year: Int, month: Int): Int =
    month match
      case 2              => if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) then 29 else 28
      case 4 | 6 | 9 | 11 => 30
      case _              => 31

  extension (date: Date) def value: String = date

  @publicInBinary private[histos] def trusted(raw: String): Date = raw
end Date

enum DateError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a date"
end DateError

/** A Maven coordinate without a version. */
opaque type Coordinate = String

object Coordinate:
  given CanEqual[Coordinate, Coordinate] = CanEqual.derived

  def from(raw: String): Either[CoordinateError, Coordinate] =
    val parts = raw.split(":").toList
    parts match
      case group :: artifact :: Nil if group.nonEmpty && artifact.nonEmpty => Right(trusted(raw))
      case _                                                               => Left(CoordinateError.Bad(raw))

  extension (coordinate: Coordinate) def value: String = coordinate

  @publicInBinary private[histos] def trusted(raw: String): Coordinate = raw
end Coordinate

enum CoordinateError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a group:artifact coordinate"
end CoordinateError

/** `owner/name`. */
opaque type RepoName = String

object RepoName:
  given CanEqual[RepoName, RepoName] = CanEqual.derived

  def from(raw: String): Either[RepoNameError, RepoName] =
    val parts = raw.split("/", -1).toList
    parts match
      case owner :: name :: Nil if owner.nonEmpty && name.nonEmpty && !name.contains("/") => Right(trusted(raw))
      case _ => Left(RepoNameError.Bad(raw))

  extension (name: RepoName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): RepoName = raw
end RepoName

enum RepoNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an owner/name repository"
end RepoNameError

/** `pkg:maven/group/artifact@version`. */
opaque type Purl = String

object Purl:
  given CanEqual[Purl, Purl] = CanEqual.derived

  def from(raw: String): Either[PurlError, Purl] =
    if raw.startsWith("pkg:") && raw.contains("@") then Right(trusted(raw)) else Left(PurlError.Bad(raw))

  extension (purl: Purl) def value: String = purl

  @publicInBinary private[histos] def trusted(raw: String): Purl = raw
end Purl

enum PurlError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a package url"
end PurlError

/** A CVSS 3.1 vector, or the fact that none was published. */
opaque type Cvss = String

object Cvss:
  given CanEqual[Cvss, Cvss] = CanEqual.derived

  def from(raw: String): Either[CvssError, Cvss] =
    if raw.startsWith("CVSS:3.1/") then Right(trusted(raw)) else Left(CvssError.Bad(raw))

  extension (vector: Cvss)
    def value: String = vector

    /** Base score band used when GHSA published no severity word. */
    def band: Severity = Cvss.bandOf(vector)
  end extension

  @publicInBinary private[histos] def trusted(raw: String): Cvss = raw

  private def bandOf(vector: String): Severity =
    val score = base(vector)
    if score >= 9.0 then Severity.Critical
    else if score >= 7.0 then Severity.High
    else if score >= 4.0 then Severity.Moderate
    else if score >= 0.1 then Severity.Low
    else Severity.Unknown

  /** A small subset of the CVSS 3.1 base score, enough to place the published vectors used in tests. */
  private def base(vector: String): Double =
    val metrics = vector
      .stripPrefix("CVSS:3.1/")
      .split("/")
      .flatMap { part =>
        part.split(":", 2) match
          case Array(key, value) => Some(key -> value)
          case _                 => None
      }
      .toMap
    val av = metrics.get("AV") match
      case Some("N") => 0.85
      case Some("A") => 0.62
      case Some("L") => 0.55
      case Some("P") => 0.2
      case _         => 0.85
    val ac = metrics.get("AC") match
      case Some("L") => 0.77
      case Some("H") => 0.44
      case _         => 0.77
    val pr = (metrics.get("PR"), metrics.get("S")) match
      case (Some("N"), _)         => 0.85
      case (Some("L"), Some("C")) => 0.68
      case (Some("L"), _)         => 0.62
      case (Some("H"), Some("C")) => 0.50
      case (Some("H"), _)         => 0.27
      case _                      => 0.85
    val ui = metrics.get("UI") match
      case Some("N") => 0.85
      case Some("R") => 0.62
      case _         => 0.85
    val confidentiality = metrics.get("C").fold(0.0)(cia)
    val integrity       = metrics.get("I").fold(0.0)(cia)
    val availability    = metrics.get("A").fold(0.0)(cia)
    val iss             = 1 - (1 - confidentiality) * (1 - integrity) * (1 - availability)
    val exploit         = 8.22 * av * ac * pr * ui
    val scopeChanged    = metrics.get("S").contains("C")
    val raw             =
      if scopeChanged then 1.08 * (exploit + 6.42 * iss - 0.029) - 0.02 * math.pow(exploit * (6.42 * iss), 0.5)
      else exploit + 6.42 * iss
    math.ceil(raw * 10 - 1e-9) / 10

  private def cia(value: String): Double = value match
    case "H" => 0.56
    case "L" => 0.22
    case _   => 0.0
end Cvss

enum CvssError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a CVSS 3.1 vector"
end CvssError

enum Severity derives CanEqual:
  case Low, Moderate, High, Critical, Unknown

  def atLeast(floor: Severity): Boolean = rank >= floor.rank

  def rank: Int = this match
    case Unknown  => 0
    case Low      => 1
    case Moderate => 2
    case High     => 3
    case Critical => 4

  /** The word a finding prints. [[Severity.fromWord]] reads it back. */
  def word: String = this match
    case Unknown  => "Unknown"
    case Low      => "Low"
    case Moderate => "Moderate"
    case High     => "High"
    case Critical => "Critical"
end Severity

object Severity:
  def fromWord(raw: String): Severity = raw.toLowerCase match
    case "low"      => Low
    case "moderate" => Moderate
    case "medium"   => Moderate
    case "high"     => High
    case "critical" => Critical
    case _          => Unknown

/** One dependency the lock names. */
final case class LockedDependency(
    coordinate: Coordinate,
    version: String,
    purl: Purl,
    direct: Boolean,
    testOnly: Boolean,
    cell: String,
)

/** A candidate release. `security` is set when an advisory names this version as a fix. */
final case class Release(
    coordinate: Coordinate,
    version: String,
    published: Date,
    security: Option[AdvisoryId],
    preRelease: Boolean,
)

/** What a bump would change. */
final case class ChosenBump(
    kind: BumpKind,
    from: LockedDependency,
    to: Release,
    forced: Boolean,
)

/** A workflow event the outage policy keys on. */
enum Event derives CanEqual:
  case PullRequest, Push, Schedule

/** The GitHub App that opens dependency pull requests. */
opaque type AppSlug = String

object AppSlug:
  given CanEqual[AppSlug, AppSlug] = CanEqual.derived

  val histosDeps: AppSlug = "histos-deps"

  extension (slug: AppSlug) def value: String = slug
end AppSlug

/** A GitHub label. */
opaque type LabelName = String

object LabelName:
  given CanEqual[LabelName, LabelName] = CanEqual.derived

  inline def apply(inline raw: String): LabelName = ${ SupplyMacros.label('raw) }

  def from(raw: String): Either[LabelNameError, LabelName] =
    if raw.nonEmpty && !raw.exists(_.isWhitespace) && raw.length <= 50 then Right(trusted(raw))
    else Left(LabelNameError.Bad(raw))

  extension (name: LabelName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): LabelName = raw
end LabelName

enum LabelNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a label"
end LabelNameError

/** An advisory source name. */
opaque type SourceName = String

object SourceName:
  given CanEqual[SourceName, SourceName] = CanEqual.derived

  def from(raw: String): Either[SourceNameError, SourceName] =
    if raw.nonEmpty && raw.forall(char => char.isLetterOrDigit || char == '-' || char == '.') then Right(trusted(raw))
    else Left(SourceNameError.Bad(raw))

  extension (name: SourceName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): SourceName = raw
end SourceName

enum SourceNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a source name"
end SourceNameError

opaque type PersonName = String

object PersonName:
  given CanEqual[PersonName, PersonName] = CanEqual.derived

  inline def apply(inline raw: String): PersonName = ${ SupplyMacros.person('raw) }

  def from(raw: String): Either[PersonNameError, PersonName] =
    if raw.nonEmpty && !raw.exists(_.isControl) then Right(trusted(raw)) else Left(PersonNameError.Bad(raw))

  extension (name: PersonName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): PersonName = raw
end PersonName

enum PersonNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a name"
end PersonNameError

opaque type Email = String

object Email:
  given CanEqual[Email, Email] = CanEqual.derived

  inline def apply(inline raw: String): Email = ${ SupplyMacros.email('raw) }

  def from(raw: String): Either[EmailError, Email] =
    val at = raw.indexOf('@')
    if at > 0 && at < raw.length - 1 && !raw.exists(_.isWhitespace) then Right(trusted(raw))
    else Left(EmailError.Bad(raw))

  extension (email: Email) def value: String = email

  @publicInBinary private[histos] def trusted(raw: String): Email = raw
end Email

enum EmailError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an email"
end EmailError

opaque type CellName = String

object CellName:
  given CanEqual[CellName, CellName] = CanEqual.derived

  def from(raw: String): Either[CellNameError, CellName] =
    if raw.nonEmpty && !raw.exists(_.isWhitespace) then Right(trusted(raw)) else Left(CellNameError.Bad(raw))

  extension (name: CellName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): CellName = raw
end CellName

enum CellNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a cell"
end CellNameError

opaque type RepoId = String

object RepoId:
  given CanEqual[RepoId, RepoId] = CanEqual.derived

  def from(raw: String): Either[RepoIdError, RepoId] =
    if raw.nonEmpty && !raw.exists(_.isWhitespace) then Right(trusted(raw)) else Left(RepoIdError.Bad(raw))

  extension (id: RepoId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): RepoId = raw
end RepoId

enum RepoIdError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a repository id"
end RepoIdError

/** `cron` plus `timezone`, rendered together. */
final case class Schedule(cron: histos.plugin.Cron, zone: TimeZone)

/** One advisory record. */
final case class Finding(
    id: AdvisoryId,
    severity: Severity,
    cvss: Option[Cvss],
    affected: List[Purl],
    fixedIn: List[String],
    summary: String,
    modified: String,
)
