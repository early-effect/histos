package histos

import _root_.scala.annotation.publicInBinary

/** What one module publishes. A module has one. Unspecified, it is a library. */
enum Publication:
  case Library

  /** The library packer under the sbt artifact id. The series is the binary series, `2` or `1.0`. */
  case SbtPlugin(series: SbtSeries)

  /** One JVM fat jar. No Scala suffix, and the pom lists no dependencies. */
  case Assembly(main: MainClass)

  /** The thin jar. The pom lists the classpath. */
  case Application
end Publication

/** The sbt binary series carried in the artifact id. sbt 2 is `2`, so a Scala 3 jar is `name_sbt2_3`. */
opaque type SbtSeries = String

object SbtSeries:
  given CanEqual[SbtSeries, SbtSeries] = CanEqual.derived

  private val Series = """[0-9]+(\.[0-9]+)*""".r

  inline def apply(inline raw: String): SbtSeries = ${ SbtSeriesMacro.literal('raw) }

  def from(raw: String): Either[SbtSeriesError, SbtSeries] =
    if Series.matches(raw) then Right(trusted(raw)) else Left(SbtSeriesError.Bad(raw))

  extension (series: SbtSeries) def value: String = series

  @publicInBinary private[histos] def trusted(raw: String): SbtSeries = raw
end SbtSeries

enum SbtSeriesError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an sbt series"
end SbtSeriesError

/** One line of pom text. Not empty, and no control characters. */
opaque type PomText = String

object PomText:
  given CanEqual[PomText, PomText] = CanEqual.derived

  inline def apply(inline raw: String): PomText = ${ PomTextMacro.literal('raw) }

  def from(raw: String): Either[PomTextError, PomText] =
    if raw.isEmpty || raw.forall(_.isWhitespace) then Left(PomTextError.Empty)
    else if raw.exists(_.isControl) then Left(PomTextError.Bad(raw))
    else Right(trusted(raw))

  extension (text: PomText) def value: String = text

  @publicInBinary private[histos] def trusted(raw: String): PomText = raw
end PomText

enum PomTextError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "pom text is not empty"
    case Bad(raw) => s"'$raw' is not pom text"
end PomTextError

/** An `https` url Central can fetch. A file path is not one. */
opaque type HttpUrl = String

object HttpUrl:
  given CanEqual[HttpUrl, HttpUrl] = CanEqual.derived

  inline def apply(inline raw: String): HttpUrl = ${ HttpUrlMacro.literal('raw) }

  def from(raw: String): Either[HttpUrlError, HttpUrl] =
    if raw.startsWith("https://") && raw.length > "https://".length && !raw.exists(_.isWhitespace) then
      Right(trusted(raw))
    else Left(HttpUrlError.Bad(raw))

  extension (url: HttpUrl) def value: String = url

  @publicInBinary private[histos] def trusted(raw: String): HttpUrl = raw
end HttpUrl

enum HttpUrlError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an https url"
end HttpUrlError

/** The license name written in the pom. `Apache-2.0` is one. */
opaque type LicenseName = String

object LicenseName:
  given CanEqual[LicenseName, LicenseName] = CanEqual.derived

  private val Name = """[A-Za-z0-9][A-Za-z0-9.+_-]*""".r

  inline def apply(inline raw: String): LicenseName = ${ LicenseNameMacro.literal('raw) }

  def from(raw: String): Either[LicenseNameError, LicenseName] =
    if Name.matches(raw) then Right(trusted(raw)) else Left(LicenseNameError.Bad(raw))

  extension (name: LicenseName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): LicenseName = raw
end LicenseName

enum LicenseNameError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a license name"
end LicenseNameError

/** A developer id. A letter, then letters, digits, `.`, `_`, or `-`. */
opaque type DeveloperId = String

object DeveloperId:
  given CanEqual[DeveloperId, DeveloperId] = CanEqual.derived

  private val Name = """[A-Za-z][A-Za-z0-9._-]*""".r

  inline def apply(inline raw: String): DeveloperId = ${ DeveloperIdMacro.literal('raw) }

  def from(raw: String): Either[DeveloperIdError, DeveloperId] =
    if Name.matches(raw) then Right(trusted(raw)) else Left(DeveloperIdError.Bad(raw))

  extension (id: DeveloperId) def value: String = id

  @publicInBinary private[histos] def trusted(raw: String): DeveloperId = raw
end DeveloperId

enum DeveloperIdError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a developer id"
end DeveloperIdError

/** A Maven scm connection. It starts with `scm:`. The git remote is not read. */
opaque type ScmConnection = String

object ScmConnection:
  given CanEqual[ScmConnection, ScmConnection] = CanEqual.derived

  inline def apply(inline raw: String): ScmConnection = ${ ScmConnectionMacro.literal('raw) }

  def from(raw: String): Either[ScmConnectionError, ScmConnection] =
    if raw.startsWith("scm:") && raw.length > "scm:".length && !raw.exists(_.isWhitespace) then Right(trusted(raw))
    else Left(ScmConnectionError.Bad(raw))

  extension (connection: ScmConnection) def value: String = connection

  @publicInBinary private[histos] def trusted(raw: String): ScmConnection = raw
end ScmConnection

enum ScmConnectionError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an scm connection"
end ScmConnectionError

/** One license. The pom writes `distribution` `repo` because Central requires that. */
final case class License(name: LicenseName, url: HttpUrl)

/** One developer. The id and the name are the pom fields. */
final case class Developer(id: DeveloperId, name: PomText)

/** One scm block. The written connection is also the developer connection. */
final case class Scm(url: HttpUrl, connection: ScmConnection)

/** Facts written once for the organization. A ship may replace description and url. */
final case class OrganizationFacts(
    license: Option[License] = None,
    developer: Option[Developer] = None,
    scm: Option[Scm] = None,
    description: Option[PomText] = None,
    url: Option[HttpUrl] = None,
)

object OrganizationFacts:
  val empty: OrganizationFacts = OrganizationFacts()

/** A release pom refuses a missing one of these before anything is signed. */
enum PomFact:
  case License
  case Developer
  case Scm

  def word: String = this match
    case PomFact.License   => "a license"
    case PomFact.Developer => "a developer"
    case PomFact.Scm       => "an scm"
end PomFact
