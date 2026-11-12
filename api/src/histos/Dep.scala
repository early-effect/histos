package histos

import histos.doctor.{Diagnostic, Noted}

import _root_.scala.annotation.publicInBinary

/** How a library dependency is cross-versioned. `Java` is one colon, `Binary` is `::`, `Full` is `:::`. */
enum DepKind:
  case Java, Binary, Full

/** `Published` is a pom dependency with no scope element. `CompileOnly` is on the compile classpath and absent from the
  * pom. `Provided` is on the compile classpath, off a JVM run and a fat jar, and written as `<scope>provided</scope>`.
  */
enum DepLink:
  case Published, CompileOnly, Provided

/** One POM exclusion. `artifact` is the published artifact id, with any Scala suffix already written. */
final case class DepExclusion(organization: Organization, artifact: ArtifactBase)

/** A library coordinate. `Kind` is the separator the literal used. `List[Dep]` stays wide. */
sealed trait Dep:
  type Kind <: DepKind
  def kind: Kind
  def organization: String
  def name: String
  def revision: LibraryRevision
  def platforms: Option[List[Platform]]
  def link: DepLink
  def exclusions: List[DepExclusion]
  def sbtSeries: Option[SbtSeries]

  /** Another artifact at this organization, kind, and revision. Platforms, link, exclusions, and sbt series reset. */
  inline def module(inline name: String): Dep { type Kind = Dep.this.Kind } =
    ${ DepMacro.module[Kind]('this, 'name) }

  /** Spliced by `module`. The name was already checked as a literal. */
  private[histos] def member(next: String): Dep { type Kind = Dep.this.Kind }

  /** Drop a transitive by its published organization and artifact id. */
  def excluding(organization: Organization, artifact: ArtifactBase): Dep { type Kind = Dep.this.Kind }

  /** This dependency is resolved only on the named platforms. */
  def on(platform: Platform, rest: Platform*): Dep { type Kind = Dep.this.Kind }

  /** Compile against this coordinate. The published pom does not fetch it. */
  def compileOnly: Dep { type Kind = Dep.this.Kind }

  /** Compile and test against this coordinate. The pom writes scope `provided`, and a JVM run or a fat jar omits it. */
  def provided: Dep { type Kind = Dep.this.Kind }

  def applies(platform: Platform): Boolean =
    platforms match
      case None         => true
      case Some(chosen) => chosen.contains(platform)
end Dep

object Dep:
  given CanEqual[Dep, Dep] = CanEqual.derived

  /** A literal (`Dep("dev.zio::zio:2.1.26")`). A bad shape is a compile error. */
  transparent inline def apply(inline raw: String): Dep = ${ DepMacro.literal('raw) }

  def from(raw: String): Either[DepError, Dep] =
    if raw.isEmpty then Left(DepError.Empty)
    else if raw.exists(_.isWhitespace) then Left(DepError.BadShape(raw))
    else parse(raw)

  def select(deps: List[Dep], platform: Platform): List[Dep] =
    deps.filter(_.applies(platform))

  /** Coordinates a JVM run and a fat jar fetch. A provided edge stays on the compile classpath and off this list. */
  def runtime(deps: List[Dep], platform: Platform): List[Dep] =
    select(deps, platform).filter(_.link != DepLink.Provided)

  /** Coordinates the pom lists. Compile-only edges stay on the compile classpath and off this list. */
  def declared(deps: List[Dep], platform: Platform): List[Dep] =
    select(deps, platform).filter(dep => dep.link == DepLink.Published || dep.link == DepLink.Provided)

  /** Spliced by the literal macro. A range is still a value here so resolve can refuse one that was built past `from`.
    */
  @publicInBinary private[histos] def trusted(
      organization: String,
      name: String,
      revision: String,
      kind: DepKind,
  ): Dep =
    LibraryRevision.from(revision) match
      case Right(parsed) => of(organization, name, parsed, kind)
      case Left(_)       => of(organization, name, LibraryRevision.trusted(revision), kind)

  private[histos] def java(
      organization: String,
      name: String,
      revision: LibraryRevision,
      platforms: Option[List[Platform]] = None,
      link: DepLink = DepLink.Published,
      exclusions: List[DepExclusion] = Nil,
  ): Dep { type Kind = DepKind.Java.type } =
    Made(DepKind.Java, organization, name, revision, platforms, link, store(exclusions), None)

  private[histos] def binary(
      organization: String,
      name: String,
      revision: LibraryRevision,
      platforms: Option[List[Platform]] = None,
      link: DepLink = DepLink.Published,
      exclusions: List[DepExclusion] = Nil,
      sbtSeries: Option[SbtSeries] = None,
  ): Dep { type Kind = DepKind.Binary.type } =
    Made(DepKind.Binary, organization, name, revision, platforms, link, store(exclusions), sbtSeries)

  private[histos] def full(
      organization: String,
      name: String,
      revision: LibraryRevision,
      platforms: Option[List[Platform]] = None,
      link: DepLink = DepLink.Published,
      exclusions: List[DepExclusion] = Nil,
  ): Dep { type Kind = DepKind.Full.type } =
    Made(DepKind.Full, organization, name, revision, platforms, link, store(exclusions), None)

  private def of(organization: String, name: String, revision: LibraryRevision, kind: DepKind): Dep =
    kind match
      case DepKind.Java   => java(organization, name, revision)
      case DepKind.Binary => binary(organization, name, revision)
      case DepKind.Full   => full(organization, name, revision)

  private def parse(raw: String): Either[DepError, Dep] =
    if raw.contains(":::") then cross(raw, ":::", DepKind.Full)
    else if raw.contains("::") then cross(raw, "::", DepKind.Binary)
    else
      raw.split(":", -1) match
        case Array(org, name, revision) => coordinate(org, name, revision, DepKind.Java, raw)
        case _                          => Left(DepError.BadShape(raw))

  private def cross(raw: String, sep: String, kind: DepKind): Either[DepError, Dep] =
    raw.split(sep, -1) match
      case Array(org, rest) =>
        rest.split(":", -1) match
          case Array(name, revision) => coordinate(org, name, revision, kind, raw)
          case _                     => Left(DepError.BadShape(raw))
      case _ => Left(DepError.BadShape(raw))

  private def coordinate(
      organization: String,
      name: String,
      revision: String,
      kind: DepKind,
      raw: String,
  ): Either[DepError, Dep] =
    if organization.isEmpty || name.isEmpty || revision.isEmpty then Left(DepError.BadShape(raw))
    else
      LibraryRevision.from(revision) match
        case Left(LibraryRevisionError.Ranged(_)) => Left(DepError.Ranged(raw))
        case Left(_)                              => Left(DepError.BadShape(raw))
        case Right(parsed)                        => Right(of(organization, name, parsed, kind))

  private def store(exclusions: List[DepExclusion]): List[DepExclusion] =
    exclusions
      .distinctBy(item => (item.organization, item.artifact))
      .sortBy(item => (item.organization.value, item.artifact.value))

  private final class Made[K <: DepKind](
      val kind: K,
      val organization: String,
      val name: String,
      val revision: LibraryRevision,
      val platforms: Option[List[Platform]],
      val link: DepLink,
      val exclusions: List[DepExclusion],
      val sbtSeries: Option[SbtSeries],
  ) extends Dep:
    type Kind = K

    def excluding(organization: Organization, artifact: ArtifactBase): Dep { type Kind = K } =
      copy(exclusions = store(exclusions :+ DepExclusion(organization, artifact)))

    def on(platform: Platform, rest: Platform*): Dep { type Kind = K } =
      copy(platforms = Some((platform :: rest.toList).distinct))

    def compileOnly: Dep { type Kind = K } =
      copy(link = DepLink.CompileOnly)

    def provided: Dep { type Kind = K } =
      copy(link = DepLink.Provided)

    def member(next: String): Dep { type Kind = K } =
      Made(kind, organization, next, revision, None, DepLink.Published, Nil, None)

    private def copy(
        platforms: Option[List[Platform]] = platforms,
        link: DepLink = link,
        exclusions: List[DepExclusion] = exclusions,
        sbtSeries: Option[SbtSeries] = sbtSeries,
    ): Made[K] =
      Made(kind, organization, name, revision, platforms, link, exclusions, sbtSeries)

    override def equals(other: Any): Boolean =
      other match
        case that: Made[?] =>
          kind == that.kind && organization == that.organization && name == that.name &&
          revision == that.revision && platforms == that.platforms && link == that.link &&
          exclusions == that.exclusions && sbtSeries == that.sbtSeries
        case _ => false

    override def hashCode: Int =
      (kind, organization, name, revision, platforms, link, exclusions, sbtSeries).##
  end Made

  /** sbt plugin cross. In the companion so it does not clash with the `sbt"..."` literal. The artifact id stays the
    * base name. Resolve appends `_sbtN` and the module's Scala suffix.
    */
  extension (dep: Dep { type Kind = DepKind.Binary.type })
    def sbt(series: SbtSeries): Dep { type Kind = DepKind.Binary.type } =
      binary(dep.organization, dep.name, dep.revision, dep.platforms, dep.link, dep.exclusions, Some(series))
end Dep

enum DepError:
  case Empty
  case BadShape(raw: String)
  case Ranged(raw: String)

  def message: String = this match
    case Empty         => "a dependency coordinate is not empty"
    case BadShape(raw) =>
      val base = s"'$raw' is not an organization:name:revision coordinate"
      hint(raw).fold(base)(line => s"$base\n$line")
    case Ranged(raw) => s"'$raw' is not a single revision"

  def diagnose: Diagnostic = this match
    case Empty =>
      Noted.carry("dep.empty", message, "A coordinate names an organization, a name, and a revision.", "histos help")
    case BadShape(raw) =>
      val summary = s"'$raw' is not an organization:name:revision coordinate"
      Noted.carry(
        "dep.shape",
        summary,
        hint(raw).getOrElse(
          "Use organization:name:revision, organization::name:revision, or organization:::name:revision."
        ),
        "histos help",
      )
    case Ranged(raw) =>
      Noted.carry(
        "dep.ranged",
        s"'$raw' is not a single revision",
        "A revision is one version. latest, a plus, a comma, or a bracket is a range.",
        "histos help",
      )

  private def hint(raw: String): Option[String] =
    val colons = raw.count(_ == ':')
    if raw.contains(":::") then Some("A full cross-version is organization:::name:revision.")
    else if raw.contains("::") then
      Some("A Scala binary coordinate is organization::name:revision. Three colons is organization:::name:revision.")
    else if colons == 2 then
      val swapped = raw.replaceFirst(":", "::")
      Some(s"':' is a Java coordinate. A Scala library uses '::', for example $swapped.")
    else if colons == 1 then
      Some("A Java coordinate is organization:name:revision. A Scala library is organization::name:revision.")
    else Some("Use organization:name:revision, organization::name:revision, or organization:::name:revision.")
end DepError
