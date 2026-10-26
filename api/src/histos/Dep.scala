package histos

import _root_.scala.annotation.publicInBinary

/** How a library dependency is cross-versioned. `Java` is one colon, `Binary` is `::`, `Full` is `:::`. */
enum DepKind:
  case Java, Binary, Full

/** A library coordinate written the way sbt writes it. The Scala suffix is not resolved here. */
/** `platforms` empty of a value means every platform. `Some` is only those platforms. */
final case class Dep @publicInBinary private (
    organization: String,
    name: String,
    revision: String,
    kind: DepKind,
    platforms: Option[List[Platform]] = None,
):
  /** This dependency is resolved only on the named platforms. */
  def on(platform: Platform, rest: Platform*): Dep =
    copy(platforms = Some((platform :: rest.toList).distinct))

  def applies(platform: Platform): Boolean =
    platforms match
      case None         => true
      case Some(chosen) => chosen.contains(platform)
end Dep

object Dep:
  /** A literal (`Dep("dev.zio::zio:2.1.26")`). A bad shape is a compile error. */
  inline def apply(inline raw: String): Dep = ${ DepMacro.literal('raw) }

  def from(raw: String): Either[DepError, Dep] =
    if raw.isEmpty then Left(DepError.Empty)
    else if raw.exists(_.isWhitespace) then Left(DepError.BadShape(raw))
    else parse(raw)

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

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(
      organization: String,
      name: String,
      revision: String,
      kind: DepKind,
  ): Dep =
    new Dep(organization, name, revision, kind)

  def select(deps: List[Dep], platform: Platform): List[Dep] =
    deps.filter(_.applies(platform))

  private def coordinate(
      organization: String,
      name: String,
      revision: String,
      kind: DepKind,
      raw: String,
  ): Either[DepError, Dep] =
    if organization.isEmpty || name.isEmpty || revision.isEmpty then Left(DepError.BadShape(raw))
    else Right(trusted(organization, name, revision, kind))
end Dep

enum DepError:
  case Empty
  case BadShape(raw: String)

  def message: String = this match
    case Empty         => "a dependency coordinate is not empty"
    case BadShape(raw) => s"'$raw' is not an organization:name:revision coordinate"
end DepError
