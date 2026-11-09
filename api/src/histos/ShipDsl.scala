package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import histos.version.{CommitSha, ReleaseVersion, Revision, ShipName, ShipNumber}

/** A ship named at the call site, before its next release is chosen. */
final class ShipStart private (val name: ShipName):
  /** The next release. Components are non-negative integer literals. */
  inline def next(inline major: Int, inline minor: Int, inline patch: Int): ShipDraft =
    ${ ShipDslMacro.next('this, 'major, 'minor, 'patch) }

object ShipStart:
  @publicInBinary private[histos] def trusted(name: ShipName): ShipStart =
    new ShipStart(name)

/** The next release of one ship. Apply it to the modules that publish that number. */
final class ShipDraft private (val name: ShipName, val number: ShipNumber):
  inline def apply(member: ProjectDef, members: ProjectDef*): ShipDef =
    ${ ShipDslMacro.row('this, 'member, 'members) }

  /** `<number>-SNAPSHOT`. The moving pointer. Not a release. */
  def pointer: PointerPin = PointerPin.trusted(number)

  /** `<number>-<12 hex>-SNAPSHOT`. The stored snapshot of one commit. Not a release. */
  inline def commit(inline sha: String): CommitPin =
    ${ ShipDslMacro.commit('this, 'sha) }

  /** The bare next release, `<number>`. */
  def released: ReleasePin = ReleasePin.trusted(number)
end ShipDraft

object ShipDraft:
  @publicInBinary private[histos] def trusted(name: ShipName, number: ShipNumber): ShipDraft =
    new ShipDraft(name, number)

/** `0.2.0-SNAPSHOT`. Publish release does not accept this type. */
final class PointerPin private (val number: ShipNumber):
  def revision: String = s"${number.render}-SNAPSHOT"

  def dep(organization: Organization, artifact: ArtifactBase): Dep =
    ShipDep.coordinate(organization, artifact, revision)
end PointerPin

object PointerPin:
  @publicInBinary private[histos] def trusted(number: ShipNumber): PointerPin =
    new PointerPin(number)

/** `0.2.0-<sha>-SNAPSHOT`. Publish release does not accept this type. */
final class CommitPin private (val number: ShipNumber, val sha: CommitSha):
  def revision: String = s"${number.render}-${sha.abbreviate.value}-SNAPSHOT"

  def dep(organization: Organization, artifact: ArtifactBase): Dep =
    ShipDep.coordinate(organization, artifact, revision)
end CommitPin

object CommitPin:
  @publicInBinary private[histos] def trusted(number: ShipNumber, sha: CommitSha): CommitPin =
    new CommitPin(number, sha)

/** The bare ship number. A snapshot pin is a different type. */
final class ReleasePin private (val number: ShipNumber):
  def revision: String = number.render

  def dep(organization: Organization, artifact: ArtifactBase): Dep =
    ShipDep.coordinate(organization, artifact, revision)

  def revisionValue: Revision = Revision.Release(ReleaseVersion(number))
end ReleasePin

object ReleasePin:
  @publicInBinary private[histos] def trusted(number: ShipNumber): ReleasePin =
    new ReleasePin(number)

/** `Ship("histos").next(0, 2, 2)(api, app)` is one catalog row. The number is the next release. */
object Ship:
  inline def apply(inline name: String): ShipStart =
    ${ ShipDslMacro.start('name) }

private object ShipDep:
  private[histos] def coordinate(organization: Organization, artifact: ArtifactBase, revision: String): Dep =
    Dep.trusted(organization.value, artifact.value, revision, DepKind.Binary)

private object ShipDslMacro:
  def start(raw: Expr[String])(using Quotes): Expr[ShipStart] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort("Ship(...) takes a literal name. Write Ship(\"histos\").", raw)
      case Some(text) =>
        ShipName.from(text) match
          case Left(_) =>
            report.errorAndAbort(nameMessage(text), raw)
          case Right(_) =>
            '{ ShipStart.trusted(ShipName.trusted(${ Expr(text) })) }

  def next(
      start: Expr[ShipStart],
      major: Expr[Int],
      minor: Expr[Int],
      patch: Expr[Int],
  )(using Quotes): Expr[ShipDraft] =
    import quotes.reflect.report
    (major.value, minor.value, patch.value) match
      case (Some(majorValue), Some(minorValue), Some(patchValue)) =>
        badComponent(major, "major", majorValue)
          .orElse(badComponent(minor, "minor", minorValue))
          .orElse(badComponent(patch, "patch", patchValue)) match
          case Some(found) => found
          case None        =>
            '{
              ShipDraft.trusted(
                $start.name,
                ShipNumber.trusted(${ Expr(majorValue) }, ${ Expr(minorValue) }, ${ Expr(patchValue) }),
              )
            }
      case _ =>
        report.errorAndAbort(
          "next(major, minor, patch) takes integer literals. Write next(0, 2, 0).",
          major,
        )

  def commit(draft: Expr[ShipDraft], raw: Expr[String])(using Quotes): Expr[CommitPin] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort(
          "commit(...) takes a literal sha. Write commit(\"0123456789abcdef0123456789abcdef01234567\").",
          raw,
        )
      case Some(text) =>
        CommitSha.from(text) match
          case Left(_) =>
            report.errorAndAbort(commitMessage(text), raw)
          case Right(_) =>
            '{ CommitPin.trusted($draft.number, CommitSha.trusted(${ Expr(text.toLowerCase) })) }

  def row(
      draft: Expr[ShipDraft],
      member: Expr[ProjectDef],
      members: Expr[Seq[ProjectDef]],
  )(using Quotes): Expr[ShipDef] =
    import quotes.reflect.Position
    val at  = Position.ofMacroExpansion
    val pos =
      '{ SourcePos(${ Expr(at.sourceFile.path) }, ${ Expr(at.startLine + 1) }, ${ Expr(at.startColumn + 1) }) }
    '{ ShipDef.line($draft.name.value, $draft.number, ::($member, $members.toList), $pos) }

  private def badComponent(using Quotes)(expr: Expr[Int], name: String, value: Int): Option[Expr[ShipDraft]] =
    if value < 0 then
      import quotes.reflect.report
      Some(
        report.errorAndAbort(
          s"$name is $value. next takes three non-negative integers. Write next(0, 2, 0).",
          expr,
        )
      )
    else None

  private def nameMessage(raw: String): String =
    if raw.isEmpty then "Ship(\"\") is not a ship name. A ship name starts with a letter. Write Ship(\"histos\")."
    else
      s"Ship(\"$raw\") is not a ship name. A ship name starts with a letter, then letters, digits, '_', or '-'. Write Ship(\"histos\")."

  private def commitMessage(raw: String): String =
    val shown = if raw.length > 48 then raw.take(48) + "..." else raw
    s"commit(\"$shown\") is not a 40-hex commit. Pass the full commit, for example commit(\"0123456789abcdef0123456789abcdef01234567\")."
end ShipDslMacro
