package histos.resolve

import histos.lock.{LockChange, LockDiff, LockDiffEntry, LockRevisionState, LockSubject, NecessaryScope}
import histos.plugin.{
  ClientId,
  CommandIdentity,
  CommandToken,
  ContentHash,
  Digests,
  GenerationId,
  InvocationId,
  LockRevision,
  PluginDetail,
  PluginDetailError,
  PluginTaskId,
  ProposalId,
  ResolutionIntent,
  ResolveVerb,
  Tokens,
}
import histos.{ProjectName, Resolution}

import java.nio.charset.StandardCharsets

import scala.annotation.publicInBinary
import scala.quoted.*

/** A retained mutable input. Ordinary repair keeps these bytes. Replacing one is a refresh. */
final case class SnapshotLocator(name: LocatorName, digest: ContentHash)

opaque type LocatorName = String

object LocatorName:
  given CanEqual[LocatorName, LocatorName] = CanEqual.derived

  inline def apply(inline raw: String): LocatorName = ${ LocatorLiteral('raw) }

  def from(raw: String): Either[LocatorNameError, LocatorName] =
    Tokens.parseSimple(raw, LocatorNameError.Empty, LocatorNameError.Bad(_)).map(trusted)

  extension (name: LocatorName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): LocatorName = raw
end LocatorName

enum LocatorNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a snapshot locator name is not empty"
    case Bad(raw) => s"'$raw' is not a snapshot locator name"
end LocatorNameError

private object LocatorLiteral:
  def apply(raw: Expr[String])(using Quotes): Expr[LocatorName] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "LocatorName(...) takes a literal. Use LocatorName.from at runtime.",
          raw,
        )
      case Some(value) =>
        LocatorName.from(value) match
          case Left(error) => quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_)    => '{ LocatorName.trusted(${ Expr(value) }) }

/** One versioned pin. The locator is present when the pin names a retained snapshot. */
final case class VersionedPin(
    coordinate: histos.plugin.PluginCoordinate,
    subject: LockSubject,
    digest: ContentHash,
    locator: Option[SnapshotLocator],
)

/** The lock bytes a transaction reads and writes. Order is part of the document. */
final case class LockDocument(pins: List[VersionedPin], retained: List[SnapshotLocator])

object LockDocument:
  val empty: LockDocument = LockDocument(Nil, Nil)

  def render(document: LockDocument): String =
    val lines = document.pins.map(renderPin) ++ document.retained.map(renderRetained)
    if lines.isEmpty then "" else lines.mkString("\n") + "\n"

  def renderPin(pin: VersionedPin): String =
    val head = s"pin ${pin.coordinate.render} ${LockCodec.subject(pin.subject)} ${pin.digest.value}"
    pin.locator match
      case None          => head
      case Some(locator) => s"$head at ${locator.name.value} ${locator.digest.value}"

  /** A document this parser cannot read back is not a lock we may write. */
  def roundTrip(document: LockDocument): Either[LockTextError, LockDocument] =
    parse(render(document)).flatMap: read =>
      if read == document then Right(read) else Left(LockTextError.BadLine(0))

  def revision(document: LockDocument): LockRevision =
    LockCodec.revision(render(document))

  def candidate(document: LockDocument): ContentHash =
    LockCodec.content(render(document))

  def parse(text: String): Either[LockTextError, LockDocument] =
    if text.isEmpty then Left(LockTextError.Empty)
    else parseLines(text.linesIterator.toList, 1, Acc())

  def closureDiff(
      before: LockDocument,
      after: LockDocument,
      closure: List[histos.plugin.PluginCoordinate],
  ): LockDiff =
    val named = closure.toSet
    val pins  = diffPins(
      before.pins.filter(pin => named.contains(pin.coordinate)),
      after.pins.filter(pin => named.contains(pin.coordinate)),
    )
    val retain = diffPins(retainedPins(before.retained), retainedPins(after.retained))
    LockDiff(pins ++ retain)

  def merge(previous: LockDocument, solved: Solved, refresh: Boolean): Either[MergeFault, LockDocument] =
    for retained <- retain(previous.retained, solved, refresh)
    yield LockDocument(mergePins(previous.pins, solved), retained)

  def scope(diff: LockDiff): Either[PluginDetailError, NecessaryScope] =
    summarize(diff).map(summary => NecessaryScope(summary, diff))

  def retainedPins(retained: List[SnapshotLocator]): List[VersionedPin] =
    retained.map: locator =>
      VersionedPin(
        histos.plugin.PluginCoordinate.Module(ProjectName("snapshots")),
        LockSubject.Locator,
        locator.digest,
        Some(locator),
      )

  private def summarize(diff: LockDiff): Either[PluginDetailError, PluginDetail] =
    val count = diff.entries.length
    val text  = if count == 1 then "1 lock change" else s"$count lock changes"
    PluginDetail.from(text)

  private def mergePins(previous: List[VersionedPin], solved: Solved): List[VersionedPin] =
    val closure          = solved.closure.toSet
    val solvedBy         = index(solved.pins)
    val (reversed, seen) =
      previous.foldLeft((List.empty[VersionedPin], Set.empty[PinKey])):
        case ((acc, seen), pin) =>
          val id = key(pin)
          if closure.contains(pin.coordinate) then
            solvedBy.get(id) match
              case Some(next) => (next :: acc, seen + id)
              case None       => (acc, seen + id)
          else (pin :: acc, seen + id)
    val added = solved.pins.filterNot(pin => seen.contains(key(pin)))
    reversed.reverse ++ added

  private def retain(
      retained: List[SnapshotLocator],
      solved: Solved,
      refresh: Boolean,
  ): Either[MergeFault, List[SnapshotLocator]] =
    if solved.replacedRetained.nonEmpty && !refresh then Left(MergeFault.RefreshRequired)
    else if !refresh then Right(retained)
    else
      solved.replacedRetained.foldLeft[Either[MergeFault, List[SnapshotLocator]]](Right(retained)): (acc, name) =>
        acc.flatMap(current => replaceLocator(current, name, solved.refreshed))

  private def replaceLocator(
      retained: List[SnapshotLocator],
      name: LocatorName,
      refreshed: List[SnapshotLocator],
  ): Either[MergeFault, List[SnapshotLocator]] =
    refreshed.find(_.name == name) match
      case None          => Left(MergeFault.MissingRefresh(name))
      case Some(updated) =>
        if retained.exists(_.name == name) then
          Right(retained.map(locator => if locator.name == name then updated else locator))
        else Left(MergeFault.MissingRefresh(name))

  private def diffPins(before: List[VersionedPin], after: List[VersionedPin]): List[LockDiffEntry] =
    val beforeBy = index(before)
    val changed  =
      before.flatMap: pin =>
        after.find(next => key(next) == key(pin)) match
          case None =>
            List(LockDiffEntry(pin.coordinate, LockChange.Removed, pin.subject, Some(pin.digest), None))
          case Some(next) if same(pin, next) => Nil
          case Some(next)                    =>
            val (previousHash, nextHash) = hashes(pin, next)
            List(LockDiffEntry(pin.coordinate, LockChange.Changed, pin.subject, previousHash, nextHash))
    val added =
      after
        .filterNot(pin => beforeBy.contains(key(pin)))
        .map: pin =>
          LockDiffEntry(pin.coordinate, LockChange.Added, pin.subject, None, Some(pin.digest))
    changed ++ added

  private def hashes(before: VersionedPin, after: VersionedPin): (Option[ContentHash], Option[ContentHash]) =
    if before.digest != after.digest then (Some(before.digest), Some(after.digest))
    else (before.locator.map(_.digest), after.locator.map(_.digest))

  private def same(before: VersionedPin, after: VersionedPin): Boolean =
    before.digest == after.digest && before.locator == after.locator

  private def index(pins: List[VersionedPin]): Map[PinKey, VersionedPin] =
    pins.map(pin => key(pin) -> pin).toMap

  private def key(pin: VersionedPin): PinKey =
    PinKey(pin.coordinate, pin.subject, pin.locator.map(_.name))

  private def renderRetained(locator: SnapshotLocator): String =
    s"retain ${locator.name.value} ${locator.digest.value}"

  private def parseLines(lines: List[String], number: Int, acc: Acc): Either[LockTextError, LockDocument] =
    lines match
      case Nil          => Right(LockDocument(acc.pins.reverse, acc.retained.reverse))
      case line :: rest =>
        if line.isEmpty then parseLines(rest, number + 1, acc)
        else parseLine(line, number).flatMap(next => parseLines(rest, number + 1, next(acc)))

  private def parseLine(line: String, number: Int): Either[LockTextError, Acc => Acc] =
    line.split(" ", -1).toList match
      case "pin" :: module :: subject :: digest :: Nil =>
        for
          name <- moduleName(module, number)
          kind <- LockCodec.parseSubject(subject).toRight(LockTextError.BadLine(number))
          hash <- contentAt(digest, number)
        yield acc =>
          acc.copy(pins = VersionedPin(histos.plugin.PluginCoordinate.Module(name), kind, hash, None) :: acc.pins)
      case "pin" :: module :: subject :: digest :: "at" :: locator :: locatorDigest :: Nil =>
        for
          name    <- moduleName(module, number)
          kind    <- LockCodec.parseSubject(subject).toRight(LockTextError.BadLine(number))
          hash    <- contentAt(digest, number)
          located <- locatorAt(locator, number)
          bytes   <- contentAt(locatorDigest, number)
        yield acc =>
          acc.copy(pins =
            VersionedPin(
              histos.plugin.PluginCoordinate.Module(name),
              kind,
              hash,
              Some(SnapshotLocator(located, bytes)),
            ) :: acc.pins
          )
      case "retain" :: locator :: digest :: Nil =>
        for
          located <- locatorAt(locator, number)
          hash    <- contentAt(digest, number)
        yield acc => acc.copy(retained = SnapshotLocator(located, hash) :: acc.retained)
      case _ => Left(LockTextError.BadLine(number))

  private def moduleName(raw: String, number: Int): Either[LockTextError, ProjectName] =
    ProjectName.from(raw).left.map(_ => LockTextError.BadLine(number))

  private def locatorAt(raw: String, number: Int): Either[LockTextError, LocatorName] =
    LocatorName.from(raw).left.map(_ => LockTextError.BadLine(number))

  private def contentAt(raw: String, number: Int): Either[LockTextError, ContentHash] =
    ContentHash.from(raw).left.map(_ => LockTextError.BadLine(number))

  private final case class PinKey(
      coordinate: histos.plugin.PluginCoordinate,
      subject: LockSubject,
      locator: Option[LocatorName],
  )

  private final case class Acc(pins: List[VersionedPin] = Nil, retained: List[SnapshotLocator] = Nil)
end LockDocument

/** Pins the preparer solved for the necessary closure. Retained locators are not rewritten unless named here. */
final case class Solved(
    closure: List[histos.plugin.PluginCoordinate],
    pins: List[VersionedPin],
    replacedRetained: List[LocatorName],
    refreshed: List[SnapshotLocator],
)

enum MergeFault:
  case RefreshRequired
  case MissingRefresh(name: LocatorName)

enum LockTextError:
  case Empty
  case BadLine(line: Int)
  case Bad(detail: PluginDetail)

  def message: String = this match
    case Empty         => "the lock document is empty"
    case BadLine(line) => s"line $line is not a lock record"
    case Bad(detail)   => detail.value
end LockTextError

/** Text forms shared by the lock document and the commit journal. */
private[resolve] object LockCodec:
  def content(text: String): ContentHash =
    Digests.content(text.getBytes(StandardCharsets.UTF_8))

  def revision(text: String): LockRevision =
    LockRevision.trusted(Digests.sha256(text.getBytes(StandardCharsets.UTF_8)))

  def subject(subject: LockSubject): String =
    subject match
      case LockSubject.Artifact => "artifact"
      case LockSubject.Eviction => "eviction"
      case LockSubject.Locator  => "locator"
      case LockSubject.Recipe   => "recipe"
      case LockSubject.Checksum => "checksum"

  def parseSubject(token: String): Option[LockSubject] =
    token match
      case "artifact" => Some(LockSubject.Artifact)
      case "eviction" => Some(LockSubject.Eviction)
      case "locator"  => Some(LockSubject.Locator)
      case "recipe"   => Some(LockSubject.Recipe)
      case "checksum" => Some(LockSubject.Checksum)
      case _          => None

  def renderRevision(state: LockRevisionState): String =
    state match
      case LockRevisionState.Absent            => "absent"
      case LockRevisionState.Present(revision) => s"present ${revision.value}"

  def parseRevision(raw: String): Either[LockTextError, LockRevisionState] =
    raw.split(" ", -1).toList match
      case "absent" :: Nil         => Right(LockRevisionState.Absent)
      case "present" :: hex :: Nil =>
        LockRevision.from(hex).left.map(_ => bad("bad lock revision")).map(LockRevisionState.Present(_))
      case _ => Left(bad("bad lock revision"))

  def renderIntent(intent: ResolutionIntent): String =
    intent match
      case ResolutionIntent.Inherit                  => "inherit"
      case ResolutionIntent.Select(Resolution.Solve) => "solve"
      case ResolutionIntent.Select(Resolution.Lock)  => "lock"

  def parseIntent(raw: String): Either[LockTextError, ResolutionIntent] =
    raw match
      case "inherit" => Right(ResolutionIntent.Inherit)
      case "solve"   => Right(ResolutionIntent.Select(Resolution.Solve))
      case "lock"    => Right(ResolutionIntent.Select(Resolution.Lock))
      case _         => Left(bad("bad resolution intent"))

  def renderCommand(command: CommandIdentity): String =
    command match
      case CommandIdentity.Compile(projects)  => withNames("compile", projects)
      case CommandIdentity.Test(projects)     => withNames("test", projects)
      case CommandIdentity.Resolve(action)    => s"resolve ${verb(action)}"
      case CommandIdentity.Refresh(selectors) => withNames("refresh", selectors)
      case CommandIdentity.Task(id)           => s"task ${id.render}"
      case CommandIdentity.Format             => "format"
      case CommandIdentity.Publish            => "publish"
      case CommandIdentity.Other(token)       => s"other ${token.value}"

  def parseCommand(raw: String): Either[LockTextError, CommandIdentity] =
    raw.split(" ", 2).toList match
      case "format" :: Nil            => Right(CommandIdentity.Format)
      case "publish" :: Nil           => Right(CommandIdentity.Publish)
      case "compile" :: Nil           => Right(CommandIdentity.Compile(Nil))
      case "compile" :: names :: Nil  => parseNames(names).map(CommandIdentity.Compile(_))
      case "test" :: Nil              => Right(CommandIdentity.Test(Nil))
      case "test" :: names :: Nil     => parseNames(names).map(CommandIdentity.Test(_))
      case "refresh" :: Nil           => Right(CommandIdentity.Refresh(Nil))
      case "refresh" :: names :: Nil  => parseNames(names).map(CommandIdentity.Refresh(_))
      case "resolve" :: action :: Nil => parseVerb(action).map(CommandIdentity.Resolve(_))
      case "task" :: id :: Nil        =>
        PluginTaskId.parse(id).left.map(_ => bad("bad plugin task")).map(CommandIdentity.Task(_))
      case "other" :: token :: Nil =>
        CommandToken.from(token).left.map(_ => bad("bad command token")).map(CommandIdentity.Other(_))
      case _ => Left(bad("bad command"))

  def parseHash(raw: String, detail: String): Either[LockTextError, ContentHash] =
    ContentHash.from(raw).left.map(_ => bad(detail))

  def parseProposal(raw: String): Either[LockTextError, ProposalId] =
    ProposalId.from(raw).left.map(_ => bad("bad proposal id"))

  def parseGeneration(raw: String): Either[LockTextError, GenerationId] =
    GenerationId.from(raw).left.map(_ => bad("bad generation id"))

  def parseInvocation(raw: String): Either[LockTextError, InvocationId] =
    InvocationId.from(raw).left.map(_ => bad("bad invocation id"))

  def parseClient(raw: String): Either[LockTextError, ClientId] =
    ClientId.from(raw).left.map(_ => bad("bad client id"))

  private def withNames(kind: String, names: List[ProjectName]): String =
    names.map(_.value).mkString(",") match
      case ""     => kind
      case joined => s"$kind $joined"

  private def parseNames(raw: String): Either[LockTextError, List[ProjectName]] =
    if raw.isEmpty then Right(Nil)
    else
      raw
        .split(",", -1)
        .toList
        .foldLeft[Either[LockTextError, List[ProjectName]]](Right(Nil)): (acc, name) =>
          acc.flatMap: names =>
            ProjectName.from(name).left.map(_ => bad("bad project name")).map(names :+ _)

  private def verb(action: ResolveVerb): String =
    action match
      case ResolveVerb.Verify => "verify"
      case ResolveVerb.Update => "update"
      case ResolveVerb.Check  => "check"

  private def parseVerb(raw: String): Either[LockTextError, ResolveVerb] =
    raw match
      case "verify" => Right(ResolveVerb.Verify)
      case "update" => Right(ResolveVerb.Update)
      case "check"  => Right(ResolveVerb.Check)
      case _        => Left(bad("bad resolve verb"))

  private def bad(text: String): LockTextError =
    PluginDetail.from(text) match
      case Right(detail) => LockTextError.Bad(detail)
      case Left(_)       => LockTextError.Bad(PluginDetail("bad lock text"))
end LockCodec
