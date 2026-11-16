package histos.resolve

import histos.lock.LockRevisionState
import histos.plugin.{
  ClientId,
  CommandIdentity,
  ContentHash,
  GenerationId,
  InvocationId,
  LockRevision,
  PluginDetail,
  ProposalId,
  ResolutionIntent,
}

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}

import zio.{IO, Semaphore, UIO, ZIO}

/** A lock replace failed, or the bytes on disk are not the revision a writer expected. */
enum LockStoreFault:
  case StaleBase
  case AtomicUnavailable
  case Unreadable
  case Unwritable
  case Corrupt(detail: PluginDetail)

/** The proposal a rename is bound to. Recovery reads this after the process returns. */
final case class CommitIntent(
    proposal: ProposalId,
    base: LockRevisionState,
    next: LockRevision,
    generation: GenerationId,
    declarations: ContentHash,
    invocation: InvocationId,
    client: ClientId,
    command: CommandIdentity,
    intent: ResolutionIntent,
    candidate: ContentHash,
)

object CommitIntent:
  def render(intent: CommitIntent): String =
    List(
      s"proposal ${intent.proposal.value}",
      s"base ${LockCodec.renderRevision(intent.base)}",
      s"next ${intent.next.value}",
      s"generation ${intent.generation.value}",
      s"declarations ${intent.declarations.value}",
      s"invocation ${intent.invocation.value}",
      s"client ${intent.client.value}",
      s"command ${LockCodec.renderCommand(intent.command)}",
      s"intent ${LockCodec.renderIntent(intent.intent)}",
      s"candidate ${intent.candidate.value}",
    ).mkString("\n") + "\n"

  def parse(text: String): Either[LockTextError, CommitIntent] =
    text.linesIterator.toList
      .foldLeft[Either[LockTextError, IntentAcc]](Right(IntentAcc())): (acc, line) =>
        acc.flatMap(next => IntentAcc.step(next, line))
      .flatMap(_.finish)

  private final case class IntentAcc(
      proposal: Option[ProposalId] = None,
      base: Option[LockRevisionState] = None,
      next: Option[LockRevision] = None,
      generation: Option[GenerationId] = None,
      declarations: Option[ContentHash] = None,
      invocation: Option[InvocationId] = None,
      client: Option[ClientId] = None,
      command: Option[CommandIdentity] = None,
      intent: Option[ResolutionIntent] = None,
      candidate: Option[ContentHash] = None,
  ):
    def finish: Either[LockTextError, CommitIntent] =
      val missing = LockTextError.Bad(PluginDetail("incomplete commit journal"))
      for
        proposal     <- proposal.toRight(missing)
        base         <- base.toRight(missing)
        next         <- next.toRight(missing)
        generation   <- generation.toRight(missing)
        declarations <- declarations.toRight(missing)
        invocation   <- invocation.toRight(missing)
        client       <- client.toRight(missing)
        command      <- command.toRight(missing)
        intent       <- intent.toRight(missing)
        candidate    <- candidate.toRight(missing)
      yield CommitIntent(proposal, base, next, generation, declarations, invocation, client, command, intent, candidate)

  private object IntentAcc:

    def step(acc: IntentAcc, line: String): Either[LockTextError, IntentAcc] =
      if line.isEmpty then Right(acc)
      else
        line.split(" ", 2).toList match
          case "proposal" :: value :: Nil =>
            once(acc.proposal, LockCodec.parseProposal(value), id => acc.copy(proposal = Some(id)))
          case "base" :: value :: Nil =>
            once(acc.base, LockCodec.parseRevision(value), state => acc.copy(base = Some(state)))
          case "next" :: value :: Nil =>
            once(
              acc.next,
              LockRevision.from(value).left.map(_ => LockTextError.Bad(PluginDetail("bad lock revision"))),
              revision => acc.copy(next = Some(revision)),
            )
          case "generation" :: value :: Nil =>
            once(acc.generation, LockCodec.parseGeneration(value), id => acc.copy(generation = Some(id)))
          case "declarations" :: value :: Nil =>
            once(
              acc.declarations,
              LockCodec.parseHash(value, "bad declaration digest"),
              hash => acc.copy(declarations = Some(hash)),
            )
          case "invocation" :: value :: Nil =>
            once(acc.invocation, LockCodec.parseInvocation(value), id => acc.copy(invocation = Some(id)))
          case "client" :: value :: Nil =>
            once(acc.client, LockCodec.parseClient(value), id => acc.copy(client = Some(id)))
          case "command" :: value :: Nil =>
            once(acc.command, LockCodec.parseCommand(value), command => acc.copy(command = Some(command)))
          case "intent" :: value :: Nil =>
            once(acc.intent, LockCodec.parseIntent(value), intent => acc.copy(intent = Some(intent)))
          case "candidate" :: value :: Nil =>
            once(
              acc.candidate,
              LockCodec.parseHash(value, "bad candidate digest"),
              hash => acc.copy(candidate = Some(hash)),
            )
          case _ => Left(LockTextError.Bad(PluginDetail("bad commit journal")))

    private def once[A](
        current: Option[A],
        parsed: Either[LockTextError, A],
        set: A => IntentAcc,
    ): Either[LockTextError, IntentAcc] =
      current match
        case Some(_) => Left(LockTextError.Bad(PluginDetail("repeated commit journal field")))
        case None    => parsed.map(set)
  end IntentAcc
end CommitIntent

/** What restart can prove from the lock file and the journal. There is no rolled-back revision. */
enum StoreRecovery:
  case Idle
  case Adopted(intent: CommitIntent, document: LockDocument)
  case Retained(revision: LockRevisionState)
  case Diverged(observed: LockRevisionState)

/** The tracked lock. Rename is atomic, or the commit fails and the previous bytes stay. */
trait CommitProbe:
  def beforeRename: UIO[Unit]
  def afterRename: UIO[Unit]

object CommitProbe:
  val none: CommitProbe = new CommitProbe:
    def beforeRename: UIO[Unit] = ZIO.unit
    def afterRename: UIO[Unit]  = ZIO.unit

final class LockStore private (
    val root: Path,
    renameLock: (Path, Path) => IO[LockStoreFault, Unit],
    probe: CommitProbe,
    guard: Semaphore,
):
  def lock: Path    = root.resolve("dependencies.lock")
  def journal: Path = root.resolve("dependencies.lock.journal")

  def read: IO[LockStoreFault, LoadedLock] =
    regular(lock).flatMap: present =>
      if present then
        readText(lock).flatMap: text =>
          LockDocument.parse(text) match
            case Left(error)     => ZIO.fail(corrupt(error))
            case Right(document) =>
              ZIO.succeed(LoadedLock(LockRevisionState.Present(LockCodec.revision(text)), document))
      else ZIO.succeed(LoadedLock(LockRevisionState.Absent, LockDocument.empty))

  /** Write the temp, recheck, then rename. Interruption before rename keeps the previous file. */
  def commit(
      expected: LockRevisionState,
      document: LockDocument,
      intent: CommitIntent,
  ): IO[LockStoreFault, LockRevision] =
    val text = LockDocument.render(document)
    val next = LockDocument.revision(document)
    LockDocument.roundTrip(document) match
      case Left(_)  => ZIO.fail(LockStoreFault.Corrupt(PluginDetail("unreadable lock")))
      case Right(_) =>
        guard.withPermit:
          withFileLock:
            for
              observed <- revision
              _        <- ZIO.when(observed != expected)(ZIO.fail(LockStoreFault.StaleBase))
              _        <- ZIO.when(next != intent.next || intent.candidate.value != next.value)(
                ZIO.fail(LockStoreFault.Corrupt(PluginDetail("candidate revision mismatch")))
              )
              temp   <- AtomicReplace.writeTemp(lock, text)
              result <- commitTemp(expected, text, next, intent, temp).ensuring(AtomicReplace.delete(temp).ignore)
              _      <- probe.afterRename
            yield result

  def forget: IO[LockStoreFault, Unit] =
    AtomicReplace.delete(journal)

  def recover: IO[LockStoreFault, StoreRecovery] =
    regular(journal).flatMap: present =>
      if !present then ZIO.succeed(StoreRecovery.Idle)
      else
        readText(journal).flatMap: text =>
          CommitIntent.parse(text) match
            case Left(error)   => ZIO.fail(corrupt(error))
            case Right(intent) =>
              revision.flatMap: observed =>
                if observed == LockRevisionState.Present(intent.next) then adopt(intent)
                else if observed == intent.base then forget.as(StoreRecovery.Retained(observed))
                else ZIO.succeed(StoreRecovery.Diverged(observed))

  private def commitTemp(
      expected: LockRevisionState,
      text: String,
      next: LockRevision,
      intent: CommitIntent,
      temp: Path,
  ): IO[LockStoreFault, LockRevision] =
    for
      _     <- probe.beforeRename
      again <- revision
      _     <- ZIO.when(again != expected)(ZIO.fail(LockStoreFault.StaleBase))
      _     <- AtomicReplace.write(journal, CommitIntent.render(intent))
      _     <- ZIO.uninterruptible:
        renameLock(temp, lock).tapError(_ => forget.ignore) *> AtomicReplace.syncDirectory(root)
    yield next

  private def adopt(intent: CommitIntent): IO[LockStoreFault, StoreRecovery] =
    read.flatMap: loaded =>
      loaded.revision match
        case LockRevisionState.Present(revision) if revision == intent.next =>
          forget.as(StoreRecovery.Adopted(intent, loaded.document))
        case _ => ZIO.succeed(StoreRecovery.Diverged(loaded.revision))

  private def revision: IO[LockStoreFault, LockRevisionState] =
    read.map(_.revision)

  private def withFileLock[A](body: IO[LockStoreFault, A]): IO[LockStoreFault, A] =
    val guardPath = root.resolve("dependencies.lock.guard")
    ZIO.acquireReleaseWith(
      AtomicReplace.mkdir(root) *> ZIO
        .attemptBlocking(FileChannel.open(guardPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE))
        .mapError(_ => LockStoreFault.Unwritable)
    )(channel => ZIO.attemptBlocking(channel.close()).ignore): channel =>
      ZIO.attemptBlockingInterrupt(channel.lock()).mapError(_ => LockStoreFault.Unwritable) *> body

  private def corrupt(error: LockTextError): LockStoreFault =
    PluginDetail.from(error.message) match
      case Right(detail) => LockStoreFault.Corrupt(detail)
      case Left(_)       => LockStoreFault.Corrupt(PluginDetail("unreadable lock"))

  private def regular(path: Path): IO[LockStoreFault, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => LockStoreFault.Unreadable)

  private def readText(path: Path): IO[LockStoreFault, String] =
    ZIO.attemptBlocking(Files.readString(path, StandardCharsets.UTF_8)).mapError(_ => LockStoreFault.Unreadable)
end LockStore

final case class LoadedLock(revision: LockRevisionState, document: LockDocument)

object LockStore:
  def open(
      root: Path,
      renameLock: (Path, Path) => IO[LockStoreFault, Unit] = AtomicReplace.rename,
      probe: CommitProbe = CommitProbe.none,
  ): UIO[LockStore] =
    Semaphore.make(1).map(guard => LockStore(root, renameLock, probe, guard))

/** Same-directory replace. Atomic failure is terminal: nothing moves the bytes another way. */
private[resolve] object AtomicReplace:
  def interpret(attempted: Either[Throwable, Unit]): Either[LockStoreFault, Unit] =
    attempted match
      case Right(())                                              => Right(())
      case Left(_: java.nio.file.AtomicMoveNotSupportedException) => Left(LockStoreFault.AtomicUnavailable)
      case Left(_)                                                => Left(LockStoreFault.Unwritable)

  def write(target: Path, text: String): IO[LockStoreFault, Unit] =
    ZIO.acquireReleaseWith(writeTemp(target, text))(temp => delete(temp).ignore): temp =>
      rename(temp, target)

  def writeTemp(target: Path, text: String): IO[LockStoreFault, Path] =
    for
      parent <- parentOf(target)
      _      <- mkdir(parent)
      temp   <- createTemp(parent, target)
      _      <- writeForced(temp, text).tapError(_ => delete(temp).ignore)
      read   <- readText(temp).tapError(_ => delete(temp).ignore)
      _ <- ZIO.when(read != text)(delete(temp) *> ZIO.fail(LockStoreFault.Corrupt(PluginDetail("lock temp mismatch"))))
    yield temp

  /** Atomic replace only. A platform that cannot replace atomically fails the commit. */
  def rename(temp: Path, target: Path): IO[LockStoreFault, Unit] =
    ZIO
      .attemptBlocking:
        val attempted =
          try
            Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
            Right(())
          catch case thrown: java.io.IOException => Left(thrown)
        interpret(attempted)
      .mapError(_ => LockStoreFault.Unwritable)
      .flatMap(ZIO.fromEither)

  def delete(path: Path): IO[LockStoreFault, Unit] =
    ZIO.attemptBlocking(Files.deleteIfExists(path)).mapError(_ => LockStoreFault.Unwritable).unit

  def mkdir(path: Path): IO[LockStoreFault, Unit] =
    ZIO.attemptBlocking(Files.createDirectories(path)).mapError(_ => LockStoreFault.Unwritable).unit

  def syncDirectory(directory: Path): UIO[Unit] =
    ZIO
      .attemptBlocking:
        val channel = FileChannel.open(directory, StandardOpenOption.READ)
        try channel.force(true)
        finally channel.close()
      .ignore

  private def createTemp(parent: Path, target: Path): IO[LockStoreFault, Path] =
    ZIO
      .attemptBlocking(Files.createTempFile(parent, s".${fileName(target)}.", ".tmp"))
      .mapError(_ => LockStoreFault.Unwritable)

  private def writeForced(path: Path, text: String): IO[LockStoreFault, Unit] =
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking(
          FileChannel.open(
            path,
            StandardOpenOption.CREATE,
            StandardOpenOption.WRITE,
            StandardOpenOption.TRUNCATE_EXISTING,
          )
        )
        .mapError(_ => LockStoreFault.Unwritable)
    )(channel => ZIO.attemptBlocking(channel.close()).ignore): channel =>
      ZIO
        .attemptBlocking:
          val buffer = ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))
          val wrote  = writeAll(channel, buffer)
          if wrote then channel.force(true)
          wrote
        .mapError(_ => LockStoreFault.Unwritable)
        .flatMap(wrote => if wrote then ZIO.unit else ZIO.fail(LockStoreFault.Unwritable))

  private def writeAll(channel: FileChannel, buffer: ByteBuffer): Boolean =
    if !buffer.hasRemaining then true
    else
      val wrote = channel.write(buffer)
      wrote > 0 && writeAll(channel, buffer)

  private def readText(path: Path): IO[LockStoreFault, String] =
    ZIO.attemptBlocking(Files.readString(path, StandardCharsets.UTF_8)).mapError(_ => LockStoreFault.Unreadable)

  private def parentOf(target: Path): IO[LockStoreFault, Path] =
    ZIO.fromOption(Option(target.getParent)).orElseFail(LockStoreFault.Unwritable)

  private def fileName(target: Path): String =
    Option(target.getFileName).map(_.toString).getOrElse("dependencies.lock")
end AtomicReplace
