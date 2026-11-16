package histos.resolve

import histos.lock.{LockMode, LockRevisionState}
import histos.plugin.{ClientId, CommandIdentity, ContentHash, GenerationId, InvocationId, ResolutionIntent}

import java.nio.file.Path

import zio.{IO, Ref, UIO, ZIO}

/** Committed snapshots satisfy a frozen caller. An ephemeral snapshot never does. */
enum SnapshotSelection:
  case Committed
  case Ephemeral

final case class SnapshotProvenance(invocation: InvocationId, client: ClientId, command: CommandIdentity)

/** One immutable view of a generation. Leases keep the snapshot they captured. */
final case class ResolutionSnapshot(
    intent: ResolutionIntent,
    lockRevision: LockRevisionState,
    declarations: ContentHash,
    cells: List[VersionedPin],
    provenance: SnapshotProvenance,
    selection: SnapshotSelection,
    generation: GenerationId,
)

object ResolutionSnapshot:
  def cells(document: LockDocument): List[VersionedPin] =
    document.pins ++ LockDocument.retainedPins(document.retained)

  def admits(snapshot: ResolutionSnapshot, mode: LockMode): Boolean =
    mode match
      case LockMode.Frozen => snapshot.selection == SnapshotSelection.Committed
      case LockMode.Offer  => true
      case LockMode.Update => true
      case LockMode.Local  => true

  def render(snapshot: ResolutionSnapshot): String =
    val selection =
      snapshot.selection match
        case SnapshotSelection.Committed => "committed"
        case SnapshotSelection.Ephemeral => "ephemeral"
    val cells = snapshot.cells.map(LockDocument.renderPin)
    val head  = List(
      s"selection $selection",
      s"revision ${LockCodec.renderRevision(snapshot.lockRevision)}",
      s"generation ${snapshot.generation.value}",
      s"declarations ${snapshot.declarations.value}",
      s"intent ${LockCodec.renderIntent(snapshot.intent)}",
      s"invocation ${snapshot.provenance.invocation.value}",
      s"client ${snapshot.provenance.client.value}",
      s"command ${LockCodec.renderCommand(snapshot.provenance.command)}",
    )
    (head ++ cells).mkString("\n") + "\n"
end ResolutionSnapshot

/** Publishes a generation. The tracked lock is not this owner's file. */
trait GenerationOwner:
  def publish(snapshot: ResolutionSnapshot): IO[LockStoreFault, Unit]
  def current: UIO[Option[ResolutionSnapshot]]
  def lease(generation: GenerationId): UIO[Option[ResolutionSnapshot]]

object GenerationOwner:
  /** In-memory generations. Ephemeral selection also lands in `.histos/resolution`. */
  def memory(root: Path): UIO[GenerationOwner] =
    for
      published <- Ref.make(Map.empty[GenerationId, ResolutionSnapshot])
      currentId <- Ref.make(Option.empty[GenerationId])
    yield Memory(root, published, currentId)

  private final class Memory(
      root: Path,
      published: Ref[Map[GenerationId, ResolutionSnapshot]],
      currentId: Ref[Option[GenerationId]],
  ) extends GenerationOwner:
    def publish(snapshot: ResolutionSnapshot): IO[LockStoreFault, Unit] =
      for
        _ <- published.update(_ + (snapshot.generation -> snapshot))
        _ <- currentId.set(Some(snapshot.generation))
        _ <- snapshot.selection match
          case SnapshotSelection.Ephemeral =>
            AtomicReplace.write(root.resolve(".histos").resolve("resolution"), ResolutionSnapshot.render(snapshot))
          case SnapshotSelection.Committed => ZIO.unit
      yield ()

    def current: UIO[Option[ResolutionSnapshot]] =
      currentId.get.flatMap:
        case None     => ZIO.succeed(None)
        case Some(id) => published.get.map(_.get(id))

    def lease(generation: GenerationId): UIO[Option[ResolutionSnapshot]] =
      published.get.map(_.get(generation))
  end Memory
end GenerationOwner
