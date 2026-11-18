package histos.task

import histos.lock.{LockMode, LockTransactionError}
import histos.plugin.{Digests, ProposalId}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

enum HostIo:
  case Absent, Read, Write, Spawn, Wait, Stat

  def message: String = this match
    case HostIo.Absent => "absent"
    case HostIo.Read   => "read"
    case HostIo.Write  => "write"
    case HostIo.Spawn  => "spawn"
    case HostIo.Wait   => "wait"
    case HostIo.Stat   => "stat"
end HostIo

/** `Io` names the path and the kind. It does not carry an exception message. */
enum HostProof:
  case ClientAbsent(path: Path)
  case ServerSilent(path: Path, note: String)
  case Frozen
  case ExitedZero
  case Neither
  case Io(path: Path, kind: HostIo)

  def message: String = this match
    case ClientAbsent(path)       => s"histos-client is not executable at ${path.toString}"
    case ServerSilent(path, note) =>
      val where = s"the scratch server wrote no server.json at ${path.toString}"
      if note.isEmpty then where else s"$where\n$note"
    case Frozen         => "the client printed a frozen lock marker"
    case ExitedZero     => "the client exited 0"
    case Neither        => "the client printed neither consent nor a frozen marker"
    case Io(path, kind) => s"${kind.message} ${path.toString}"
end HostProof

/** Consent, or one of the output proofs. A missing file is not a verdict. */
enum HostVerdict:
  case Consent
  case Failed(proof: HostProof)

object HostVerdict:
  def verdict(exit: Int, output: String): HostVerdict =
    if markers.exists(output.contains) then HostVerdict.Failed(HostProof.Frozen)
    else if exit == 0 then HostVerdict.Failed(HostProof.ExitedZero)
    else if output.contains(consent) then HostVerdict.Consent
    else HostVerdict.Failed(HostProof.Neither)

  private val consent: String =
    LockTransactionError
      .ConsentRequired(ProposalId.trusted(Digests.sha256("host".getBytes(StandardCharsets.UTF_8))))
      .message

  private val markers: List[String] =
    List(
      "frozen drift",
      LockTransactionError.Unapplied(LockMode.Frozen).message,
    )
end HostVerdict
