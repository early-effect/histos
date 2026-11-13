package histos.publish

import histos.assembly.AssembleError
import histos.program.ProgramError
import histos.workspace.IoFailure
import histos.PairError

/** Pack, link the host client, prove the pair, then install or upload. A proof failure is
  * [[ToolchainPublishError.Proof]] and nothing is uploaded.
  */
enum ToolchainPublishError:
  case Pack(error: AssembleError)
  case Link(platform: DistPlatform, error: ProgramError)
  case Proof(platform: DistPlatform, error: PairError)
  case Unsupported(host: String)
  case Upload(detail: String)
  case Install(failure: IoFailure)

  def message: String = this match
    case Pack(error)            => error.message
    case Link(platform, error)  => s"${platform.token} client: ${error.message}"
    case Proof(platform, error) => s"${platform.token} pair: ${error.message}"
    case Unsupported(host)      => s"$host is not a dist platform this publish links"
    case Upload(detail)         => detail
    case Install(failure)       => failure.message
end ToolchainPublishError

object ToolchainPublish:
  /** `proof` runs first. `deliver` is the install or the upload, and it does not run when `proof` fails. */
  def commit(
      platform: DistPlatform,
      proof: zio.IO[PairError, Unit],
      deliver: zio.IO[ToolchainPublishError, Unit],
  ): zio.IO[ToolchainPublishError, Unit] =
    proof.mapError(error => ToolchainPublishError.Proof(platform, error)) *> deliver
