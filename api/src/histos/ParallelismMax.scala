package histos

import _root_.scala.annotation.publicInBinary

/** A ceiling on how many steps of one resource class may run at once. The core count is the other bound. */
opaque type ParallelismMax = Int

object ParallelismMax:
  given CanEqual[ParallelismMax, ParallelismMax] = CanEqual.derived

  def from(raw: Int): Either[ContractFault, ParallelismMax] =
    if raw > 0 then Right(trusted(raw)) else Left(ContractFault.Parallelism(raw))

  @publicInBinary private[histos] def trusted(raw: Int): ParallelismMax = raw

  extension (max: ParallelismMax) def value: Int = max
end ParallelismMax
