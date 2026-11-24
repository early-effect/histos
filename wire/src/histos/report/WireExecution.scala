package histos.report

import histos.load.PluginFault
import histos.preview.PreviewError
import histos.program.ProgramError
import histos.schedule.ScheduleError
import histos.testrun.ModuleTestError
import zio.json.*

/** Execution faults share recursive leaf codecs outside the enclosing socket result. */
private[histos] object WireExecution extends WireSupport:
  import WireLeaves.given
  import WireContract.given
  import WireStages.given
  import WireProducts.given
  import WireBuild.given
  import WireCall.given

  given JsonCodec[ModuleTestError]            = JsonCodec.derived
  given JsonCodec[ProgramError]               = JsonCodec.derived
  given JsonCodec[PreviewError]               = JsonCodec.derived
  given JsonCodec[ScheduleError[PluginFault]] = JsonCodec.derived
  given JsonCodec[PluginFault]                = JsonCodec.derived
end WireExecution
