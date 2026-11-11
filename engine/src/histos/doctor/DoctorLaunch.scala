package histos.doctor

import histos.select.{SelectError, Selection}
import histos.{Module, ProjectGraph, ProjectName}

/** The modules one doctor invocation launches. No names launches nothing. That is not every module in the graph. */
object DoctorLaunch:
  def modules(graph: ProjectGraph, names: List[ProjectName]): Either[SelectError, List[Module]] =
    if names.isEmpty then Right(Nil)
    else Selection.select(graph, names).map(_.modules)
