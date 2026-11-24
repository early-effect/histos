package histos.report

import heddle.endpoint.Schema
import histos.plugin.{EmittedContent, OutputKind}
import histos.{
  BuildNode,
  Cell,
  CompilationStage,
  ContractFault,
  JsOptimize,
  PlatformSelection,
  Platforms,
  RootKind,
  ScopedRoot,
  SelectionError,
  TestSelection,
  TestTargets,
  TextListError,
}

/** Scoped graph schemas mirror the separately compiled socket codecs. */
private object WireSchemaStages extends WireSchemaSupport:
  import WireSchemaLeaves.given
  import WireSchemaContract.given

  private given Schema[Char] = text("Char")

  given Schema[Platforms]         = Schema.derived
  given Schema[PlatformSelection] = Schema.derived
  given Schema[TextListError]     = Schema.derived
  given Schema[SelectionError]    = Schema.derived
  given Schema[TestTargets]       = Schema.derived
  given Schema[TestSelection]     = Schema.derived
  given Schema[Cell]              = Schema.derived
  given Schema[CompilationStage]  = Schema.derived
  given Schema[RootKind]          = Schema.derived
  given Schema[ScopedRoot]        = Schema.derived
  given Schema[OutputKind]        = Schema.derived
  given Schema[EmittedContent]    = Schema.derived
  given Schema[BuildNode]         = Schema.derived
  given Schema[ContractFault]     = Schema.derived
  given Schema[JsOptimize]        = enumeration("JsOptimize", List("fast", "full"))
end WireSchemaStages
