package histos.report

import histos.plugin.{EmittedContent, OutputKind}
import histos.{
  BuildNode,
  Cell,
  CompilationStage,
  CompileExtent,
  ContractFault,
  JsOptimize,
  PlatformSelection,
  Platforms,
  RootKind,
  ScalaSelection,
  ScalaToken,
  ScopedRoot,
  SelectionError,
  TestSelection,
  TestTargets,
  TextListError,
}

import zio.json.*

/** Scoped graph codecs are separate from build failures to bound derivation and class constant-pool size. */
private[histos] object WireStages extends WireSupport:
  import WireLeaves.given
  import WireContract.given

  given JsonCodec[Platforms]         = JsonCodec.derived
  given JsonCodec[PlatformSelection] = JsonCodec.derived
  given JsonCodec[ScalaToken]        = JsonCodec.derived
  given JsonCodec[ScalaSelection]    = JsonCodec.derived
  given JsonCodec[CompileExtent]     = JsonCodec.derived
  given JsonCodec[TextListError]     = JsonCodec.derived
  given JsonCodec[SelectionError]    = JsonCodec.derived
  given JsonCodec[TestTargets]       = JsonCodec.derived
  given JsonCodec[TestSelection]     = JsonCodec.derived
  given JsonCodec[Cell]              = JsonCodec.derived
  given JsonCodec[CompilationStage]  = JsonCodec.derived
  given JsonCodec[RootKind]          = JsonCodec.derived
  given JsonCodec[ScopedRoot]        = JsonCodec.derived
  given JsonCodec[OutputKind]        = JsonCodec.derived
  given JsonCodec[EmittedContent]    = JsonCodec.derived
  given JsonCodec[BuildNode]         = JsonCodec.derived
  given JsonCodec[ContractFault]     = JsonCodec.derived
  given JsonCodec[JsOptimize]        =
    choose("JS optimization", JsOptimize.values.toList.map(mode => mode.token -> mode), _.token)
end WireStages
