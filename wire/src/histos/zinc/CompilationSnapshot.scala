package histos.zinc

import histos.jdk.JdkVersion
import histos.plugin.{
  ArtifactIdentity,
  ClosureArtifact,
  EntryName,
  HostSubstitution,
  InputRecord,
  ProducerBinding,
  RecordedInputs,
  SealedArtifactManifest,
  SelectedInput,
  SemanticSetting,
  StageContract,
}
import histos.{Platform, ProjectName, ScalaVersion}

/** The frozen inputs and sealed outputs of one cell. P5 captures this. P1 only defines it. */
final case class CompilationSnapshot(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    sources: List[SelectedInput],
    resources: List[SelectedInput],
    settings: List[SemanticSetting],
    stages: List[StageContract],
    closure: List[ClosureArtifact],
    stageArtifacts: List[ArtifactIdentity],
    host: List[HostSubstitution],
    compiler: List[ArtifactIdentity],
    runtime: List[ArtifactIdentity],
    jdk: JdkVersion,
    outputs: SealedArtifactManifest,
    knownStages: List[EntryName],
)

/** `Unrecorded` means no snapshot was captured. It is not a successful empty capture. */
enum CompilationCapture:
  case Unrecorded
  case Recorded(snapshot: CompilationSnapshot)

  /** Recipe input for this capture. Unrecorded stays unrecorded. An empty recorded source list stays empty. */
  def record: InputRecord = this match
    case CompilationCapture.Unrecorded         => InputRecord.Unrecorded
    case CompilationCapture.Recorded(snapshot) =>
      InputRecord.Frozen(
        RecordedInputs(
          sources = snapshot.sources ++ snapshot.resources,
          settings = snapshot.settings,
          closure = snapshot.closure,
          stages = snapshot.stageArtifacts,
          host = snapshot.host,
          producer = ProducerBinding(
            snapshot.version,
            snapshot.jdk.major.value,
            snapshot.compiler,
            snapshot.runtime,
          ),
        )
      )
end CompilationCapture
