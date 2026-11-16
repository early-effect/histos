package histos.zinc

import histos.jdk.JdkVersion
import histos.plugin.{
  ArtifactIdentity,
  ContentHash,
  EntryName,
  FrozenInput,
  InputKey,
  InputRecord,
  RepoPath,
  SealedArtifactManifest,
  StageContract,
}
import histos.{Platform, ProjectName, ScalaVersion}

/** One selected compile input. A recorded empty list is not an absent capture. */
enum SourceKind:
  case Scala
  case Java
  case Resource

final case class SelectedSource(path: RepoPath, kind: SourceKind, digest: ContentHash, length: Long)

/** One semantic compiler setting. Output paths are not settings. */
final case class SemanticSetting(key: InputKey, value: String)

/** The frozen inputs and sealed outputs of one cell. P5 captures this. P1 only defines it. */
final case class CompilationSnapshot(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    sources: List[SelectedSource],
    resources: List[SelectedSource],
    settings: List[SemanticSetting],
    stages: List[StageContract],
    dependencies: List[ArtifactIdentity],
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
      InputRecord.Frozen:
        snapshot.sources.map: source =>
          FrozenInput(source.path, source.digest, source.length)
end CompilationCapture
