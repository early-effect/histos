package histos.contracts

import histos.jdk.{JdkMajor, JdkVersion}
import histos.plugin.{
  ActivationManifest,
  ApiDigest,
  ArtifactFormat,
  ArtifactIdentity,
  ArtifactPath,
  ContentHash,
  Digests,
  EntryName,
  InputRecord,
  PluginOrigin,
  RecipeNodeId,
  RepoPath,
  SealedArtifactManifest,
  ServiceKey,
  SourceId,
  VerifiedPluginManifest,
}
import histos.zinc.{CompilationCapture, CompilationSnapshot}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets

/** Tiny real blobs for later publication, cache, and provenance suites. */
object ContractFixtures:
  val blob: Array[Byte] = "histos-contract".getBytes(StandardCharsets.UTF_8)

  val digest: ContentHash = Digests.content(blob)

  val api: ApiDigest = ApiDigest.trusted(Digests.sha256(blob))

  val jdk: JdkVersion = JdkVersion.trusted("21", JdkMajor.trusted(21))

  def seal(bytes: Array[Byte]): Either[histos.plugin.ArtifactManifestError, SealedArtifactManifest] =
    val hash = Digests.content(bytes)
    SealedArtifactManifest.from(
      List(
        histos.plugin.ArtifactEntry(ArtifactPath("pkg/A.class"), bytes.length.toLong, hash),
        histos.plugin.ArtifactEntry(ArtifactPath("pkg/A.tasty"), bytes.length.toLong, hash),
      )
    )

  def snapshot(bytes: Array[Byte]): Either[histos.plugin.ArtifactManifestError, CompilationSnapshot] =
    seal(bytes).map: outputs =>
      CompilationSnapshot(
        ProjectName("app"),
        ScalaVersion("3.10.1"),
        Platform.Jvm,
        List(
          histos.plugin.SelectedInput(
            RepoPath("src/A.scala"),
            histos.plugin.SourceKind.Scala,
            histos.plugin.SourceSet.Main,
            histos.plugin.SourceProvenance.Unmanaged,
            Digests.content(bytes),
            bytes.length.toLong,
          )
        ),
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        jdk,
        outputs,
        List(EntryName("scala.quoted.Quotes")),
      )

  def recorded(bytes: Array[Byte]): Either[histos.plugin.ArtifactManifestError, CompilationCapture] =
    snapshot(bytes).map(CompilationCapture.Recorded(_))

  def recordOf(capture: CompilationCapture): InputRecord = capture.record

  val unverified: histos.load.ProvenanceResult =
    histos.load.ProvenanceResult.Unverified(SourceId("reviewed-locks"))

  def verifiedEmpty: Either[histos.plugin.ArtifactManifestError, histos.load.ProvenanceResult] =
    seal(blob).map: outputs =>
      val identity = ArtifactIdentity(ArtifactFormat.Jar, outputs.identity.digest)
      val manifest = VerifiedPluginManifest(
        ActivationManifest(
          SourceId("reviewed-locks"),
          ServiceKey("reviewed-locks"),
          PluginOrigin.Source(SourceId("reviewed-locks"), digest),
          identity,
          List(identity),
          api,
          digest,
        ),
        Nil,
      )
      histos.load.ProvenanceResult.Verified(manifest)

  val root: RecipeNodeId = RecipeNodeId("root")
end ContractFixtures
