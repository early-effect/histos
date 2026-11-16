package histos.contracts

import histos.load.ProvenanceResult
import histos.plugin.{InputRecord, RecipeError, RebuildRecipe, SealedArtifactManifest}
import histos.zinc.CompilationCapture

import zio.test.*

object ContractModelsSpec extends ZIOSpecDefault:
  def spec = suite("contract models")(
    test("unverified provenance is not an empty artifact list") {
      val verified = ContractFixtures.verifiedEmpty.map(ProvenanceResult.artifacts)
      assertTrue(
        ProvenanceResult.artifacts(ContractFixtures.unverified) == None,
        verified == Right(Some(Nil)),
      )
    },
    test("an unrecorded capture is not a recorded empty source list") {
      val captured = ContractFixtures.recorded(ContractFixtures.blob).toOption
      val emptied  = captured.collect:
        case CompilationCapture.Recorded(snapshot) =>
          CompilationCapture.Recorded(snapshot.copy(sources = Nil))
      val unrecorded = CompilationCapture.Unrecorded.record
      val frozen     = emptied.map(_.record)
      assertTrue(
        captured.isDefined,
        unrecorded == InputRecord.Unrecorded,
        frozen.contains(InputRecord.Frozen(Nil)),
        unrecorded != InputRecord.Frozen(Nil),
        RebuildRecipe.sourcesOf(ContractFixtures.root, unrecorded) ==
          Left(RecipeError.Unrecorded(ContractFixtures.root)),
        RebuildRecipe.sourcesOf(ContractFixtures.root, InputRecord.Frozen(Nil)) == Right(Nil),
        emptied != Some(CompilationCapture.Unrecorded),
      )
    },
    test("a sealed fixture hashes the real blob and is not the empty seal") {
      val filled = ContractFixtures.seal(ContractFixtures.blob)
      val empty  = SealedArtifactManifest.from(Nil)
      assertTrue(
        filled.map(_.entries.map(_.digest)) == Right(List(ContractFixtures.digest, ContractFixtures.digest)),
        filled.map(_.identity) != empty.map(_.identity),
      )
    },
  )
