package histos.contracts

import histos.load.ProvenanceResult
import histos.plugin.{InputRecord, RecipeError, RebuildRecipe, SealedArtifactManifest}
import histos.suite.HistosSpec
import histos.zinc.CompilationCapture

import zio.test.*

object ContractModelsSpec extends HistosSpec:
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
      val unrecorded   = CompilationCapture.Unrecorded.record
      val frozen       = emptied.map(_.record)
      val emptySources = frozen match
        case Some(InputRecord.Frozen(body)) => body.sources.isEmpty
        case _                              => false
      val read = frozen match
        case Some(record) =>
          RebuildRecipe.sourcesOf(ContractFixtures.root, record) match
            case Right(body) => body.sources.isEmpty
            case Left(_)     => false
        case None => false
      assertTrue(
        captured.isDefined,
        unrecorded == InputRecord.Unrecorded,
        emptySources,
        frozen != Some(InputRecord.Unrecorded),
        RebuildRecipe.sourcesOf(ContractFixtures.root, unrecorded) ==
          Left(RecipeError.Unrecorded(ContractFixtures.root)),
        read,
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
