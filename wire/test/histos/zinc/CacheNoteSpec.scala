package histos.zinc

import histos.suite.HistosSpec
import histos.{Platform, ProjectName, ScalaVersion}

import zio.test.*

object CacheNoteSpec extends HistosSpec:
  private val lib     = ProjectName("lib")
  private val version = ScalaVersion("3.10.1")

  def spec = suite("cache note")(
    test("a hit, a vacant cell, and a miss name the decision") {
      val disk   = CacheNote.line(lib, version, Platform.Jvm, CacheNote.Disk)
      val store  = CacheNote.line(lib, version, Platform.Native, CacheNote.Store)
      val vacant = CacheNote.line(lib, version, Platform.Js, CacheNote.Vacant)
      val fresh  = CacheNote.line(
        lib,
        version,
        Platform.Jvm,
        CacheNote.Miss(CacheLookup.NoBlob, KeyMiss.NoPrior, CacheSave.Stored),
      )
      val dropped = CacheNote.line(
        lib,
        version,
        Platform.Native,
        CacheNote.Miss(
          CacheLookup.Stamps,
          KeyMiss.Changed(List(CompileInput.Sources, CompileInput.Apis)),
          CacheSave.Unsaved(CacheSkip.Stamps),
        ),
      )
      assertTrue(
        disk == "lib 3.10.1 jvm cache hit disk",
        store == "lib 3.10.1 native cache hit store",
        vacant == "lib 3.10.1 js cache vacant",
        fresh == "lib 3.10.1 jvm cache miss no-blob no-prior stored",
        dropped == "lib 3.10.1 native cache miss stamps inputs sources apis unsaved stamps",
      )
    }
  )
end CacheNoteSpec
