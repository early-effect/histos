package histos.js

import histos.jdk.Sha256
import histos.pack.MainClass
import histos.{JsModuleKind, JsVersion, ScalaVersion}

import zio.Chunk
import zio.test.*

object JsLinkSpec extends histos.SuiteClock:
  def spec = suite("js link")(
    test("a trailing comment keeps the link key, and a method body moves it"):
      val comment = Sha256.of(Chunk.fromArray("comment".getBytes))
      val body    = Sha256.of(Chunk.fromArray("body".getBytes))
      val version = ScalaVersion("3.9.0")
      val linker  = JsVersion("1.22.0")
      val main    = MainClass.trusted("demo.Hello")
      val kept    = JsKey.of(version, linker, JsModuleKind.NoModule, main, List(comment))
      val other   = MainClass.trusted("demo.Other")
      assertTrue(
        kept == JsKey.of(version, linker, JsModuleKind.NoModule, main, List(comment)),
        kept != JsKey.of(version, linker, JsModuleKind.NoModule, main, List(body)),
        kept != JsKey.of(version, JsVersion("1.22.1"), JsModuleKind.NoModule, main, List(comment)),
        kept != JsKey.of(version, linker, JsModuleKind.ESModule, main, List(comment)),
        kept != JsKey.of(version, linker, JsModuleKind.NoModule, other, List(comment)),
      )
  )
