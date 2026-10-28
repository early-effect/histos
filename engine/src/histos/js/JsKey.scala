package histos.js

import histos.jdk.Sha256
import histos.pack.MainClass
import histos.{JsModuleKind, JsVersion, ScalaVersion}

import java.nio.charset.StandardCharsets

import zio.Chunk

/** Identity of a Scala.js link. A classes directory contributes its product digest, not its class bytes. `products`
  * keeps that digest from colliding with another checksum of the same hex.
  */
object JsKey:
  def of(
      version: ScalaVersion,
      linker: JsVersion,
      kind: JsModuleKind,
      main: MainClass,
      products: List[Sha256],
  ): Sha256 =
    val text =
      s"""${version.value}
         |${linker.value}
         |${kind.token}
         |${main.binary}
         |products
         |${products.map(_.value).mkString("\n")}
         |""".stripMargin
    Sha256.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))
