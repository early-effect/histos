package histos.pack

import histos.jdk.Sha256

import java.nio.charset.StandardCharsets

import zio.Chunk

/** Identity of a package. Each classes directory contributes its product digest, not tasty and not a jar timestamp.
  * `products` keeps that digest from colliding with another checksum of the same hex.
  */
object PackageKey:
  def of(products: List[Sha256]): Sha256 =
    val text =
      s"""products
         |${products.map(_.value).mkString("\n")}
         |""".stripMargin
    Sha256.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))
