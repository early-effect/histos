package histos.js

import histos.jdk.{Sha256, Sha256Digest}
import histos.pack.MainClass
import histos.{JsModuleKind, JsVersion, ScalaVersion}

import java.nio.charset.StandardCharsets

import zio.Chunk

/** Identity of a Scala.js link. A classes directory contributes its product digest, not its class bytes. `products`
  * keeps that digest from colliding with another checksum of the same hex. A stored key restores the script and does
  * not start the linker.
  */
object JsKey:
  def of(
      version: ScalaVersion,
      linker: JsVersion,
      kind: JsModuleKind,
      main: MainClass,
      products: List[Sha256],
      initialize: Boolean = true,
      arguments: List[String] = Nil,
      optimize: histos.JsOptimize = histos.JsOptimize.Fast,
  ): Sha256 =
    val entry =
      if initialize then main.binary else "exports"
    val base =
      s"""${version.value}
         |${linker.value}
         |${kind.token}
         |$entry
         |products
         |${products.map(_.value).mkString("\n")}
         |""".stripMargin
    val asked =
      if arguments.isEmpty then base
      else base + "args\n" + encoded(arguments) + "\n"
    val text = asked + s"optimize\n${optimize.token}\n"
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  /** Count, then one Base64 line per argument. Empty arguments stay out of the key. */
  private[js] def encoded(arguments: List[String]): String =
    arguments.length.toString + "\n" + arguments.map(encode).mkString("\n")

  private def encode(argument: String): String =
    java.util.Base64.getEncoder.encodeToString(argument.getBytes(StandardCharsets.UTF_8))
end JsKey
