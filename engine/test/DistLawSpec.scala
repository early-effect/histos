package histos.publish

import histos.workspace.Workspace

import java.nio.charset.StandardCharsets

import zio.ZIO
import zio.test.*

/** A dist bundle names the jar, the clients, and the sources, and the source tar is the three module trees that exist.
  */
object DistLawSpec extends ZIOSpecDefault:
  def spec = suite("dist")(
    test("a bundle names the manifest, the jar, the sources, and each client, and sorts the client object"):
      val jar     = Array[Byte](9)
      val linux   = Array[Byte](1, 2)
      val mac     = Array[Byte](3)
      val sources = Array[Byte](7)
      val packed  = Dist.bundle(
        "9.9.9",
        jar,
        List(DistPlatform.MacosAarch64 -> mac, DistPlatform.LinuxX86_64 -> linux),
        sources,
      )
      val text = packed
        .collectFirst { case ("9.9.9-manifest.json", bytes) =>
          new String(bytes, StandardCharsets.UTF_8)
        }
        .getOrElse("")
      assertTrue(
        packed.map(_._1) == List(
          "9.9.9-manifest.json",
          "9.9.9-histos.jar",
          "9.9.9-sources.tar",
          "9.9.9-client-macos-aarch64",
          "9.9.9-client-linux-x86_64",
        ),
        packed.collect { case ("9.9.9-histos.jar", bytes) => bytes.toList } == List(jar.toList),
        packed.collect { case ("9.9.9-sources.tar", bytes) => bytes.toList } == List(sources.toList),
        text.contains("\"version\": \"9.9.9\""),
        text.indexOf("\"linux-x86_64\"") >= 0,
        text.indexOf("\"macos-aarch64\"") > text.indexOf("\"linux-x86_64\""),
        text.contains(DistLayout.sha256(jar)),
        text.contains(DistLayout.sha256(linux)),
        text.contains(DistLayout.sha256(mac)),
        text.contains(DistLayout.sha256(sources)),
      )
    ,
    test("sources pack the build file and the api tree, and a missing module tree adds no entry"):
      ZIO.scoped {
        Workspace.work("histos-dist").flatMap { repo =>
          val deep = "n" * 80 + ".scala"
          for
            _ <- Workspace.writeString(
              repo.resolve("api").resolve("src").resolve("main").resolve("scala").resolve(deep),
              "object OnlyQuick\n",
            )
            tar <- ZIO.attemptBlocking(Dist.sources(repo)).orDie
            names = tarNames(tar)
          yield assertTrue(
            names.contains("histos.scala"),
            names.contains(s"api/src/main/scala/$deep"),
            names.forall(name => !name.startsWith("wire/") && !name.startsWith("client/")),
            new String(tar, StandardCharsets.UTF_8).contains("object OnlyQuick"),
            new String(tar, StandardCharsets.US_ASCII).contains("object build extends HistosBuild"),
          )
        }
      },
  )

  /** USTAR names. A path longer than the name field is prefix plus name. */
  private def tarNames(bytes: Array[Byte]): List[String] =
    def go(at: Int, acc: List[String]): List[String] =
      if at + 512 > bytes.length then acc.reverse
      else if (0 until 512).forall(i => bytes(at + i) == 0) then acc.reverse
      else
        val name   = field(bytes, at, 100)
        val prefix = field(bytes, at + 345, 155)
        val path   = if prefix.isEmpty then name else s"$prefix/$name"
        octal(field(bytes, at + 124, 12)) match
          case None       => acc.reverse
          case Some(size) =>
            val data = ((size + 511) / 512) * 512
            go(at + 512 + data, path :: acc)
    go(0, Nil)

  private def field(bytes: Array[Byte], at: Int, length: Int): String =
    val raw = new String(bytes, at, length, StandardCharsets.UTF_8)
    raw.takeWhile(_ != '\u0000').trim

  private def octal(text: String): Option[Int] =
    val digits = text.takeWhile(c => c >= '0' && c <= '7')
    if digits.isEmpty then None
    else Some(digits.foldLeft(0)((n, c) => n * 8 + (c - '0')))
end DistLawSpec
