package histos.native

import histos.NativeLibrary

import java.nio.file.Path

import zio.test.*

object NativeFlagsSpec extends ZIOSpecDefault:
  private val openssl = Path.of("/opt/homebrew/opt/openssl@3")

  def spec = suite("native link flags")(
    test("linux ssl and crypto are the two linker names") {
      val (_, link) = NativeFlags.of(Set(NativeLibrary.Ssl, NativeLibrary.Crypto), "Linux", None)
      assertTrue(link == List("-lcrypto", "-lssl"))
    },
    test("macos ssl uses the homebrew prefix when it is given") {
      val (compile, link) =
        NativeFlags.of(Set(NativeLibrary.Ssl), "Mac OS X", Some(openssl))
      assertTrue(
        compile == List(s"-I${openssl.resolve("include")}"),
        link.contains(s"-L${openssl.resolve("lib")}"),
        link.contains("-lssl"),
      )
    },
    test("macos without the prefix is the plain linker name") {
      val (compile, link) = NativeFlags.of(Set(NativeLibrary.Ssl), "Mac OS X", None)
      assertTrue(compile.isEmpty, link == List("-lssl"))
    },
    test("an empty set adds no flag") {
      val (compile, link) = NativeFlags.of(Set.empty, "Linux", Some(openssl))
      assertTrue(compile.isEmpty, link.isEmpty)
    },
    test("two library sets do not share a link key") {
      val empty = key(Set.empty)
      val ssl   = key(Set(NativeLibrary.Ssl))
      assertTrue(empty != ssl)
    },
  )

  private def key(libraries: Set[NativeLibrary]) =
    val clang  = ClangVersion.from("21.1.0").toOption
    val target = ClangTarget.from("arm64-apple-macosx").toOption
    val main   = histos.pack.MainClass.from("demo.Main").toOption
    (clang, target, main) match
      case (Some(version), Some(triple), Some(entry)) =>
        NativeKey.of(
          histos.ScalaVersion.trusted("3.9.0"),
          version,
          triple,
          entry,
          Nil,
          Nil,
          false,
          libraries,
        )
      case _ => histos.jdk.Sha256Digest.of(zio.Chunk.empty)
end NativeFlagsSpec
