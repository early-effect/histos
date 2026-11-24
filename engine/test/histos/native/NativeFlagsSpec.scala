package histos.native

import histos.NativeLibrary
import histos.workspace.Workspace

import zio.ZIO

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
    test("homebrew prefers openssl 3, then generic, then installed versioned prefixes") {
      val generic   = Path.of("/opt/homebrew/opt/openssl")
      val versioned = Path.of("/opt/homebrew/opt/openssl@4")
      assertTrue(
        HomebrewOpenssl.select(List(versioned, generic, openssl)).contains(openssl),
        HomebrewOpenssl.select(List(versioned, generic)).contains(generic),
        HomebrewOpenssl.select(List(versioned)).contains(versioned),
        HomebrewOpenssl.select(List(Path.of("/"), Path.of("/opt/homebrew/opt/openssl-backup"))).isEmpty,
        HomebrewOpenssl.select(Nil).isEmpty,
      )
    },
    test("discovery requires both include and lib and leaves linux untouched") {
      ZIO.scoped:
        for
          opt <- Workspace.work("native-openssl")
          preferred = opt.resolve("openssl@3")
          generic   = opt.resolve("openssl")
          versioned = opt.resolve("openssl@4")
          _        <- Workspace.createDirectories(preferred.resolve("lib"))
          _        <- Workspace.createDirectories(generic.resolve("include"))
          _        <- Workspace.createDirectories(generic.resolve("lib"))
          _        <- Workspace.createDirectories(versioned.resolve("include"))
          _        <- Workspace.createDirectories(versioned.resolve("lib"))
          fallback <- HomebrewOpenssl.discover("Mac OS X", opt)
          _        <- Workspace.createDirectories(preferred.resolve("include"))
          selected <- HomebrewOpenssl.discover("Darwin", opt)
          _        <- Workspace.delete(generic)
          _        <- Workspace.delete(preferred)
          version  <- HomebrewOpenssl.discover("Mac OS X", opt)
          linux    <- HomebrewOpenssl.discover("Linux", opt)
          missing  <- HomebrewOpenssl.discover("Mac OS X", opt.resolve("absent"))
        yield assertTrue(
          fallback.contains(generic),
          selected.contains(preferred),
          version.contains(versioned),
          linux.isEmpty,
          missing.isEmpty,
        )
    },
    test("generic homebrew installation supplies matching include and lib bridge options") {
      val generic                   = Path.of("/opt/homebrew/opt/openssl")
      val libraries                 = Set(NativeLibrary.Ssl, NativeLibrary.Crypto)
      val (compile, link)           = NativeFlags.of(libraries, "Mac OS X", Some(generic))
      val (linuxCompile, linuxLink) = NativeFlags.of(libraries, "Linux", Some(generic))
      val source                    = NativeBridge.source(
        "test",
        "demo.Main",
        Path.of("/usr/bin/clang"),
        Path.of("/usr/bin/clang++"),
        Nil,
        false,
        libraries,
        Some("Mac OS X"),
        Some(generic),
      )
      assertTrue(
        compile == List(s"-I${generic.resolve("include")}"),
        link == List(s"-L${generic.resolve("lib")}", "-lcrypto", "-lssl"),
        source.contains(s"\"-I${generic.resolve("include")}\""),
        source.contains(s"\"-L${generic.resolve("lib")}\""),
        linuxCompile.isEmpty,
        linuxLink == List("-lcrypto", "-lssl"),
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
