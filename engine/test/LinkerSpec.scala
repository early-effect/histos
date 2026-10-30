package histos.js

import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.native.{
  ClangTarget,
  ClangText,
  ClangVersion,
  LinkError,
  NativeError,
  NativeKey,
  NativeLinkKind,
  NativeLinkRequest,
  NativeLinker,
  NativeToolchain,
}
import histos.pack.MainClass
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace
import histos.{ClangMajor, JsModuleKind, JsVersion, ScalaVersion}

import java.nio.file.Path

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object LinkerSpec extends histos.SuiteClock:
  private val version  = ScalaVersion("3.9.0")
  private val jsLinker = JsVersion("1.22.0")
  private val main     = MainClass.trusted("demo.Hello")
  private val comment  = Sha256.of(Chunk.fromArray("comment".getBytes))
  private val body     = Sha256.of(Chunk.fromArray("body".getBytes))
  private val kept     = JsKey.of(version, jsLinker, JsModuleKind.NoModule, main, List(comment))
  private val moved    = JsKey.of(version, jsLinker, JsModuleKind.NoModule, main, List(body))
  private val jdk      = JdkRuntime(
    Path.of("jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("jdk"), Sha256.of(Chunk.fromArray("jdk".getBytes))),
  )

  def spec =
    suite("linker")(
      unchanged,
      changed,
      nativeUnchanged,
      nativeChanged,
    )

  private def unchanged =
    test("a comment-sized input records no link"):
      ZIO.scoped:
        (for
          work <- Workspace.work("histos-linker-comment")
          script = work.resolve("program.js")
          _ <- stub[Linker](_.link(any[JsLinkRequest]))(writeJs(script, kept))
          request = asked(work)
          _      <- Linker.reuse(kept, request)
          first  <- ZIO.serviceWith[Stubbed[Linker]](_.calls)
          again  <- Linker.reuse(kept, request)
          second <- ZIO.serviceWith[Stubbed[Linker]](_.calls)
        yield assertTrue(
          kept != moved,
          again.script == script.toAbsolutePath,
          again.key == kept,
          names(first) == List("link"),
          names(second) == List("link"),
        )).provideSomeLayer[Scope](stubbedTraced[Linker])

  private def changed =
    test("a body change records one link"):
      ZIO.scoped:
        (for
          work <- Workspace.work("histos-linker-body")
          script = work.resolve("program.js")
          _ <- stub[Linker](_.link(any[JsLinkRequest]))(writeJs(script, moved))
          request = asked(work)
          _      <- Linker.reuse(kept, request)
          first  <- ZIO.serviceWith[Stubbed[Linker]](_.calls)
          linked <- Linker.reuse(moved, request)
          second <- ZIO.serviceWith[Stubbed[Linker]](_.calls)
        yield assertTrue(
          linked.script == script.toAbsolutePath,
          linked.key == moved,
          names(first) == List("link"),
          names(second) == List("link", "link"),
        )).provideSomeLayer[Scope](stubbedTraced[Linker])

  private def nativeUnchanged =
    test("a comment-sized native input records no link"):
      ZIO.scoped:
        (for
          work <- Workspace.work("histos-native-linker-comment")
          binary = work.resolve("hello")
          keptKey  <- nativeKey(comment)
          movedKey <- nativeKey(body)
          _        <- stub[NativeLinker](_.linkClasspath(any[NativeLinkRequest], any[List[Path]]))(writeNative(binary))
          _        <- NativeLinker.reuse(keptKey, nativeRequest(work), Nil)
          first    <- ZIO.serviceWith[Stubbed[NativeLinker]](_.calls)
          again    <- NativeLinker.reuse(keptKey, nativeRequest(work), Nil)
          second   <- ZIO.serviceWith[Stubbed[NativeLinker]](_.calls)
        yield assertTrue(
          keptKey != movedKey,
          again == binary.toAbsolutePath,
          names(first) == List("linkClasspath"),
          names(second) == List("linkClasspath"),
        )).provideSomeLayer[Scope](stubbedTraced[NativeLinker])

  private def nativeChanged =
    test("a native body change records one link"):
      ZIO.scoped:
        (for
          work <- Workspace.work("histos-native-linker-body")
          binary = work.resolve("hello")
          keptKey  <- nativeKey(comment)
          movedKey <- nativeKey(body)
          _        <- stub[NativeLinker](_.linkClasspath(any[NativeLinkRequest], any[List[Path]]))(writeNative(binary))
          _        <- NativeLinker.reuse(keptKey, nativeRequest(work), Nil)
          first    <- ZIO.serviceWith[Stubbed[NativeLinker]](_.calls)
          linked   <- NativeLinker.reuse(movedKey, nativeRequest(work), Nil)
          second   <- ZIO.serviceWith[Stubbed[NativeLinker]](_.calls)
        yield assertTrue(
          linked == binary.toAbsolutePath,
          names(first) == List("linkClasspath"),
          names(second) == List("linkClasspath", "linkClasspath"),
        )).provideSomeLayer[Scope](stubbedTraced[NativeLinker])

  private def asked(work: Path): JsLinkRequest =
    JsLinkRequest(version, main, Nil, work, jdk)

  private def nativeRequest(work: Path): NativeLinkRequest =
    NativeLinkRequest(
      ClangMajor(21),
      NativeToolchain.Release,
      Map.empty,
      work,
      jdk,
      main,
      version,
      NativeLinkKind.Program,
    )

  private def nativeKey(sources: Sha256): ZIO[Any, ClangText, Sha256] =
    for
      clang  <- ZIO.fromEither(ClangVersion.from("21.0.0"))
      target <- ZIO.fromEither(ClangTarget.from("arm64-apple-darwin"))
    yield NativeKey.of(version, clang, target, sources, Nil)

  private def writeJs(script: Path, key: Sha256) =
    Workspace
      .writeString(script, "script")
      .mapError(failure => JsLinkError.Linker(failure.message))
      .as(JsLinked(script, key))

  private def writeNative(binary: Path) =
    Workspace
      .writeString(binary, "bin")
      .mapError(failure => NativeError.Link(LinkError.Linker(failure.message)))
      .as(binary)

  private def names(calls: Chunk[slayer.Call]): List[String] =
    calls.map(_.id.jvmName).toList
end LinkerSpec
