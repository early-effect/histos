package histos.zinc

import java.nio.file.Path

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** An unmanaged source under `target/` replaces the committed file of the same entry. */
object SourcesLawSpec extends ZIOSpecDefault:
  def spec = suite("source overlay laws")(
    test("the unmanaged toolchain source replaces the committed object and keeps the other file"):
      val committedRoot = Path.of("/repo/wire/src")
      val unmanagedRoot = Path.of("/repo/target/unmanaged/wire/src")
      val committed     = List(
        committedRoot.resolve("histos").resolve("Toolchain.scala"),
        committedRoot.resolve("histos").resolve("Dep.scala"),
      )
      val unmanaged = List(unmanagedRoot.resolve("histos").resolve("Toolchain.scala"))
      val merged    = Sources.prefer(committedRoot, committed, unmanagedRoot, unmanaged)
      assertTrue(
        Sources.unmanagedRoot(Path.of("/repo"), "wire") == unmanagedRoot,
        merged == List(
          committedRoot.resolve("histos").resolve("Dep.scala"),
          unmanagedRoot.resolve("histos").resolve("Toolchain.scala"),
        ).sortBy(_.toString),
        Sources.prefer(committedRoot, committed, unmanagedRoot, Nil) == committed,
      )
  )
