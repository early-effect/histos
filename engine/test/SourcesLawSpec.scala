package histos.zinc

import histos.workspace.Workspace
import histos.{Platform, ScalaVersion}

import java.nio.file.Path

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Which files a cell compiles. An unmanaged source under `target/` replaces the committed file of the same entry. */
object SourcesLawSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.10.0")

  def spec = suite("source selection laws")(
    test("main and test sources stay apart, java is jvm only, and a platform directory keeps its platform"):
      Workspace
        .work("histos-sources")
        .flatMap: root =>
          val files = List(
            "src/main/scala/demo/A.scala",
            "src/main/scala/js/Pkg.scala",
            "src/main/java/demo/J.java",
            "src/main/js/scala/Js.scala",
            "src/test/scala/demo/T.scala",
          )
          for
            absent <- Sources.select(root.resolve("missing"), version, Platform.Jvm)
            _      <- ZIO.foreachDiscard(files)(file => Workspace.writeString(root.resolve(file), "object A"))
            jvm    <- Sources.select(root, version, Platform.Jvm)
            js     <- Sources.select(root, version, Platform.Js)
            tests  <- Sources.tests(root, version, Platform.Jvm)
          yield assertTrue(
            absent.isEmpty,
            jvm.map(_.getFileName.toString).sorted == List("A.scala", "J.java", "Pkg.scala"),
            js.map(_.getFileName.toString).sorted == List("A.scala", "Js.scala", "Pkg.scala"),
            tests.map(_.getFileName.toString) == List("T.scala"),
          )
    ,
    test("an unmanaged source replaces the committed file of the same relative path"):
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
      ),
  )
end SourcesLawSpec
