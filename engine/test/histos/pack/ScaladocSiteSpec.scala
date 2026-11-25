package histos.pack

import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.ToolchainIdentity

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object ScaladocSiteSpec extends HistosSpec:
  def spec = suite("scaladoc refuses a packed launcher")(
    test("a stamped jar is not a tool classpath") {
      ZIO.scoped:
        for
          path     <- jar(ToolchainIdentity.entry)
          rejected <- ZIO.attempt(ScaladocSite.rejects(path.toString)).orDie
        yield assertTrue(rejected)
    },
    test("a merged scala.caps jar is not a tool classpath") {
      ZIO.scoped:
        for
          path     <- jar("scala/caps.class", "scala/caps/Pure.class")
          rejected <- ZIO.attempt(ScaladocSite.rejects(path.toString)).orDie
        yield assertTrue(rejected)
    },
    test("a library jar stays a tool classpath") {
      ZIO.scoped:
        for
          path  <- jar("scala/caps/Pure.class")
          stays <- ZIO.attempt(!ScaladocSite.rejects(path.toString)).orDie
        yield assertTrue(stays)
    },
    test("one stamped entry rejects the whole classpath") {
      ZIO.scoped:
        for
          stamped  <- jar(ToolchainIdentity.entry)
          library  <- jar("com/example/Lib.class")
          rejected <- ZIO
            .attempt(ScaladocSite.rejects(library.toString + java.io.File.pathSeparator + stamped.toString))
            .orDie
        yield assertTrue(rejected)
    },
  )

  private def jar(names: String*): ZIO[Scope, java.io.IOException, Path] =
    for
      root <- Workspace.work("histos-scaladoc-classpath").orDieWith(error => new java.io.IOException(error.message))
      path  = root.resolve("tool.jar")
      bytes = Chunk.fromArray("x".getBytes(StandardCharsets.UTF_8))
      _ <- ClassJar
        .writeBytes(names.toList.map(name => name -> bytes), path)
        .orDieWith(error => new java.io.IOException(error.message))
    yield path
end ScaladocSiteSpec
