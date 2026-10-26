package histos.bsp

import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.json.*
import zio.{Scope, ZIO}
import zio.test.*

object ConnectionSpec extends ZIOSpecDefault:
  private val committed =
    """{
      |  "name": "histos",
      |  "argv": ["./histos", "bsp"],
      |  "version": "0.0.1-SNAPSHOT",
      |  "bspVersion": "2.2.0",
      |  "languages": ["scala"]
      |}
      |""".stripMargin

  def spec = suite("bsp connection")(
    test("the committed file is the connection for this repository's wrapper"):
      assertTrue(committed.fromJson[Connection] == Right(Connection.here(List("./histos", "bsp"))))
    ,
    test("a project without a wrapper is launched from PATH"):
      written(wrapper = false).map: text =>
        assertTrue(text.fromJson[Connection] == Right(Connection.here(List("histos", "bsp"))))
    ,
    test("a project with the wrapper uses ./histos"):
      written(wrapper = true).map: text =>
        assertTrue(text.fromJson[Connection] == Right(Connection.here(List("./histos", "bsp"))))
    ,
    test("a matching file is left untouched"):
      ZIO.scoped:
        for
          repo <- work
          _    <- Connection.ensure(repo)
          path = repo.resolve(".bsp").resolve("histos.json")
          stamped <- ZIO.attempt(Files.getLastModifiedTime(path))
          _       <- Connection.ensure(repo)
          again   <- ZIO.attempt(Files.getLastModifiedTime(path))
        yield assertTrue(stamped == again),
  )

  private def written(wrapper: Boolean): ZIO[Scope, Any, String] =
    for
      repo <- work
      _    <- ZIO.when(wrapper)(ZIO.attempt(Files.writeString(repo.resolve("histos"), "")))
      _    <- Connection.ensure(repo)
      text <- ZIO.attempt(Files.readString(repo.resolve(".bsp").resolve("histos.json")))
    yield text

  private def work: ZIO[Scope, Any, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-bsp")))(path => Workspace.delete(path).ignore)
end ConnectionSpec
