package histos.bsp

import histos.publish.LauncherPin
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.json.*
import zio.{Scope, System, ZIO}
import zio.test.*

object ConnectionSpec extends ZIOSpecDefault:
  def spec = suite("bsp connection")(
    test("the committed file is the version in build.properties"):
      for
        root <- System.env("HISTOS_REPO").some
        repo = Path.of(root)
        pin  <- LauncherPin.read(repo)
        text <- ZIO.attempt(Files.readString(repo.resolve(".bsp").resolve("histos.json")))
      yield assertTrue(
        text.fromJson[Connection] == Right(Connection.here(List("./histos", "bsp"), pin.version))
      )
    ,
    test("a project without a wrapper is launched from PATH"):
      written(wrapper = false).map: text =>
        assertTrue(text.fromJson[Connection] == Right(Connection.here(List("histos", "bsp"), pinned)))
    ,
    test("a project with the wrapper uses ./histos"):
      written(wrapper = true).map: text =>
        assertTrue(text.fromJson[Connection] == Right(Connection.here(List("./histos", "bsp"), pinned)))
    ,
    test("build.properties supplies the connection version"):
      ZIO.scoped:
        for
          repo <- work
          _    <- plant(repo, "histos.version=0.0.2-SNAPSHOT\n")
          _    <- ZIO.attempt(Files.writeString(repo.resolve("histos"), ""))
          text <- writtenAt(repo)
        yield assertTrue(
          text.fromJson[Connection] == Right(Connection.here(List("./histos", "bsp"), "0.0.2-SNAPSHOT"))
        )
    ,
    test("a bad version line does not write the connection"):
      ZIO.scoped:
        for
          repo    <- work
          _       <- plant(repo, "histos.version=0.0.2\n")
          _       <- Connection.ensure(repo)
          present <- ZIO.attempt(Files.isRegularFile(repo.resolve(".bsp").resolve("histos.json")))
        yield assertTrue(!present)
    ,
    test("a matching file is left untouched"):
      ZIO.scoped:
        for
          repo <- work
          _    <- plant(repo, s"histos.version=$pinned\n")
          _    <- Connection.ensure(repo)
          path = repo.resolve(".bsp").resolve("histos.json")
          stamped <- ZIO.attempt(Files.getLastModifiedTime(path))
          _       <- Connection.ensure(repo)
          again   <- ZIO.attempt(Files.getLastModifiedTime(path))
        yield assertTrue(stamped == again),
  ) @@ TestAspect.withLiveSystem

  private val pinned = "0.0.2-SNAPSHOT"

  private def written(wrapper: Boolean): ZIO[Scope, Any, String] =
    for
      repo <- work
      _    <- plant(repo, s"histos.version=$pinned\n")
      _    <- ZIO.when(wrapper)(ZIO.attempt(Files.writeString(repo.resolve("histos"), "")))
      text <- writtenAt(repo)
    yield text

  private def writtenAt(repo: Path): ZIO[Any, Any, String] =
    Connection.ensure(repo) *> ZIO.attempt(Files.readString(repo.resolve(".bsp").resolve("histos.json")))

  private def plant(repo: Path, text: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      Files.createDirectories(repo.resolve(".histos"))
      Files.writeString(repo.resolve(LauncherPin.file), text)
    }

  private def work: ZIO[Scope, Any, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-bsp")))(path => Workspace.delete(path).ignore)
end ConnectionSpec
