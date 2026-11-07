package histos.workspace

import java.nio.file.{Files, Path}

import zio.{Exit, ZIO}
import zio.test.*

object WorkspaceLawSpec extends ZIOSpecDefault:
  def spec = suite("workspace laws")(
    test("a written string reads back, a missing file is a read fault, and a directory is absent"):
      ZIO.scoped:
        Workspace
          .work("histos-workspace")
          .flatMap: root =>
            val file = root.resolve("lock").resolve("dependencies.lock")
            val dir  = root.resolve("dir")
            for
              _       <- Workspace.writeString(file, "cell\n")
              text    <- Workspace.readString(file)
              present <- Workspace.readIfPresent(file)
              _       <- Workspace.createDirectories(dir)
              folder  <- Workspace.readIfPresent(dir)
              missing <- Workspace.readString(root.resolve("gone")).exit
              absent  <- Workspace.readIfPresent(root.resolve("gone"))
            yield assertTrue(
              text == "cell\n",
              present == Some("cell\n"),
              folder == None,
              absent == None,
              missing match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(IoFailure(root.resolve("gone"), IoFault.Read))
                case Exit.Success(_) => false,
            )
    ,
    test("deleting a path that is not there succeeds, and a work directory is removed with its scope"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-workspace-scope")
          file = root.resolve("note")
          _     <- Workspace.writeString(file, "x")
          _     <- Workspace.delete(root.resolve("missing"))
          gone  <- ZIO.scoped(Workspace.work("histos-workspace-gone"))
          after <- ZIO.attemptBlocking(Files.notExists(gone)).orDie
          still <- ZIO.attemptBlocking(Files.isRegularFile(file)).orDie
          held  <- ZIO.attemptBlocking(Files.isDirectory(root)).orDie
        yield assertTrue(after, still, held),
  )
