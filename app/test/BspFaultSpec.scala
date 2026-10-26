package histos.bsp

import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.CompileProblem

import java.nio.file.Files

import zio.{Chunk, ZIO}
import zio.test.*

object BspFaultSpec extends ZIOSpecDefault:
  def spec = suite("bsp faults")(
    test("a file cannot be the parent of a directory"):
      ZIO.scoped {
        for
          dir <- Workspace.work("bsp-io")
          parent = dir.resolve("parent")
          child  = parent.resolve("child")
          _ <- ZIO
            .attemptBlocking(Files.writeString(parent, "x"))
            .mapError(_ => IoFailure(parent, IoFault.Write))
          failed <- Bsp.ensureDir(child).either
        yield assertTrue(failed == Left(Bsp.BspError.Io(IoFailure(child, IoFault.Create))))
      }
    ,
    test("a failed compile keeps heard problems in order"):
      val first  = CompileProblem.text("one")
      val second = CompileProblem.text("two")
      Bsp
        .problems { hear =>
          ZIO.succeed(hear(first)) *> ZIO.succeed(hear(second)) *> ZIO.fail(Bsp.BspError.UnknownTarget)
        }
        .map { (result, found) =>
          assertTrue(
            result == Left(Bsp.BspError.UnknownTarget),
            found == Chunk(first, second),
          )
        },
  )
