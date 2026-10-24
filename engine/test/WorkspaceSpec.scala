package histos.workspace

import histos.resolve.{Resolve, ResolveError}

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions

import zio.{Scope, ZIO}
import zio.test.*

object WorkspaceSpec extends ZIOSpecDefault:
  def spec = suite("workspace")(
    test(
      "a missing dependency lock names the path, and an unreadable one is a read fault"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-lock")
          path = Resolve.lockPath(root)
          missing <- Resolve.readLock(root).either
          _ <- ZIO.attempt:
            Files.createDirectories(path.getParent)
            Files.writeString(path, "x")
            Files.setPosixFilePermissions(
              path,
              PosixFilePermissions.fromString("---------")
            )
          failed <- Resolve.readLock(root).either
          _ <- ZIO.attempt(
            Files.setPosixFilePermissions(
              path,
              PosixFilePermissions.fromString("rw-------")
            )
          )
        yield assertTrue(
          missing == Left(ResolveError.NoLock(path)),
          failed match
            case Left(ResolveError.Io(IoFailure(found, IoFault.Read))) =>
              found == path
            case _ => false
        )
    ,
    test("a lock that is not a lock keeps the parse error"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-bad-lock")
          path = Resolve.lockPath(root)
          _ <- ZIO.attempt:
            Files.createDirectories(path.getParent)
            Files.writeString(path, "not a lock\n")
          failed <- Resolve.readLock(root).either
        yield assertTrue:
          failed match
            case Left(ResolveError.Lock(_)) => true
            case _                          => false
    ,
    test("a work directory is removed when its scope closes"):
      for
        path <- ZIO.scoped(Workspace.work("histos-work"))
        gone <- ZIO.attempt(Files.exists(path))
      yield assertTrue(!gone)
    ,
    test("delete removes a nested tree"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tree")
          file = root.resolve("a").resolve("b.txt")
          _ <- ZIO.attempt:
            Files.createDirectories(file.getParent)
            Files.writeString(file, "x")
          _ <- Workspace.delete(root.resolve("a"))
          nested <- ZIO.attempt(Files.exists(file))
          parent <- ZIO.attempt(Files.exists(root))
        yield assertTrue(!nested, parent)
    ,
    test("writeString creates parents and recreate removes old contents"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-write")
          file = root.resolve("a").resolve("b.txt")
          _ <- Workspace.writeString(file, "x")
          before <- ZIO.attempt(Files.readString(file))
          _ <- Workspace.recreate(root.resolve("a"))
          after <- ZIO.attempt(Files.exists(file))
        yield assertTrue(before == "x", !after)
  )
