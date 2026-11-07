package histos.archive

import histos.workspace.Workspace

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import zio.{Exit, ZIO}
import zio.test.*

object ArchiveLawSpec extends ZIOSpecDefault:
  def spec = suite("archive laws")(
    test(
      "a file is not a runtime cache, a shared directory is refused, and a private directory is not refused as itself"
    ):
      ZIO.scoped:
        Workspace
          .work("histos-archive")
          .flatMap: root =>
            val file   = root.resolve("file")
            val shared = root.resolve("shared")
            val closed = root.resolve("closed")
            for
              _        <- Workspace.writeString(file, "x")
              fileExit <- RuntimeCache.prepare(file).exit
              _        <- mode(shared, "rwxr-xr-x")
              open     <- RuntimeCache.prepare(shared).exit
              _        <- mode(closed, "rwx------")
              shut     <- RuntimeCache.prepare(closed).exit
            yield assertTrue(
              failed(fileExit, RuntimeCacheError.NotDirectory(file)),
              failed(open, RuntimeCacheError.Shared(shared)),
              shut match
                case Exit.Success(_)     => true
                case Exit.Failure(cause) =>
                  cause.failureOption match
                    case Some(RuntimeCacheError.Shared(path)) => path != closed
                    case Some(RuntimeCacheError.Owner(path))  => path != closed
                    case _                                    => false,
            )
  )

  private def failed(exit: Exit[RuntimeCacheError, Unit], error: RuntimeCacheError): Boolean =
    exit match
      case Exit.Failure(cause) => !cause.isDie && cause.failureOption.contains(error)
      case Exit.Success(_)     => false

  private def mode(path: Path, text: String): zio.UIO[Unit] =
    ZIO
      .attemptBlocking:
        Files.createDirectory(path)
        Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(text))
        ()
      .orDie
end ArchiveLawSpec
