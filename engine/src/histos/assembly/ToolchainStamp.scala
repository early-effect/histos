package histos.assembly

import histos.{ApiClock, SnapshotId}
import histos.zinc.Sources

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** Writes the ship revision assembly compiles into the toolchain. A checkout with no `app` ship is left alone. */
object ToolchainStamp:
  def write(repo: Path): IO[AssembleError, Unit] =
    SnapshotId.current(repo).flatMap {
      case None           => ZIO.unit
      case Some(revision) =>
        val path = Sources.unmanagedRoot(repo, "wire").resolve("histos").resolve("Toolchain.scala")
        ZIO
          .attemptBlocking {
            Files.createDirectories(path.getParent)
            Files.writeString(path, ApiClock.source(revision), StandardCharsets.UTF_8)
          }
          .mapError(_ => AssembleError.Io(path))
          .unit
    }
