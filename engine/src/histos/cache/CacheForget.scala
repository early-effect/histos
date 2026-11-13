package histos.cache

import histos.schedule.CacheError
import histos.workspace.{IoFailure, IoFault}
import histos.ProjectName

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** Drop one module's compile hits from the local store and delete the markers, so the next compile of the same tree
  * cannot recall them from disk.
  */
private[histos] object CacheForget:
  def module(repo: Path, store: ContentStore, name: ProjectName): IO[CacheError | IoFailure, Unit] =
    ReplayMarks
      .collect(repo, Some(Set(name)), coverage = true)
      .flatMap: marks =>
        ZIO.foreachDiscard(marks): mark =>
          store.remove(mark.key) *>
            ZIO
              .attemptBlocking(Files.deleteIfExists(mark.marker))
              .mapError(_ => IoFailure(mark.marker, IoFault.Delete))
              .unit
