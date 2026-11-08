package histos.load

import histos.pack.ClassJar
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** The api boundary jar a loader spec hands to `Loader`. `prepare` hashes the path with `Files.readAllBytes`, so a
  * classes directory is zipped first.
  */
object LoaderApiJar:
  def of(cls: Class[?], dest: Path): IO[LoadError, Path] =
    Boundary.codeSource(cls) match
      case None       => ZIO.fail(LoadError.MissingFile(dest))
      case Some(path) =>
        ZIO
          .attemptBlocking(Files.isRegularFile(path))
          .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Stat)))
          .flatMap: file =>
            if file then ZIO.succeed(path)
            else
              ClassJar
                .write(path, dest)
                .mapBoth(_ => LoadError.Io(IoFailure(dest, IoFault.Write)), _.path)
