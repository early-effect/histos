package histos.native

import histos.jdk.Sha256
import histos.process.Process
import histos.resolve.LockedTools
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.{Files, InvalidPathException, Path}

import zio.{IO, ZIO, ZLayer}

/** Link Scala Native inputs to one binary. The live layer is the only place that runs clang and the linker. */
trait NativeLinker:
  def link(request: NativeLinkRequest, sourcesRoot: Path): IO[NativeError, Linked]

  def linkClasspath(request: NativeLinkRequest, classpath: List[Path]): IO[NativeError, Path]
end NativeLinker

object NativeLinker:
  val live: ZLayer[LockedTools & Process, Nothing, NativeLinker] =
    ZLayer.fromZIO:
      for
        tools   <- ZIO.service[LockedTools]
        process <- ZIO.service[Process]
      yield new NativeLinker:
        def link(request: NativeLinkRequest, sourcesRoot: Path): IO[NativeError, Linked] =
          NativeLink
            .link(request, sourcesRoot)
            .provide(ZLayer.succeed(tools) ++ ZLayer.succeed(process))

        def linkClasspath(request: NativeLinkRequest, classpath: List[Path]): IO[NativeError, Path] =
          NativeLink
            .linkClasspath(request, classpath)
            .provide(ZLayer.succeed(tools) ++ ZLayer.succeed(process))

  /** A work directory that already holds `key` and its binary is that link. Anything else links once and records it.
    * The key is the caller's: clang's version is part of a native key, and this method does not probe.
    */
  def reuse(
      key: Sha256,
      request: NativeLinkRequest,
      classpath: List[Path],
  ): ZIO[NativeLinker, NativeError, Path] =
    cached(request.work, key).flatMap:
      case Some(binary) => ZIO.succeed(binary)
      case None         =>
        for
          _      <- Workspace.recreate(request.work).mapError(failed)
          binary <- ZIO.serviceWithZIO[NativeLinker](_.linkClasspath(request, classpath))
          _      <- remember(request.work, key, binary)
        yield binary

  private def cached(work: Path, key: Sha256): IO[NativeError, Option[Path]] =
    for
      rawKey    <- Workspace.readIfPresent(work.resolve(KeyFile)).mapError(failed)
      rawBinary <- Workspace.readIfPresent(work.resolve(BinaryFile)).mapError(failed)
      found     <- (rawKey.flatMap(text => Sha256.from(text).toOption), rawBinary.flatMap(pathOf)) match
        case (Some(stored), Some(binary)) if stored == key =>
          present(binary).map(ok => if ok then Some(binary) else None)
        case _ => ZIO.succeed(None)
    yield found

  private def remember(work: Path, key: Sha256, binary: Path): IO[NativeError, Unit] =
    Workspace.writeString(work.resolve(KeyFile), key.value).mapError(failed) *>
      Workspace
        .writeString(work.resolve(BinaryFile), binary.toAbsolutePath.toString)
        .mapError(failed)

  private def present(path: Path): IO[NativeError, Boolean] =
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .mapError(_ => failed(IoFailure(path, histos.workspace.IoFault.Stat)))

  private def pathOf(raw: String): Option[Path] =
    try Some(Path.of(raw.trim))
    catch case _: InvalidPathException => None

  private def failed(failure: IoFailure): NativeError =
    NativeError.Link(LinkError.Linker(failure.message))

  private val KeyFile    = "link.key"
  private val BinaryFile = "link.binary"
end NativeLinker
