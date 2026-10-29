package histos.js

import histos.jdk.Sha256
import histos.resolve.LockedTools
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.{Files, InvalidPathException, Path}

import zio.{IO, ZIO, ZLayer}

/** Link `.sjsir` to one script. The live layer is the only place that loads the linker. */
trait Linker:
  def link(request: JsLinkRequest): IO[JsLinkError, JsLinked]

object Linker:
  val live: ZLayer[LockedTools, Nothing, Linker] =
    ZLayer.fromZIO:
      ZIO.serviceWith[LockedTools]: tools =>
        new Linker:
          def link(request: JsLinkRequest): IO[JsLinkError, JsLinked] =
            JsLink.link(request).provide(ZLayer.succeed(tools))

  /** Product digests decide the key, then [[reuse]]. */
  def reuse(request: JsLinkRequest): ZIO[Linker & LockedTools, JsLinkError, JsLinked] =
    JsLink.keyOf(request).flatMap(key => reuse(key, request))

  /** A work directory that already holds `key` and its script is that link. Anything else links once and records it.
    */
  def reuse(key: Sha256, request: JsLinkRequest): ZIO[Linker, JsLinkError, JsLinked] =
    cached(request.work, key).flatMap:
      case Some(script) => ZIO.succeed(JsLinked(script, key))
      case None         =>
        for
          _      <- Workspace.recreate(request.work).mapError(failed)
          linked <- ZIO.serviceWithZIO[Linker](_.link(request))
          _      <- remember(request.work, key, linked.script)
        yield linked

  private def cached(work: Path, key: Sha256): IO[JsLinkError, Option[Path]] =
    for
      rawKey    <- Workspace.readIfPresent(work.resolve(KeyFile)).mapError(failed)
      rawScript <- Workspace.readIfPresent(work.resolve(ScriptFile)).mapError(failed)
      found     <- (rawKey.flatMap(text => Sha256.from(text).toOption), rawScript.flatMap(pathOf)) match
        case (Some(stored), Some(script)) if stored == key =>
          present(script).map(ok => if ok then Some(script) else None)
        case _ => ZIO.succeed(None)
    yield found

  private def remember(work: Path, key: Sha256, script: Path): IO[JsLinkError, Unit] =
    Workspace.writeString(work.resolve(KeyFile), key.value).mapError(failed) *>
      Workspace
        .writeString(work.resolve(ScriptFile), script.toAbsolutePath.toString)
        .mapError(failed)

  private def present(path: Path): IO[JsLinkError, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => failed(IoFailure(path, histos.workspace.IoFault.Stat)))

  private def pathOf(raw: String): Option[Path] =
    try Some(Path.of(raw.trim))
    catch case _: InvalidPathException => None

  private def failed(failure: IoFailure): JsLinkError =
    JsLinkError.Linker(failure.message)

  private val KeyFile    = "link.key"
  private val ScriptFile = "link.script"
end Linker
