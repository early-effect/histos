package histos.js

import histos.cache.{ContentStore, LinkReplay, LinkerStarts, ProductCache}
import histos.jdk.Sha256
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest, RelayCommand, RunRelay}
import histos.resolve.LockedTools
import histos.schedule.{CacheError, RemoteCache, RemoteError}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.CompileError
import histos.{Cell, JsOptimize, Platform}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Queue, Scope, ZIO}
import zio.json.*

/** A scoped, copied tool generation. Reuse it for a build invocation, then recapture on reload. */
final class JsLinkSession private[js] (private[js] val tools: List[JsInput]):
  val toolBytes: Long = tools.map(_.bytes).sum

  def link(
      request: JsLinkRequest
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    JsProducts.inSession(request, tools, None)
end JsLinkSession

/** One cache for all JS callers. Identity and child consumption share the same owned input generation. */
object JsProducts:
  private val BridgeMain = MainClass.trusted("histos.js.child.LinkMain")

  def work(repo: Path, cell: Cell, effective: JsOptimize): Path =
    repo
      .resolve(".histos/link")
      .resolve(cell.module.value)
      .resolve(cell.version.value)
      .resolve("js")
      .resolve(effective.token)

  def session: ZIO[Scope, JsLinkError, JsLinkSession] =
    for
      owned <- Workspace.work("histos-js-tools").mapError(JsLinkError.Io(_))
      host  <- hostClasspath
      tools <- JsInputs.capture(host.entries, owned)
    yield new JsLinkSession(tools)

  def link(
      request: JsLinkRequest
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    observed(request)(None)

  private[js] def observed(request: JsLinkRequest)(
      observer: Option[JsReady => IO[JsLinkError, Unit]]
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    ZIO.scoped(session.flatMap(generation => inSession(request, generation.tools, observer)))

  private[js] def inSession(
      request: JsLinkRequest,
      tools: List[JsInput],
      observer: Option[JsReady => IO[JsLinkError, Unit]],
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    ZIO.scoped:
      for
        scratch <- Workspace.work("histos-js-link").mapError(JsLinkError.Io(_))
        view    <- ZIO.service[LockedTools]
        selected = request.revision.getOrElse(view.catalog.js.revision)
        libraries  <- ScalaJs.libraries(request.version, Some(selected)).mapError(JsLinkError.Library(_))
        inputs     <- JsInputs.capture(request.classpath ++ libraries, scratch.resolve("inputs"))
        identities <- toolIdentity(request, tools)
        key = JsKey.of(
          request.version,
          selected,
          request.kind,
          request.main,
          Nil,
          request.initialize,
          request.arguments,
          request.optimize,
          inputs.map(_.digest),
          identities,
        )
        destination = request.work.resolve(s"${request.optimize.token}-${key.value}")
        store  <- ZIO.service[ContentStore]
        remote <- ZIO.service[RemoteCache]
        _      <- ProductCache.stage(remote, store, key).mapError(carried)
        hit    <- LinkReplay.load(store, key, destination).mapError(stored)
        script <- hit match
          case Some(path) => ZIO.succeed(path)
          case None       =>
            for
              _ <- ZIO.foreachDiscard(observer)(notify =>
                JsBridge.ready(JsPhase.Captured, scratch.resolve("js.result"), scratch.resolve("out")).flatMap(notify)
              )
              host   <- childClasspath(tools.map(_.path))
              linked <- produce(request, inputs.map(_.path), host, scratch, observer)
              _      <- JsInputs.unchanged(inputs)
              _      <- LinkReplay.save(store, key, linked).mapError(stored)
              placed <- LinkReplay.load(store, key, destination).mapError(stored).someOrFail(JsLinkError.Bridge)
              _      <- ProductCache.publish(remote, store, key).mapError(carried)
            yield placed
      yield JsLinked(script, key)

  private def produce(
      request: JsLinkRequest,
      classpath: List[Path],
      host: ChildClasspath,
      scratch: Path,
      observer: Option[JsReady => IO[JsLinkError, Unit]],
  ): ZIO[LinkerStarts, JsLinkError, Path] =
    val process   = scratch.resolve("process")
    val output    = scratch.resolve("out")
    val result    = scratch.resolve("js.result")
    val temporary = scratch.resolve("temporary")
    val args      = List(
      result.toString,
      output.toString,
      request.kind.token,
      request.optimize.token,
      if request.initialize then "main" else "exports",
      request.main.binary,
      if observer.nonEmpty then "observed" else "ordinary",
      request.arguments.size.toString,
    ) ++
      request.arguments ++ classpath.map(_.toString)
    for
      _      <- io(process, IoFault.Create)(Files.createDirectories(process))
      _      <- io(temporary, IoFault.Create)(Files.createDirectories(temporary))
      _      <- LinkerStarts.tick
      script <- child(
        ChildRequest(
          Platform.Jvm,
          request.jdk.home.resolve("bin/java"),
          host,
          BridgeMain,
          process,
          environment = temporaryEnvironment(temporary),
          arguments = args,
        ),
        result,
        observer,
      )
      valid <- io(script, IoFault.Stat)(
        script.toAbsolutePath.normalize.startsWith(scratch.toAbsolutePath.normalize) && Files.isRegularFile(script)
      )
      _ <- ZIO.fail(JsLinkError.Bridge).unless(valid)
    yield script

  private[js] def temporaryEnvironment(root: Path): Map[String, String] =
    val quoted = "\"" + root.toAbsolutePath.normalize.toString.replace("\"", "\"'\"'\"") + "\""
    Map("JAVA_TOOL_OPTIONS" -> ("-Djava.io.tmpdir=" + quoted))

  private[js] def child(
      request: ChildRequest,
      result: Path,
      observer: Option[JsReady => IO[JsLinkError, Unit]] = None,
  ): IO[JsLinkError, Path] =
    ZIO.scoped:
      for
        queue <- ZIO.acquireRelease(Queue.unbounded[RelayCommand])(_.shutdown)
        relay = if observer.nonEmpty then RunRelay.Open(queue.take.map(Some(_))) else RunRelay.Closed
        events <- ChildProcess
          .events(request, relay)
          .mapError(JsLinkError.Child(_))
          .tap: event =>
            event match
              case ChildEvent.Out(text) =>
                ZIO.foreachDiscard(observer): notify =>
                  ZIO.fromEither(text.fromJson[JsReady]).mapError(_ => JsLinkError.Bridge).flatMap(notify) *>
                    queue
                      .offer(
                        RelayCommand
                          .Stdin(Chunk.fromArray((JsResume.Continue.toJson + "\n").getBytes(StandardCharsets.UTF_8)))
                      )
                      .unit
              case ChildEvent.Err(_) | ChildEvent.Bytes(_) | ChildEvent.Exited(_) => ZIO.unit
          .runCollect
        exit <- ZIO
          .fromOption(events.collect { case ChildEvent.Exited(code) => code }.lastOption)
          .orElseFail(JsLinkError.Child(ChildError.Status))
        _        <- ZIO.fail(JsLinkError.Child(ChildError.Status)).unless(exit.value == 0)
        text     <- io(result, IoFault.Read)(Files.readString(result))
        reported <- ZIO.fromEither(text.fromJson[JsResult]).mapError(_ => JsLinkError.Bridge)
        script   <- reported match
          case JsResult.Script(path)                     => ZIO.attempt(Path.of(path)).mapError(_ => JsLinkError.Bridge)
          case JsResult.Failed(JsLinkError.Linker(text)) =>
            val diagnostics = events.collect { case ChildEvent.Err(line) => line }.mkString("\n")
            ZIO.fail(JsLinkError.Linker(text + "\n" + diagnostics))
          case JsResult.Failed(error) => ZIO.fail(error)
      yield script

  private def hostClasspath: IO[JsLinkError, ChildClasspath] =
    ZIO
      .fromOption(HostClasspath.child)
      .orElseFail(JsLinkError.Unavailable)
      .flatMap: host =>
        io(host.classes, IoFault.Stat)(host.entries.filter(Files.exists(_)).map(_.toAbsolutePath.normalize))
          .flatMap(childClasspath)

  private def childClasspath(paths: List[Path]): IO[JsLinkError, ChildClasspath] = paths match
    case first :: rest => ZIO.succeed(ChildClasspath(first, rest))
    case Nil           => ZIO.fail(JsLinkError.Unavailable)

  private def toolIdentity(request: JsLinkRequest, tools: List[JsInput]): IO[JsLinkError, List[Sha256]] =
    for
      minifier <- request.optimize match
        case JsOptimize.Fast => ZIO.succeed(Nil)
        case JsOptimize.Full =>
          ZIO
            .fromEither(histos.splice.EsbuildNative.currentPin)
            .mapError(error => JsLinkError.Minifier(error.message))
            .map(pin =>
              List(JsInputs.textDigest(JsKey.encoded(List(histos.splice.EsbuildNative.Version, pin.id, pin.sha256))))
            )
      runtime = JsInputs.textDigest(Runtimes.identityToken(request.jdk.identity))
    yield runtime :: tools.map(_.digest) ++ minifier

  private def io[A](path: Path, fault: IoFault)(body: => A): IO[JsLinkError, A] =
    ZIO.attemptBlockingInterrupt(body).mapError(_ => JsLinkError.Io(IoFailure(path, fault)))

  private def stored(error: CacheError): JsLinkError = JsLinkError.Product(CompileError.Stored(error))

  private def carried(error: RemoteError | CacheError): JsLinkError = error match
    case found: RemoteError => JsLinkError.Product(CompileError.Remote(found))
    case found: CacheError  => stored(found)
end JsProducts
