package histos.js

import histos.cache.{ContentStore, LinkReplay, LinkerStarts, ProductCache}
import histos.jdk.{Sha256, Sha256Digest}
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildProcess, ChildRequest}
import histos.resolve.LockedTools
import histos.schedule.{CacheError, RemoteCache, RemoteError}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.CompileError
import histos.{Cell, JsOptimize, Platform}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

/** One cache for all JS callers. Products are immutable per effective mode and complete input key. */
object JsProducts:
  private val BridgeMain = MainClass.trusted("histos.js.child.LinkMain")

  def work(repo: Path, cell: Cell, effective: JsOptimize): Path =
    repo
      .resolve(".histos/link")
      .resolve(cell.module.value)
      .resolve(cell.version.value)
      .resolve("js")
      .resolve(effective.token)

  def link(
      request: JsLinkRequest
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    for
      view <- ZIO.service[LockedTools]
      selected = request.revision.getOrElse(view.catalog.js.revision)
      libraries <- ScalaJs.libraries(request.version, Some(selected)).mapError(JsLinkError.Library(_))
      host      <- hostClasspath
      classpath = request.classpath ++ libraries
      inputs <- fingerprints(classpath)
      tools  <- toolIdentity(request, host)
      key = JsKey.of(
        request.version,
        selected,
        request.kind,
        request.main,
        Nil,
        request.initialize,
        request.arguments,
        request.optimize,
        inputs,
        tools,
      )
      destination = request.work.resolve(s"${request.optimize.token}-${key.value}")
      store  <- ZIO.service[ContentStore]
      remote <- ZIO.service[RemoteCache]
      _      <- ProductCache.stage(remote, store, key).mapError(carried)
      hit    <- LinkReplay.load(store, key, destination).mapError(stored)
      script <- hit match
        case Some(path) => ZIO.succeed(path)
        case None       =>
          ZIO.scoped:
            for
              scratch    <- Workspace.work("histos-js-link").mapError(JsLinkError.Io(_))
              linked     <- produce(request, classpath, host, scratch)
              after      <- fingerprints(classpath)
              afterTools <- toolIdentity(request, host)
              _          <- ZIO.fail(JsLinkError.InputsChanged).unless(after == inputs && afterTools == tools)
              _          <- LinkReplay.save(store, key, linked).mapError(stored)
              placed     <- LinkReplay
                .load(store, key, destination)
                .mapError(stored)
                .someOrFail(JsLinkError.Bridge)
              _ <- ProductCache.publish(remote, store, key).mapError(carried)
            yield placed
    yield JsLinked(script, key)

  private def produce(
      request: JsLinkRequest,
      classpath: List[Path],
      host: ChildClasspath,
      scratch: Path,
  ): ZIO[LinkerStarts, JsLinkError, Path] =
    val process = scratch.resolve("process")
    val output  = scratch.resolve("out")
    val result  = scratch.resolve("js.path")
    val args    = List(
      result.toString,
      output.toString,
      request.kind.token,
      request.optimize.token,
      if request.initialize then "main" else "exports",
      request.main.binary,
      request.arguments.size.toString,
    ) ++
      request.arguments ++ classpath.map(_.toAbsolutePath.toString)
    for
      _     <- io(process, IoFault.Write)(Files.createDirectories(process))
      _     <- LinkerStarts.tick
      child <- ChildProcess
        .run(
          ChildRequest(Platform.Jvm, request.jdk.home.resolve("bin/java"), host, BridgeMain, process, arguments = args)
        )
        .mapError(JsLinkError.Child(_))
      _     <- ZIO.fail(JsLinkError.Linker(shown(child.out.text, child.err.text))).unless(child.exit.value == 0)
      path  <- io(result, IoFault.Read)(Path.of(Files.readString(result).trim))
      valid <- io(path, IoFault.Stat)(
        path.toAbsolutePath.normalize.startsWith(scratch.toAbsolutePath.normalize) && Files.isRegularFile(path)
      )
      _ <- ZIO.fail(JsLinkError.Bridge).unless(valid)
    yield path

  private def hostClasspath: IO[JsLinkError, ChildClasspath] =
    ZIO
      .fromOption(HostClasspath.child)
      .orElseFail(JsLinkError.Unavailable)
      .flatMap: host =>
        io(host.classes, IoFault.Stat)(host.entries.filter(Files.exists(_)).map(_.toAbsolutePath.normalize)).flatMap:
          case first :: rest => ZIO.succeed(ChildClasspath(first, rest))
          case Nil           => ZIO.fail(JsLinkError.Unavailable)

  private def toolIdentity(request: JsLinkRequest, host: ChildClasspath): IO[JsLinkError, List[Sha256]] =
    for
      content  <- fingerprints(host.entries)
      minifier <- request.optimize match
        case JsOptimize.Fast => ZIO.succeed(Nil)
        case JsOptimize.Full =>
          ZIO
            .fromEither(histos.splice.EsbuildNative.currentPin)
            .mapError(error => JsLinkError.Minifier(error.message))
            .map(pin => List(textDigest(JsKey.encoded(List(histos.splice.EsbuildNative.Version, pin.id, pin.sha256)))))
      runtime = textDigest(Runtimes.identityToken(request.jdk.identity))
    yield runtime :: content ++ minifier

  private def fingerprints(paths: List[Path]): IO[JsLinkError, List[Sha256]] =
    ZIO.foreach(paths)(fingerprint)

  private def fingerprint(path: Path): IO[JsLinkError, Sha256] =
    for
      root    <- io(path, IoFault.Stat)(path.toRealPath())
      entries <- io(root, IoFault.Walk) {
        if Files.isDirectory(root) then
          val walk = Files.walk(root)
          try walk.iterator.asScala.toList.sortBy(file => root.relativize(file).toString)
          finally walk.close()
        else List(root)
      }
      rows <- ZIO.foreach(entries): file =>
        for
          symbolic <- io(file, IoFault.Stat)(Files.isSymbolicLink(file))
          _        <- ZIO.fail(JsLinkError.Io(IoFailure(file, IoFault.Read))).when(symbolic)
          regular  <- io(file, IoFault.Stat)(Files.isRegularFile(file))
          row      <-
            if regular then
              fileDigest(file).map: digest =>
                val name = if root == file then "" else root.relativize(file).toString
                Some(JsKey.encoded(List(name, digest.value)))
            else ZIO.none
        yield row
    yield textDigest(JsKey.encoded(path.toAbsolutePath.normalize.toString :: root.toString :: rows.flatten))

  private def fileDigest(path: Path): IO[JsLinkError, Sha256] =
    ZIO.scoped:
      for
        input <- ZIO.acquireRelease(io(path, IoFault.Read)(Files.newInputStream(path)))(input =>
          ZIO.attemptBlocking(input.close()).ignore
        )
        digest <- io(path, IoFault.Read)(MessageDigest.getInstance("SHA-256"))
        buffer = new Array[Byte](65536)
        read   = io(path, IoFault.Read)(input.read(buffer)).flatMap: size =>
          if size < 0 then ZIO.succeed(false)
          else
            ZIO.succeed:
              digest.update(buffer, 0, size)
              true
        _ <- read.repeatWhile(identity)
      yield Sha256.fromDigest(digest.digest())

  private def textDigest(text: String): Sha256 =
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  private def io[A](path: Path, fault: IoFault)(body: => A): IO[JsLinkError, A] =
    ZIO.attemptBlocking(body).mapError(_ => JsLinkError.Io(IoFailure(path, fault)))

  private def stored(error: CacheError): JsLinkError =
    JsLinkError.Product(CompileError.Stored(error))

  private def carried(error: RemoteError | CacheError): JsLinkError = error match
    case found: RemoteError => JsLinkError.Product(CompileError.Remote(found))
    case found: CacheError  => stored(found)

  private def shown(out: String, err: String): String =
    val text = List(out, err).filter(_.nonEmpty).mkString("\n")
    if text.isEmpty then "the js linker failed" else text
end JsProducts
