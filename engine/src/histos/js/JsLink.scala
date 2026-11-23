package histos.js

import histos.cache.{ContentStore, LinkReplay, LinkerStarts, ProductCache}
import histos.jdk.{JdkRuntime, Sha256}
import histos.pack.MainClass
import histos.process.{ChildProcess, ChildRequest}
import histos.schedule.{CacheError, RemoteCache, RemoteError}
import histos.workspace.Workspace
import histos.zinc.{CompileError, GraphCompile, ProductDigest}
import histos.ScalaVersion

import histos.resolve.LockedTools

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** One link of `.sjsir` into a script. The engine does not load `org.scalajs.linker`.
  */
final case class JsLinkRequest(
    version: ScalaVersion,
    main: MainClass,
    classpath: List[Path],
    work: Path,
    jdk: JdkRuntime,
    kind: histos.JsModuleKind = histos.JsModuleKind.NoModule,
    revision: Option[histos.JsVersion] = None,
    initialize: Boolean = true,
    arguments: List[String] = Nil,
    optimize: histos.JsOptimize = histos.JsOptimize.Fast,
)

/** Link `.sjsir` in a child JVM. The linker classes are the ones histos was compiled against.
  */
final case class JsLinked(script: Path, key: Sha256)

object JsLink:
  private val BridgeMain: String = "histos.js.child.LinkMain"

  def link(
      request: JsLinkRequest
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    val JsLinkRequest(version, main, classpath, _, _, kind, revision, initialize, arguments, optimize) =
      request
    for
      view <- ZIO.service[LockedTools]
      selected = revision.getOrElse(view.catalog.js.revision)
      digests <- productsOf(classpath)
      key = JsKey.of(version, selected, kind, main, digests, initialize, arguments, optimize)
      store  <- ZIO.service[ContentStore]
      remote <- ZIO.service[RemoteCache]
      _      <- ProductCache.stage(remote, store, key).mapError(carried)
      hit    <- LinkReplay.load(store, key, request.work).mapError(stored)
      script <- hit match
        case Some(path) => ZIO.succeed(path)
        case None       =>
          produce(request, selected).tap: path =>
            LinkReplay.save(store, key, path).mapError(stored) *>
              ProductCache.publish(remote, store, key).mapError(carried)
    yield JsLinked(script, key)

  private def stored(error: CacheError): JsLinkError =
    JsLinkError.Product(CompileError.Stored(error))

  private def carried(error: RemoteError | CacheError): JsLinkError =
    error match
      case found: RemoteError => JsLinkError.Product(CompileError.Remote(found))
      case found: CacheError  => JsLinkError.Product(CompileError.Stored(found))

  private def produce(
      request: JsLinkRequest,
      selected: histos.JsVersion,
  ): ZIO[LockedTools & LinkerStarts, JsLinkError, Path] =
    val JsLinkRequest(version, main, classpath, work, jdk, kind, _, initialize, arguments, optimize) =
      request
    val output   = work.resolve("out")
    val mainName = if initialize then Some(main.binary) else None
    for
      libraries <- ScalaJs
        .libraries(version, Some(selected))
        .mapError(JsLinkError.Library(_))
      _      <- empty(output)
      _      <- LinkerStarts.tick
      linked <- ZIO
        .attemptBlocking(Linker.link(classpath ++ libraries, output, kind, mainName, arguments, optimize))
        .mapError(error => JsLinkError.Linker(Option(error.getMessage).getOrElse("the js linker failed")))
        .flatMap(ZIO.fromEither(_).mapError(JsLinkError.Linker(_)))
      script <- optimize match
        case histos.JsOptimize.Fast => ZIO.succeed(linked)
        case histos.JsOptimize.Full =>
          histos.splice.Splice
            .full(linked, work.resolve("full.js"))
            .mapError(error => JsLinkError.Linker(error.message))
    yield script

  private def productsOf(classpath: List[Path]): IO[JsLinkError, List[Sha256]] =
    ZIO.foreach(classpath.flatMap(path => GraphCompile.analysisDirOf(path).toList)): dir =>
      ZIO.fromEither(ProductDigest.read(dir)).mapError(JsLinkError.Product(_))

  private def readJs(path: Path): IO[JsLinkError, Path] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Right(Path.of(Files.readString(path).trim))
        else Left(JsLinkError.Linker("the js bridge did not write a script"))
      }
      .mapError(_ => JsLinkError.Linker(s"could not read $path"))
      .flatMap(ZIO.fromEither)

  private def write(path: Path, text: String): IO[JsLinkError, Unit] =
    Workspace
      .writeString(path, text)
      .mapError(_ => JsLinkError.Linker(s"could not write $path"))

  private def empty(path: Path): IO[JsLinkError, Unit] =
    Workspace
      .recreate(path)
      .mapError(_ => JsLinkError.Linker(s"could not prepare $path"))
      .unit

  private def shown(out: String, err: String): String =
    val text = List(out, err).filter(_.nonEmpty).mkString("\n")
    if text.isEmpty then "the js linker failed" else text
end JsLink
