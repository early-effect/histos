package histos.bsp

import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Resolve, ResolveCell, ResolveError}
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, JvmCompile, ResolvedJdk, ScalaSeries}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.json.*
import zio.json.ast.Json
import zio.{IO, Ref, UIO, ZIO}

/** BSP 2.2. Compile, sources, and scalac options. No test or run until those requests exist. */
object Bsp:
  val BspVersion: String = "2.2.0"
  val ServerName: String = "histos"
  val ServerVersion: String = "0.0.1-SNAPSHOT"

  enum Answer:
    case Reply(line: String)
    case Quiet
    case Stop(code: Int)

  private enum Phase:
    case New, Ready, Shutdown

  private enum ParamFault:
    case Missing, NoTargets, NoUri

    def message: String = this match
      case Missing   => "bsp params are required"
      case NoTargets => "bsp targets are required"
      case NoUri     => "a build target needs a uri"

  private enum BspError:
    case Parse
    case NotRequest
    case NoMethod
    case NotInitialized
    case ShutDown
    case UnknownMethod(method: String)
    case Params(fault: ParamFault)
    case UnknownTarget
    case Lock(error: ResolveError)
    case LockRead(path: Path)
    case Resolve(error: ResolveError)
    case Toolchain(error: CompileError)
    case Sources(error: GraphCompileError)

    def code: Int = this match
      case Parse                                  => -32700
      case NotRequest | NoMethod | ShutDown       => -32600
      case UnknownMethod(_)                       => -32601
      case Params(_) | UnknownTarget              => -32602
      case NotInitialized                         => -32002
      case Lock(_) | LockRead(_) | Resolve(_) | Toolchain(_) | Sources(_) => -32603

    def message: String = this match
      case Parse                 => "parse error"
      case NotRequest            => "bsp requires a json-rpc object"
      case NoMethod              => "bsp requires a method"
      case NotInitialized        => "bsp is not initialized"
      case ShutDown              => "bsp has shut down"
      case UnknownMethod(method) => s"bsp does not handle $method"
      case Params(fault)         => fault.message
      case UnknownTarget         => "unknown build target"
      case Lock(error)           => error.message
      case LockRead(path)        => s"could not read $path"
      case Resolve(error)        => error.message
      case Toolchain(error)      => error.message
      case Sources(error)        => error.message

  def open(repo: Path, graph: ProjectGraph, jdk: ResolvedJdk): UIO[Session] =
    Ref.make[Phase](Phase.New).map(phase => Session.start(repo, graph, jdk, phase))

  object Session:
    private[Bsp] def start(repo: Path, graph: ProjectGraph, jdk: ResolvedJdk, phase: Ref[Phase]): Session =
      new Session(repo, graph, jdk, phase)

  final class Session private (repo: Path, graph: ProjectGraph, jdk: ResolvedJdk, phase: Ref[Phase]):
    def answer(payload: String): ZIO[Resolve, Nothing, Answer] =
      payload.fromJson[Json] match
        case Left(_) => ZIO.succeed(Answer.Reply(failure(Json.Null, BspError.Parse)))
        case Right(json) =>
          json match
            case Json.Obj(_) =>
              val id = field(json, "id").getOrElse(Json.Null)
              method(json) match
                case None => ZIO.succeed(Answer.Reply(failure(id, BspError.NoMethod)))
                case Some(name) =>
                  phase.get.flatMap(current => dispatch(current, id, name, json, field(json, "id").isEmpty))
            case _ => ZIO.succeed(Answer.Reply(failure(Json.Null, BspError.NotRequest)))

    private def dispatch(
        current: Phase,
        id: Json,
        name: String,
        json: Json,
        notification: Boolean,
    ): ZIO[Resolve, Nothing, Answer] =
      (current, name) match
        case (_, "build/exit") =>
          ZIO.succeed(Answer.Stop(if current == Phase.Shutdown then 0 else 1))
        case (Phase.New, "build/initialize") if !notification =>
          phase.set(Phase.Ready).as(Answer.Reply(result(id, initialize)))
        case (Phase.New, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.New, _) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.NotInitialized)))
        case (Phase.Shutdown, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Shutdown, _) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.ShutDown)))
        case (Phase.Ready, "build/initialized") =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, "build/shutdown") if !notification =>
          phase.set(Phase.Shutdown).as(Answer.Reply(result(id, Json.Null)))
        case (Phase.Ready, "workspace/buildTargets") if !notification =>
          reply(id, targets)
        case (Phase.Ready, "buildTarget/sources") if !notification =>
          sources(id, json)
        case (Phase.Ready, "buildTarget/scalacOptions") if !notification =>
          options(id, json)
        case (Phase.Ready, "buildTarget/compile") if !notification =>
          compile(id, json)
        case (Phase.Ready, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, other) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownMethod(other))))

    private def sources(id: Json, json: Json): ZIO[Resolve, Nothing, Answer] =
      reply(id, asked(json).flatMap(uris => ZIO.foreach(uris)(sourceItem).map(items => Json.Obj("items" -> Json.Arr(items*)))))

    private def options(id: Json, json: Json): ZIO[Resolve, Nothing, Answer] =
      reply(id, asked(json).flatMap(uris => ZIO.foreach(uris)(scalac).map(items => Json.Obj("items" -> Json.Arr(items*)))))

    private def compile(id: Json, json: Json): ZIO[Resolve, Nothing, Answer] =
      asked(json).foldZIO(
        error => ZIO.succeed(Answer.Reply(failure(id, error))),
        uris =>
          val cells = uris.flatMap(cellOf)
          if cells.length != uris.length then ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownTarget)))
          else
            val origin = field(json, "params").flatMap(params => string(params, "originId"))
            GraphCompile.runCells(repo, graph, cells.map(cell => (cell.module.name, cell.version)), jdk).either.map:
              case Left(_)  => Answer.Reply(result(id, compileResult(2, origin)))
              case Right(_) => Answer.Reply(result(id, compileResult(1, origin)))
      )

    private def reply(id: Json, body: ZIO[Resolve, BspError, Json]): ZIO[Resolve, Nothing, Answer] =
      body.fold(error => Answer.Reply(failure(id, error)), value => Answer.Reply(result(id, value)))

    private def scalac(uri: String): ZIO[Resolve, BspError, Json] =
      cellOf(uri) match
        case None => ZIO.fail(BspError.UnknownTarget)
        case Some(cell) =>
          classpath(cell.module, cell.version).map: paths =>
            Json.Obj(
              "target" -> ident(uri),
              "options" -> Json.Arr(),
              "classpath" -> Json.Arr(paths.map(path => Json.Str(fileUri(path)))*),
              "classDirectory" -> Json.Str(fileUri(GraphCompile.classesDir(repo, cell.module.name, cell.version))),
            )

    private def classpath(module: Module, version: ScalaVersion): ZIO[Resolve, BspError, List[Path]] =
      val cell = ResolveCell(
        module.name,
        module.dependencies,
        version,
        Platform.Jvm,
        BuildResolvers(graph.resolvers),
        ClasspathScope.Compile,
      )
      val upstream = module.dependsOn.map(dependency => GraphCompile.classesDir(repo, dependency, version))
      for
        lock    <- readLock
        jars    <- Resolve.materialize(cell, lock).mapBoth(BspError.Resolve(_), _.artifacts.map(_.path))
        library <- JvmCompile.runtime(version).mapError(BspError.Toolchain(_))
      yield (library ++ jars ++ upstream).distinct

    private def readLock: IO[BspError, DependencyLock] =
      val path = repo.resolve(".histos").resolve("dependencies.lock")
      ZIO.attemptBlocking {
        if Files.isRegularFile(path) then Right(Files.readString(path))
        else Left(BspError.Lock(ResolveError.NoLock(path)))
      }.mapError(_ => BspError.LockRead(path)).flatMap(ZIO.fromEither).flatMap: text =>
        ZIO.fromEither(DependencyLock.parse(text).left.map(error => BspError.Lock(ResolveError.Lock(error))))

    private def sourceItem(uri: String): IO[BspError, Json] =
      cellOf(uri) match
        case None => ZIO.fail(BspError.UnknownTarget)
        case Some(cell) =>
          val modules = graph.projects.collect { case item: Module => item }
          GraphCompile.sourceBase(repo, cell.module, modules).mapError(BspError.Sources(_)).map:
            case Some(root) =>
              List(Json.Obj(
                "uri" -> Json.Str(directoryUri(root)),
                "kind" -> Json.Num(2),
                "generated" -> Json.Bool(false),
              ))
            case None => Nil
          .map(found => Json.Obj("target" -> ident(uri), "sources" -> Json.Arr(found*)))

    private def targets: ZIO[Any, BspError, Json] =
      val cells = jvmCells
      val versions = cells.map(_.version).distinct
      ZIO.foreach(versions)(version => JvmCompile.scalaJars(version).map(version.value -> _)).mapBoth(
        BspError.Toolchain(_),
        jars => jars.toMap,
      ).map: byVersion =>
        val items = cells.map(cell => target(cell, byVersion.getOrElse(cell.version.value, Nil)))
        Json.Obj("targets" -> Json.Arr(items*))

    private def target(cell: Cell, jars: List[Path]): Json =
      val uri = targetUri(cell.module.name, cell.version)
      Json.Obj(
        "id" -> ident(uri),
        "displayName" -> Json.Str(cell.module.name.value),
        "baseDirectory" -> Json.Str(directoryUri(repo.resolve(cell.module.name.value))),
        "tags" -> Json.Arr(Json.Str("library")),
        "languageIds" -> Json.Arr(Json.Str("scala")),
        "dependencies" -> Json.Arr(dependencyUris(cell.module, cell.version).map(ident)*),
        "capabilities" -> Json.Obj(
          "canCompile" -> Json.Bool(true),
          "canTest" -> Json.Bool(false),
          "canRun" -> Json.Bool(false),
        ),
        "dataKind" -> Json.Str("scala"),
        "data" -> Json.Obj(
          "scalaOrganization" -> Json.Str("org.scala-lang"),
          "scalaVersion" -> Json.Str(cell.version.value),
          "scalaBinaryVersion" -> Json.Str(binary(cell.version)),
          "platform" -> Json.Num(1),
          "jars" -> Json.Arr(jars.map(path => Json.Str(fileUri(path)))*),
        ),
      )

    private def jvmCells: List[Cell] =
      graph.projects.collect { case module: Module if module.platforms.all.contains(Platform.Jvm) => module }
        .flatMap(module => module.versions.all.map(version => Cell(module, version)))
        .sortBy(cell => (cell.module.name.value, cell.version.value))

    private def dependencyUris(module: Module, version: ScalaVersion): List[String] =
      module.dependsOn.flatMap: name =>
        graph.get(name) match
          case Some(dependency: Module)
              if dependency.platforms.all.contains(Platform.Jvm) && dependency.versions.all.exists(_.value == version.value) =>
            Some(targetUri(dependency.name, version))
          case _ => None

    private def cellOf(uri: String): Option[Cell] =
      jvmCells.find(cell => targetUri(cell.module.name, cell.version) == uri)

    private def targetUri(name: ProjectName, version: ScalaVersion): String =
      s"${directoryUri(repo)}${name.value}?scala=${version.value}"

    private def initialize: Json =
      Json.Obj(
        "displayName" -> Json.Str(ServerName),
        "version" -> Json.Str(ServerVersion),
        "bspVersion" -> Json.Str(BspVersion),
        "capabilities" -> Json.Obj(
          "compileProvider" -> Json.Obj("languageIds" -> Json.Arr(Json.Str("scala"))),
        ),
      )

  private final case class Cell(module: Module, version: ScalaVersion)

  private def compileResult(code: Int, origin: Option[String]): Json =
    val base = List("statusCode" -> Json.Num(code))
    val fields = origin match
      case Some(id) => ("originId" -> Json.Str(id)) :: base
      case None     => base
    Json.Obj(fields*)

  private def asked(json: Json): IO[BspError, List[String]] =
    field(json, "params") match
      case None => ZIO.fail(BspError.Params(ParamFault.Missing))
      case Some(params) =>
        field(params, "targets") match
          case Some(Json.Arr(values)) =>
            val uris = values.toList.flatMap(item => string(item, "uri"))
            if uris.length == values.length then ZIO.succeed(uris)
            else ZIO.fail(BspError.Params(ParamFault.NoUri))
          case _ => ZIO.fail(BspError.Params(ParamFault.NoTargets))

  private def result(id: Json, value: Json): String =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> value).toJson

  private def failure(id: Json, error: BspError): String =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id" -> id,
      "error" -> Json.Obj("code" -> Json.Num(error.code), "message" -> Json.Str(error.message)),
    ).toJson

  private def ident(uri: String): Json =
    Json.Obj("uri" -> Json.Str(uri))

  private def method(json: Json): Option[String] =
    string(json, "method")

  private def string(json: Json, name: String): Option[String] =
    field(json, name).collect { case Json.Str(value) => value }

  private def field(json: Json, name: String): Option[Json] =
    json match
      case Json.Obj(fields) => fields.collectFirst { case (`name`, value) => value }
      case _                => None

  private def fileUri(path: Path): String =
    path.toAbsolutePath.normalize.toUri.toString

  private def directoryUri(path: Path): String =
    val text = fileUri(path)
    if text.endsWith("/") then text else s"$text/"

  private def binary(version: ScalaVersion): String =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3)        => "3"
      case Some(ScalaSeries.Scala2(minor)) => s"2.$minor"
      case None                            => version.value
end Bsp
