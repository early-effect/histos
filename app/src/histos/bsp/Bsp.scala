package histos.bsp

import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Resolve, ResolveCell, ResolveError}
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, JvmCompile, ResolvedJdk, ScalaSeries}
import histos.{Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.json.*
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
    case Missing, Rejected

    def message: String = this match
      case Missing  => "bsp params are required"
      case Rejected => "bsp params were rejected"

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
      payload.fromJson[zio.json.ast.Json] match
        case Left(_) => ZIO.succeed(Answer.Reply(failure(Rpc.RpcId.Absent, BspError.Parse)))
        case Right(json) =>
          json.as[Rpc.Incoming] match
            case Left(_) => ZIO.succeed(Answer.Reply(failure(Rpc.RpcId.Absent, BspError.NotRequest)))
            case Right(incoming) =>
              val id = Rpc.idOf(incoming)
              incoming.method match
                case None => ZIO.succeed(Answer.Reply(failure(id, BspError.NoMethod)))
                case Some(name) =>
                  phase.get.flatMap(current => dispatch(current, id, name, incoming, incoming.id.isEmpty))

    private def dispatch(
        current: Phase,
        id: Rpc.RpcId,
        name: String,
        incoming: Rpc.Incoming,
        notification: Boolean,
    ): ZIO[Resolve, Nothing, Answer] =
      (current, name) match
        case (_, "build/exit") =>
          ZIO.succeed(Answer.Stop(if current == Phase.Shutdown then 0 else 1))
        case (Phase.New, "build/initialize") if !notification =>
          phase.set(Phase.Ready).as(Answer.Reply(Rpc.ok(id, initialize)))
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
          phase.set(Phase.Shutdown).as(Answer.Reply(Rpc.ok(id, Rpc.NullResult())))
        case (Phase.Ready, "workspace/buildTargets") if !notification =>
          reply(id, targets)
        case (Phase.Ready, "buildTarget/sources") if !notification =>
          sources(id, incoming)
        case (Phase.Ready, "buildTarget/scalacOptions") if !notification =>
          options(id, incoming)
        case (Phase.Ready, "buildTarget/compile") if !notification =>
          compile(id, incoming)
        case (Phase.Ready, _) if notification =>
          ZIO.succeed(Answer.Quiet)
        case (Phase.Ready, other) =>
          ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownMethod(other))))

    private def sources(id: Rpc.RpcId, incoming: Rpc.Incoming): ZIO[Resolve, Nothing, Answer] =
      reply(id, uris(incoming).flatMap(found => ZIO.foreach(found)(sourceItem).map(Rpc.SourcesResult(_))))

    private def options(id: Rpc.RpcId, incoming: Rpc.Incoming): ZIO[Resolve, Nothing, Answer] =
      reply(id, uris(incoming).flatMap(found => ZIO.foreach(found)(scalac).map(Rpc.ScalacOptionsResult(_))))

    private def compile(id: Rpc.RpcId, incoming: Rpc.Incoming): ZIO[Resolve, Nothing, Answer] =
      ZIO.fromEither(Rpc.compileParams(incoming.params).left.map(fault)).foldZIO(
        error => ZIO.succeed(Answer.Reply(failure(id, error))),
        params =>
          val found = params.targets.map(_.uri)
          val cells = found.flatMap(cellOf)
          if cells.length != found.length then ZIO.succeed(Answer.Reply(failure(id, BspError.UnknownTarget)))
          else
            GraphCompile.runCells(repo, graph, cells.map(cell => (cell.module.name, cell.version)), jdk).either.map:
              case Left(_)  => Answer.Reply(Rpc.ok(id, Rpc.CompileResult(2, params.originId)))
              case Right(_) => Answer.Reply(Rpc.ok(id, Rpc.CompileResult(1, params.originId)))
      )

    private def uris(incoming: Rpc.Incoming): IO[BspError, List[String]] =
      ZIO.fromEither(Rpc.targets(incoming.params).left.map(fault))

    private def fault(decoded: Rpc.ParamsDecode): BspError =
      decoded match
        case Rpc.ParamsDecode.Missing  => BspError.Params(ParamFault.Missing)
        case Rpc.ParamsDecode.Rejected => BspError.Params(ParamFault.Rejected)

    private def reply[A: JsonCodec](id: Rpc.RpcId, body: ZIO[Resolve, BspError, A]): ZIO[Resolve, Nothing, Answer] =
      body.fold(error => Answer.Reply(failure(id, error)), value => Answer.Reply(Rpc.ok(id, value)))

    private def scalac(uri: String): ZIO[Resolve, BspError, Rpc.ScalacOptionsItem] =
      cellOf(uri) match
        case None => ZIO.fail(BspError.UnknownTarget)
        case Some(cell) =>
          classpath(cell.module, cell.version).map: paths =>
            Rpc.ScalacOptionsItem(
              Rpc.TargetId(uri),
              Nil,
              paths.map(fileUri),
              fileUri(GraphCompile.classesDir(repo, cell.module.name, cell.version)),
            )

    private def classpath(module: Module, version: ScalaVersion): ZIO[Resolve, BspError, List[Path]] =
      val cell = ResolveCell(
        module.name,
        Dep.select(module.dependencies, Platform.Jvm),
        version,
        Platform.Jvm,
        BuildResolvers(graph.resolvers),
        ClasspathScope.Compile,
      )
      val upstream = module.upstream(Platform.Jvm).map(dependency => GraphCompile.classesDir(repo, dependency, version))
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

    private def sourceItem(uri: String): IO[BspError, Rpc.SourcesItem] =
      cellOf(uri) match
        case None => ZIO.fail(BspError.UnknownTarget)
        case Some(cell) =>
          val modules = graph.projects.collect { case item: Module => item }
          GraphCompile.sourceBase(repo, cell.module, modules).mapError(BspError.Sources(_)).map: root =>
            val sources = root.toList.map(path => Rpc.SourceItem(directoryUri(path), 2, false))
            Rpc.SourcesItem(Rpc.TargetId(uri), sources)

    private def targets: ZIO[Any, BspError, Rpc.BuildTargetsResult] =
      val cells = jvmCells
      val versions = cells.map(_.version).distinct
      ZIO.foreach(versions)(version => JvmCompile.scalaJars(version).map(version.value -> _)).mapBoth(
        BspError.Toolchain(_),
        jars => jars.toMap,
      ).map: byVersion =>
        Rpc.BuildTargetsResult(cells.map(cell => target(cell, byVersion.getOrElse(cell.version.value, Nil))))

    private def target(cell: Cell, jars: List[Path]): Rpc.BuildTarget =
      val uri = targetUri(cell.module.name, cell.version)
      Rpc.BuildTarget(
        Rpc.TargetId(uri),
        cell.module.name.value,
        directoryUri(repo.resolve(cell.module.baseDirectory)),
        List("library"),
        List("scala"),
        dependencyUris(cell.module, cell.version).map(Rpc.TargetId(_)),
        Rpc.TargetCapabilities(canCompile = true, canTest = false, canRun = false),
        "scala",
        Rpc.ScalaBuildTarget(
          "org.scala-lang",
          cell.version.value,
          binary(cell.version),
          1,
          jars.map(fileUri),
        ),
      )

    private def jvmCells: List[Cell] =
      graph.projects.collect { case module: Module if module.platforms.all.contains(Platform.Jvm) => module }
        .flatMap(module => module.versions.all.map(version => Cell(module, version)))
        .sortBy(cell => (cell.module.name.value, cell.version.value))

    private def dependencyUris(module: Module, version: ScalaVersion): List[String] =
      module.upstream(Platform.Jvm).flatMap: name =>
        graph.get(name) match
          case Some(dependency: Module)
              if dependency.platforms.all.contains(Platform.Jvm) && dependency.versions.all.exists(_.value == version.value) =>
            Some(targetUri(dependency.name, version))
          case _ => None

    private def cellOf(uri: String): Option[Cell] =
      jvmCells.find(cell => targetUri(cell.module.name, cell.version) == uri)

    private def targetUri(name: ProjectName, version: ScalaVersion): String =
      s"${directoryUri(repo)}${name.value}?scala=${version.value}"

    private def initialize: Rpc.InitializeResult =
      Rpc.InitializeResult(
        ServerName,
        ServerVersion,
        BspVersion,
        Rpc.Capabilities(Rpc.CompileProvider(List("scala"))),
      )

    private def failure(id: Rpc.RpcId, error: BspError): String =
      Rpc.fail(id, error.code, error.message)

  private final case class Cell(module: Module, version: ScalaVersion)

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
