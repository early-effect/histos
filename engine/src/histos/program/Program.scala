package histos.program

import histos.js.{JsLink, JsRun}
import histos.native.{NativeLink, NativeToolchain}
import histos.pack.{MainClass as Binary, MainClassError}
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.testrun.{TestClasspath, TestEvent, TestProcess, TestRequest}
import histos.zinc.{GraphCompile, GraphCompileError, JvmCompile, ResolvedJdk}
import histos.{Aggregate, ClangPin, Dep, MainClass, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, Ref, ZIO}
import zio.stream.ZStream

/**
 * Run the class a module names, on one platform. The main classpath only.
 * No test classes and no classpath scan. A module that builds more than one platform
 * does not compile and does not start until `choice` names one of them.
 */
object Program:
  def console(
      repo: Path,
      graph: ProjectGraph,
      name: String,
      jdk: ResolvedJdk,
      env: Map[String, String],
      choice: Option[Platform] = None,
  ): ZStream[Resolve, ProgramError, ProgramFeed] =
    ZStream.unwrap:
      for
        module <- ZIO.fromEither(locate(graph, name))
        main <- ZIO.fromEither(module.application.toRight(ProgramError.NoMain(module.name)))
        binary <- ZIO.fromEither(Binary.from(main.value).left.map(ProgramError.Main(_)))
        platform <- ZIO.fromEither(pick(module, choice))
        lock <- readLock(repo)
        compiled <- GraphCompile.run(repo, graph, List(module.name.value), jdk, Some(platform)).mapError(ProgramError.Compile(_))
        cells = compiled.filter(cell => cell.module == module.name && cell.platform == platform).sortBy(_.version.value)
        continue <- Ref.make(true)
      yield ZStream.fromIterable(cells).flatMap: cell =>
        ZStream.unwrap:
          continue.get.map: keep =>
            if keep then
              one(repo, graph, module, binary, cell, lock, jdk, env).tap:
                case ProgramFeed.Exited(_, _, _, code) if code != 0 => continue.set(false)
                case _                                              => ZIO.unit
            else ZStream.empty

  private def locate(graph: ProjectGraph, name: String): Either[ProgramError, Module] =
    ProjectName.from(name) match
      case Left(_) => Left(ProgramError.Absent(name))
      case Right(found) =>
        graph.get(found) match
          case Some(module: Module) => Right(module)
          case Some(_: Aggregate)   => Left(ProgramError.Aggregate(found))
          case None                 => Left(ProgramError.Absent(name))

  /** One platform, or a refusal. Nothing is compiled and nothing is started on the refusal. */
  private def pick(module: Module, choice: Option[Platform]): Either[ProgramError, Platform] =
    val built = module.platforms.all
    choice match
      case Some(platform) if built.contains(platform) => Right(platform)
      case Some(platform)                             => Left(ProgramError.Unbuilt(module.name, platform, built))
      case None =>
        built match
          case only :: Nil => Right(only)
          case many        => Left(ProgramError.Choose(module.name, many))

  private def one(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      binary: Binary,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      jdk: ResolvedJdk,
      env: Map[String, String],
  ): ZStream[Resolve, ProgramError, ProgramFeed] =
    val modules = graph.projects.collect { case item: Module => item }
    val repositories = BuildResolvers(graph.resolvers)
    val began = ZStream.succeed(ProgramFeed.Begin(cell.module, cell.version, cell.platform))
    val ran =
      cell.platform match
        case Platform.Jvm =>
          ZStream.unwrap:
            for
              paths <- classpath(repo, modules, module, cell, lock, repositories)
              work <- fresh(workDir(repo, cell))
            yield events(cell, TestRequest(
              Platform.Jvm,
              jdk.home.resolve("bin").resolve("java"),
              TestClasspath(cell.classes, paths.drop(1)),
              binary,
              work,
              env,
            ))
        case Platform.Js =>
          ZStream.unwrap:
            val linkDir = workDir(repo, cell)
            for
              paths <- classpath(repo, modules, module, cell, lock, repositories)
              _ <- fresh(linkDir)
              script <- JsLink.link(cell.version, binary, paths, linkDir, jdk, module.jsModule)
                .mapError(error => ProgramError.Link(cell.module, cell.version, cell.platform, error.message))
              asked <- JsRun.start(module.jsEnv, module.jsModule, script, linkDir, jdk, env)
                .mapError(error => ProgramError.Link(cell.module, cell.version, cell.platform, error.message))
            yield events(cell, asked)
        case Platform.Native =>
          ZStream.unwrap:
            val linkDir = workDir(repo, cell)
            for
              major <- nativeMajor(module, cell.version)
              paths <- classpath(repo, modules, module, cell, lock, repositories)
              _ <- fresh(linkDir)
              program <- NativeLink.linkClasspath(
                           major,
                           module.nativeVersion.getOrElse(NativeToolchain.ReleaseRevision),
                           env,
                           paths,
                           linkDir,
                           jdk,
                           binary,
                         ).mapError(error => ProgramError.Link(cell.module, cell.version, cell.platform, error.message))
              work <- fresh(linkDir.resolve("exec"))
            yield command(cell, List(program.toString), work, env)
    began ++ ran

  private def events(cell: histos.zinc.CompiledCell, request: TestRequest): ZStream[Any, ProgramError, ProgramFeed] =
    TestProcess.events(request).mapError(error => ProgramError.Process(cell.module, cell.version, error.message)).map:
      case TestEvent.Out(text)    => ProgramFeed.Out(text)
      case TestEvent.Err(text)    => ProgramFeed.Err(text)
      case TestEvent.Exited(code) => ProgramFeed.Exited(cell.module, cell.version, cell.platform, code.value)

  private def command(cell: histos.zinc.CompiledCell, argv: List[String], work: Path, env: Map[String, String]): ZStream[Any, ProgramError, ProgramFeed] =
    TestProcess.command(argv, work, env).mapError(error => ProgramError.Process(cell.module, cell.version, error.message)).map:
      case TestEvent.Out(text)    => ProgramFeed.Out(text)
      case TestEvent.Err(text)    => ProgramFeed.Err(text)
      case TestEvent.Exited(code) => ProgramFeed.Exited(cell.module, cell.version, cell.platform, code.value)

  /** Main classes, upstream main classes, the JVM library on JVM, then compile jars. */
  private def classpath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: histos.zinc.CompiledCell,
      lock: DependencyLock,
      repositories: Repositories,
  ): ZIO[Resolve, ProgramError, List[Path]] =
    val resolveCell = ResolveCell(module.name, Dep.select(module.dependencies, cell.platform), cell.version, cell.platform, repositories, ClasspathScope.Compile)
    for
      fetched <- Resolve.materialize(resolveCell, lock).mapError(ProgramError.Lock(_))
      upstream <- ZIO.foreach(Module.chain(module, modules, cell.platform)): dependency =>
        val path = GraphCompile.classesDir(repo, dependency.name, cell.version, cell.platform)
        ZIO.attemptBlocking(Files.isDirectory(path)).mapError(_ => ProgramError.Root(path)).flatMap: found =>
          if found then ZIO.succeed(path)
          else ZIO.fail(ProgramError.MissingUpstream(module.name, dependency.name, cell.version))
      library <- cell.platform match
        case Platform.Jvm => JvmCompile.runtime(cell.version).mapError(error => ProgramError.Library(error.message))
        case Platform.Js | Platform.Native => ZIO.succeed(Nil)
      jars = fetched.artifacts.map(_.path).distinct.filterNot(library.toSet.contains)
    yield cell.classes :: (upstream ++ library ++ jars)

  private def nativeMajor(module: Module, version: ScalaVersion): IO[ProgramError, histos.ClangMajor] =
    module.clang match
      case ClangPin.Major(major) => ZIO.succeed(major)
      case ClangPin.Unused =>
        ZIO.fail(ProgramError.NoClang(module.name, version))

  private def readLock(repo: Path): IO[ProgramError, DependencyLock] =
    val path = repo.resolve(".histos").resolve("dependencies.lock")
    ZIO.attemptBlocking {
      if Files.isRegularFile(path) then Right(Files.readString(path))
      else Left(ProgramError.Lock(ResolveError.NoLock(path)))
    }.mapError(_ => ProgramError.LockRead(path)).flatMap(ZIO.fromEither).flatMap: text =>
      ZIO.fromEither(DependencyLock.parse(text).left.map(error => ProgramError.Lock(ResolveError.Lock(error))))

  private def fresh(work: Path): IO[ProgramError, Path] =
    ZIO.attemptBlocking {
      if Files.exists(work) then
        val walk = Files.walk(work)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      Files.createDirectories(work)
      work
    }.mapError(_ => ProgramError.Work(work))

  private def workDir(repo: Path, cell: histos.zinc.CompiledCell): Path =
    repo.resolve(".histos").resolve("run").resolve(cell.module.value).resolve(cell.version.value).resolve(token(cell.platform))

  private def token(platform: Platform): String = platform.token

enum ProgramFeed:
  case Begin(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Out(text: String)
  case Err(text: String)
  case Exited(module: ProjectName, version: ScalaVersion, platform: Platform, code: Int)

enum ProgramError:
  case Absent(name: String)
  case Aggregate(name: ProjectName)
  case NoMain(name: ProjectName)
  case Choose(name: ProjectName, platforms: List[Platform])
  case Unbuilt(name: ProjectName, platform: Platform, built: List[Platform])
  case NoClang(name: ProjectName, version: ScalaVersion)
  case Main(error: MainClassError)
  case Compile(error: GraphCompileError)
  case Library(detail: String)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case Link(module: ProjectName, version: ScalaVersion, platform: Platform, detail: String)
  case Process(module: ProjectName, version: ScalaVersion, detail: String)
  case MissingUpstream(module: ProjectName, dependency: ProjectName, version: ScalaVersion)
  case Work(path: Path)
  case Root(path: Path)

  def message: String = this match
    case Absent(name)        => s"'$name' is not a module"
    case Aggregate(name)     => s"'${name.value}' is an aggregate"
    case NoMain(name)        => s"'${name.value}' does not name a main"
    case Choose(name, platforms) =>
      s"'${name.value}' builds ${joined(platforms)}. Name one: histos run ${name.value} ${sample(platforms)}"
    case Unbuilt(name, platform, built) =>
      s"'${name.value}' does not build ${platform.token}. It builds ${joined(built)}"
    case NoClang(name, version) =>
      s"'${name.value}' ${version.value} native does not name a clang major"
    case Main(error)         => error.message
    case Compile(error)      => error.message
    case Library(detail)     => detail
    case Lock(error)         => error.message
    case LockRead(path)      => s"could not read $path"
    case Link(module, version, platform, detail) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to link\n$detail"
    case Process(module, version, detail) =>
      s"'${module.value}' ${version.value} did not start\n$detail"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Work(path) => s"could not prepare the run directory $path"
    case Root(path) => s"could not read $path"

private def joined(platforms: List[Platform]): String =
  val names = platforms.map(_.token)
  names match
    case Nil                => ""
    case one :: Nil         => one
    case one :: two :: Nil  => s"$one or $two"
    case many               =>
      val leading = many.dropRight(1).mkString(", ")
      val ending = many.takeRight(1).mkString
      s"$leading, or $ending"

private def sample(platforms: List[Platform]): String =
  platforms match
    case head :: _ => head.token
    case Nil       => "jvm"
