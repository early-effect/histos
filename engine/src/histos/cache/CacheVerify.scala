package histos.cache

import histos.jdk.{JdkRuntime, Sha256}
import histos.schedule.{CacheError, RemoteCache}
import histos.testrun.TestCompile
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileError, CompileReplay, GraphCompile, GraphCompileError, ProductDigest}
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO, ZLayer}

/** Recompile stored compile hits in a scratch checkout with an empty store and no remote, and name a module whose class
  * files differ. A module with no marker is not a differ. Coverage markers are not compared to a normal compile.
  */
private[histos] object CacheVerify:
  def differ(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      jdk: JdkRuntime,
      native: histos.native.NativeOrigin,
      store: ContentStore,
  ): ZIO[histos.resolve.Resolve, CacheError | IoFailure, List[ProjectName]] =
    val modules = graph.projects.collect { case module: Module => module.name }
    val asked   = if projects.isEmpty then modules else projects.filter(modules.contains)
    if asked.isEmpty then ZIO.succeed(Nil)
    else
      ReplayMarks
        .collect(repo, Some(asked.toSet), coverage = false)
        .flatMap: marks =>
          if marks.isEmpty then ZIO.succeed(Nil)
          else
            ZIO.scoped:
              for
                held    <- ZIO.foreach(marks)(hold(store, _))
                cas     <- Workspace.work("histos-verify-cas")
                scratch <- Workspace.work("histos-verify-scratch")
                named   <- compare(repo, graph, jdk, native, cas, scratch, held)
              yield named.distinct

  private enum Held:
    case Differ(module: ProjectName)
    case Intact(mark: ReplayMarks.Mark, classes: List[(String, Array[Byte])])

  private def hold(store: ContentStore, mark: ReplayMarks.Mark): IO[CacheError, Held] =
    store
      .read(mark.key)
      .map:
        case None        => Held.Differ(mark.module)
        case Some(bytes) =>
          CompileReplay.classBytes(mark.key, bytes) match
            case Left(_)        => Held.Differ(mark.module)
            case Right(classes) => Held.Intact(mark, classes)

  private def compare(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      native: histos.native.NativeOrigin,
      cas: Path,
      scratch: Path,
      held: List[Held],
  ): ZIO[histos.resolve.Resolve & zio.Scope, CacheError | IoFailure, List[ProjectName]] =
    val corrupt = held.collect { case Held.Differ(module) => module }
    val intact  = held.collect { case row: Held.Intact => row }
    if intact.isEmpty then ZIO.succeed(corrupt)
    else
      val store = ContentStore.in(cas)
      val names = intact.map(_.mark.module).distinct
      for
        _        <- copyTree(repo, scratch)
        compiled <- GraphCompile
          .strict(GraphCompile.console(scratch, graph, names, jdk, native, None))
          .runCollect
          .provideSomeLayer[histos.resolve.Resolve & zio.Scope](
            ZLayer.succeed(store) ++ RemoteCache.off
          )
          .either
        named <- compiled match
          case Left(error) => ZIO.succeed(corrupt ++ failed(error, names))
          case Right(_)    =>
            ZIO.foreach(intact)(row => differs(repo, scratch, row)).map(found => corrupt ++ found.flatten)
      yield named

  private def failed(error: GraphCompileError, names: List[ProjectName]): List[ProjectName] =
    val named = modulesOf(error)
    if named.isEmpty then names else named

  private def modulesOf(error: GraphCompileError): List[ProjectName] =
    error match
      case GraphCompileError.Failed(module, _, _, _)                  => List(module)
      case GraphCompileError.UnknownCell(module, _)                   => List(module)
      case GraphCompileError.NoRoot(module)                           => List(module)
      case GraphCompileError.NoSources(module, _)                     => List(module)
      case GraphCompileError.MissingUpstream(module, _, _, _)         => List(module)
      case GraphCompileError.IncompatibleUpstream(module, _, _, _, _) => List(module)
      case GraphCompileError.Cycle(module)                            => List(module)
      case GraphCompileError.Assets(module, _, _, _)                  => List(module)
      case GraphCompileError.Cells(errors)                            => errors.toList.flatMap(modulesOf)
      case GraphCompileError.Runtime(_) | GraphCompileError.Target(_) | GraphCompileError.Lock(_) |
          GraphCompileError.LockRead(_) | GraphCompileError.RootRead(_) | GraphCompileError.Output(_) =>
        Nil

  private def differs(
      repo: Path,
      scratch: Path,
      row: Held.Intact,
  ): IO[IoFailure, Option[ProjectName]] =
    ZIO.scoped:
      Workspace
        .work("histos-verify-classes")
        .flatMap: dir =>
          writeClasses(dir, row.classes) *>
            digest(dir, repo).either.flatMap:
              case Left(_)       => ZIO.succeed(Some(row.mark.module))
              case Right(stored) =>
                digest(classesOf(row.mark, scratch), scratch).either.map:
                  case Left(_)      => Some(row.mark.module)
                  case Right(fresh) =>
                    if fresh == stored then None else Some(row.mark.module)

  private def digest(root: Path, checkout: Path): IO[CacheError, Sha256] =
    ZIO
      .fromEither(ProductDigest.ofDirectory(root, checkout))
      .mapError:
        case CompileError.Io(failure) => CacheError.Io(failure)
        case _                        => CacheError.Io(IoFailure(root, IoFault.Read))

  private def classesOf(mark: ReplayMarks.Mark, repo: Path): Path =
    mark.kind match
      case ReplayMarks.Kind.Main => GraphCompile.classesDir(repo, mark.module, mark.version, mark.platform)
      case ReplayMarks.Kind.Test => TestCompile.testClasses(repo, mark.module, mark.version, mark.platform)

  private def writeClasses(dir: Path, classes: List[(String, Array[Byte])]): IO[IoFailure, Unit] =
    ZIO.foreachDiscard(classes): (relative, bytes) =>
      val path = dir.resolve(relative)
      ZIO
        .attemptBlocking {
          Option(path.getParent).foreach(parent => Files.createDirectories(parent))
          Files.write(path, bytes)
          ()
        }
        .mapError(_ => IoFailure(path, IoFault.Write))

  private val Skip = Set(".histos", ".git", "target", "dist")

  private def copyTree(from: Path, to: Path): IO[IoFailure, Unit] =
    ZIO.attemptBlocking(copyChildren(from, to, root = true)).mapError(_ => IoFailure(from, IoFault.Walk))

  private def copyChildren(from: Path, to: Path, root: Boolean): Unit =
    val stream = Files.newDirectoryStream(from)
    try
      stream.iterator.asScala.foreach: child =>
        val name = Option(child.getFileName).map(_.toString).getOrElse("")
        val skip = root && Skip.contains(name)
        if !skip && !Files.isSymbolicLink(child) then
          val dest = to.resolve(name)
          if Files.isDirectory(child) then
            Files.createDirectories(dest)
            copyChildren(child, dest, root = false)
          else if Files.isRegularFile(child) then
            Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
            Files.copy(child, dest)
    finally stream.close()
end CacheVerify
