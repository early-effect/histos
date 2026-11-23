package histos.watch

import java.nio.file.{FileVisitResult, Files, LinkOption, Path, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest

import scala.util.control.NonFatal

import histos.{Cell, CompilationStage, Module, ProjectGraph}
import histos.plugin.SourceSet
import histos.build.{BuildDag, BuildDagError, BuildNode, ExecutionPlan}
import histos.plugin.{DeclaredInput, OutputKind, PluginTaskId}
import histos.zinc.{StageOrigin, StageRoot, StageRootError, StageRoots}
import histos.jdk.Sha256

import zio.{durationInt, IO, ZIO}

/** Source files a command recompiles. Output under `.histos` is not a source. A file is its bytes, so an edit wakes the
  * watch whatever its modified time and size say.
  */
object SourceWatch:
  final case class Entry(path: String, digest: Sha256)
  final case class Snapshot(entries: List[Entry])

  val Poll: zio.Duration   = 200.millis
  val Settle: zio.Duration = 300.millis

  final case class Root(path: Path, extent: OutputKind):
    def contains(file: Path): Boolean =
      val base      = path.toAbsolutePath.normalize
      val candidate = file.toAbsolutePath.normalize
      extent match
        case OutputKind.File      => candidate == base
        case OutputKind.Directory => candidate.startsWith(base)

  final case class Projection(inputs: List[Root], excluded: List[Root])

  /** The same closure drives compilation, source ownership and producer input invalidation. */
  def project(
      repo: Path,
      graph: ProjectGraph,
      nodes: List[BuildNode],
  ): Either[BuildDagError | StageRootError, Projection] =
    val inventory = StageRoots.of(graph)
    val tasks     = graph.plugins
      .flatMap(plugin => plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task))
      .toMap
    for
      plan  <- watchedPlan(graph, nodes)
      roots <- plan.nodes.foldLeft[Either[StageRootError, List[Root]]](Right(Nil)) { (acc, node) =>
        acc.flatMap { seen =>
          node match
            case BuildNode.Compile(stage) =>
              inventory.inventory(stage).map { declared =>
                seen ++ declared.flatMap { root =>
                  root.origin match
                    case StageOrigin.Generated(_, _) => Nil
                    case StageOrigin.Unmanaged       =>
                      root match
                        case StageRoot.ResourceFile(_, _, _) => List(Root(repo.resolve(root.path.value), root.extent))
                        case StageRoot.Sources(_, _, _) | StageRoot.Resources(_, _, _) |
                            StageRoot.SourceFile(_, _, _) =>
                          Nil
                    case StageOrigin.Conventional(fallback) =>
                      Root(repo.resolve(root.path.value), root.extent) ::
                        fallback.toList.map(path => Root(repo.resolve(path.value), root.extent))
                    case StageOrigin.Declared => List(Root(repo.resolve(root.path.value), root.extent))
                }
              }
            case BuildNode.Generate(id) =>
              Right(
                seen ++ tasks
                  .get(id)
                  .toList
                  .flatMap(_.inputs.flatMap {
                    case DeclaredInput.File(path)      => List(Root(repo.resolve(path.value), OutputKind.File))
                    case DeclaredInput.Directory(path) => List(Root(repo.resolve(path.value), OutputKind.Directory))
                    case DeclaredInput.Module(_, _, _) | DeclaredInput.Note(_, _) | DeclaredInput.Text(_, _) |
                        DeclaredInput.Env(_) =>
                      Nil
                  })
              )
            case BuildNode.Link(_, _) => Right(seen)
        }
      }
      unmanaged <- plan.nodes
        .collect { case BuildNode.Compile(stage) => stage }
        .foldLeft[Either[StageRootError, List[Root]]](Right(Nil)) { (acc, stage) =>
          for
            seen     <- acc
            declared <- inventory.inventory(stage)
          yield seen ++ declared.collect { case root @ StageRoot.Sources(_, _, StageOrigin.Unmanaged) =>
            Root(repo.resolve(root.path.value), root.extent)
          }
        }
    yield
      val definitions = List("histos.scala", "histos.plugins.scala", "histos.strategies.scala")
        .map(name => Root(repo.resolve(name), OutputKind.File))
      val outputs =
        tasks.values.toList.flatMap(_.outputs.map(output => Root(repo.resolve(output.path.value), output.kind)))
      Projection(
        (definitions ++ roots).distinct,
        (Root(repo.resolve(".histos"), OutputKind.Directory) :: outputs ::: unmanaged).distinct,
      )

  /** Module material inputs hash Main products but do not schedule compilation themselves. */
  private def watchedPlan(graph: ProjectGraph, nodes: List[BuildNode]): Either[BuildDagError, ExecutionPlan] =
    val modules = graph.projects.collect { case module: Module => module }
    val tasks   = graph.plugins
      .flatMap(plugin => plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task))
      .toMap
    for
      plan   <- BuildDag.plan(graph, nodes)
      inputs <- plan.nodes
        .collect { case BuildNode.Generate(id) => id }
        .foldLeft[Either[BuildDagError, List[BuildNode]]](Right(Nil)) { (acc, id) =>
          tasks.get(id).toList.flatMap(_.inputs).foldLeft(acc) { (seen, input) =>
            seen.flatMap { previous =>
              input match
                case DeclaredInput.Module(name, version, platform) =>
                  val selected = modules
                    .filter(_.name == name)
                    .flatMap(module =>
                      for
                        scala  <- module.versions.all.filter(value => version.forall(_ == value))
                        target <- module.platforms.all.filter(value => platform.forall(_ == value))
                      yield BuildNode.Compile(CompilationStage(Cell(name, scala, target), SourceSet.Main))
                    )
                  if selected.isEmpty then Left(BuildDagError.MissingCell(id, name, version, platform))
                  else Right(previous ++ selected)
                case DeclaredInput.File(_) | DeclaredInput.Directory(_) | DeclaredInput.Note(_, _) |
                    DeclaredInput.Text(_, _) | DeclaredInput.Env(_) =>
                  Right(previous)
            }
          }
        }
      added = inputs.distinct.filterNot(plan.nodes.contains)
      expanded <- if added.isEmpty then Right(plan) else watchedPlan(graph, nodes ++ added)
    yield expanded

  def take(projection: Projection): IO[WatchError, Snapshot] =
    ZIO
      .foreach(projection.inputs)(root => one(root, projection.excluded))
      .map(found => Snapshot(found.flatten.distinct.sortBy(_.path)))

  def since(projection: Projection, start: Snapshot): IO[WatchError, List[Path]] =
    def settle(seen: Snapshot): IO[WatchError, Snapshot] =
      ZIO.sleep(Settle) *> take(projection).flatMap(now => if now == seen then ZIO.succeed(now) else settle(now))
    def poll: IO[WatchError, Snapshot] =
      ZIO.sleep(Poll) *> take(projection).flatMap(now => if now == start then poll else settle(now))
    poll.map(done => woke(start, done))

  private def one(root: Root, excluded: List[Root]): IO[WatchError, List[Entry]] =
    ZIO
      .attemptBlocking {
        def skipped(path: Path): Boolean = excluded.exists(_.contains(path))
        if skipped(root.path) || !Files.exists(root.path, LinkOption.NOFOLLOW_LINKS) then Right(Nil)
        else
          val paths = root.extent match
            case OutputKind.File      => List(root.path)
            case OutputKind.Directory =>
              val found = List.newBuilder[Path]
              Files.walkFileTree(
                root.path,
                new SimpleFileVisitor[Path]:
                  override def preVisitDirectory(path: Path, attributes: BasicFileAttributes): FileVisitResult =
                    if skipped(path) then FileVisitResult.SKIP_SUBTREE else FileVisitResult.CONTINUE
                  override def visitFile(path: Path, attributes: BasicFileAttributes): FileVisitResult =
                    if !skipped(path) then found += path
                    FileVisitResult.CONTINUE,
              )
              found.result()
          paths.find(Files.isSymbolicLink(_)) match
            case Some(path) => Left(WatchError.Link(path))
            case None       => Right(paths.flatMap(entry))
      }
      .mapError(_ => WatchError.Read(root.path))
      .flatMap(ZIO.fromEither)

  def roots(repo: Path, modules: List[Module]): List[Path] =
    val graph  = ProjectGraph.trusted(modules)
    val stages = modules.flatMap(module =>
      for
        version  <- module.versions.all
        platform <- module.platforms.all
        scope    <- List(SourceSet.Main, SourceSet.Test)
      yield BuildNode.Compile(CompilationStage(Cell(module.name, version, platform), scope))
    )
    project(repo, graph, stages).fold(
      _ =>
        List(
          repo.resolve("histos.scala"),
          repo.resolve("histos.plugins.scala"),
          repo.resolve("histos.strategies.scala"),
        ),
      projection => projection.inputs.map(_.path).distinct,
    )

  def take(roots: List[Path]): IO[WatchError, Snapshot] =
    take(roots, Set.empty)

  /** Legacy path lists have subtree extent. Typed callers retain the declared File/Directory distinction. */
  def take(roots: List[Path], excluded: Set[Path]): IO[WatchError, Snapshot] =
    take(legacy(roots, excluded))

  private def legacy(roots: List[Path], excluded: Set[Path]): Projection =
    Projection(roots.map(Root(_, OutputKind.Directory)), excluded.toList.map(Root(_, OutputKind.Directory)))

  /** Wait until a snapshot differs from one taken now, and until a burst of writes has settled. */
  def changed(roots: List[Path]): IO[WatchError, List[Path]] =
    take(roots).flatMap(since(roots, _))

  /** Wait until a snapshot differs from `start`, and until a burst of writes has settled. The paths are the ones that
    * differ from `start`.
    */
  def since(roots: List[Path], start: Snapshot): IO[WatchError, List[Path]] =
    since(roots, start, Set.empty)

  def since(roots: List[Path], start: Snapshot, excluded: Set[Path]): IO[WatchError, List[Path]] =
    since(legacy(roots, excluded), start)

  private def woke(before: Snapshot, after: Snapshot): List[Path] =
    val previous = before.entries.map(entry => entry.path -> entry).toMap
    val next     = after.entries.map(entry => entry.path -> entry).toMap
    (previous.keySet ++ next.keySet).toList.sorted.flatMap { path =>
      (previous.get(path), next.get(path)) match
        case (Some(left), Some(right)) if left == right => None
        case _                                          => Some(Path.of(path))
    }

  /** A file that vanishes or cannot be read between the walk and the read is absent from the snapshot. */
  private def entry(path: Path): Option[Entry] =
    if !Files.isRegularFile(path) then None
    else
      try
        val bytes = Files.readAllBytes(path)
        Some(
          Entry(
            path.toAbsolutePath.normalize.toString,
            Sha256.fromDigest(MessageDigest.getInstance("SHA-256").digest(bytes)),
          )
        )
      catch case NonFatal(_) => None
end SourceWatch
