package histos.zinc

import histos.pack.Resources
import histos.plugin.{ArtifactPath, OutputKind, OutputName, PluginTaskId, RepoPath, SourceSet}
import histos.workspace.{IoFailure, IoFault}
import histos.{AssetError, BuildError, CompilationStage, GeneratedAsset, Module, ProjectGraph, ResourceName, RootKind}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum StageLayout:
  case Main, Test, TestSibling, Exact

enum StageOrigin:
  case Conventional(fallback: Option[RepoPath])
  case Declared
  case Unmanaged
  case Generated(task: PluginTaskId, output: OutputName)

enum StageRoot:
  case Sources(path: RepoPath, layout: StageLayout, origin: StageOrigin)
  case Resources(path: RepoPath, layout: StageLayout, origin: StageOrigin)
  case SourceFile(path: RepoPath, entry: ArtifactPath, origin: StageOrigin)
  case ResourceFile(path: RepoPath, name: ResourceName, origin: StageOrigin)

  def path: RepoPath
  def origin: StageOrigin

  def kind: RootKind = this match
    case Sources(_, _, _) | SourceFile(_, _, _)     => RootKind.Sources
    case Resources(_, _, _) | ResourceFile(_, _, _) => RootKind.Resources

  def extent: OutputKind = this match
    case SourceFile(_, _, _) | ResourceFile(_, _, _) => OutputKind.File
    case Sources(_, _, _) | Resources(_, _, _)       => OutputKind.Directory
end StageRoot

final case class GeneratedRootId(task: PluginTaskId, output: OutputName)
final case class GeneratedLocation(id: GeneratedRootId, kind: OutputKind, path: Path)

enum GeneratedRoots:
  case Existing
  case Provided(locations: List[GeneratedLocation])

final case class StageFiles(
    sources: List[PackagedSource],
    resources: List[(ResourceName, Path)],
    assets: List[GeneratedAsset.Text],
    origins: Map[Path, StageOrigin] = Map.empty,
):
  def generatedPaths: Set[Path] = origins.collect { case (path, StageOrigin.Generated(_, _) | StageOrigin.Unmanaged) =>
    path.toAbsolutePath.normalize
  }.toSet

enum StageRootError:
  case UnknownStage(stage: CompilationStage)
  case Contract(error: BuildError)
  case InvalidPath(raw: String)
  case Missing(path: Path, kind: OutputKind)
  case Link(path: Path)
  case Escaped(path: Path)
  case MissingGenerated(id: GeneratedRootId)
  case DuplicateGenerated(id: GeneratedRootId)
  case OutputKindMismatch(id: GeneratedRootId, expected: OutputKind, actual: OutputKind)
  case Io(error: IoFailure)
  case Source(error: CompileError)
  case Resource(error: AssetError)
  case SourceCollision(entry: String, paths: List[Path])
  case ResourceCollision(name: ResourceName, paths: List[Path])

/** One physical-input inventory for compilation, packaging, and root projections. */
final class StageRoots private (graph: ProjectGraph):
  private val modules = graph.projects.collect { case module: Module => module }

  def inventory(stage: CompilationStage): Either[StageRootError, List[StageRoot]] =
    modules.find(_.name == stage.cell.module) match
      case None => Left(StageRootError.UnknownStage(stage))
      case Some(module)
          if !module.versions.all.contains(stage.cell.version) ||
            !module.platforms.all.contains(stage.cell.platform) =>
        Left(StageRootError.UnknownStage(stage))
      case Some(module) =>
        val layout = stage.scope match
          case SourceSet.Main => StageLayout.Main
          case SourceSet.Test => StageLayout.Test
        for
          primary  <- path(s"${module.baseDirectory}/src")
          fallback <- optionalPath("src")
          conventional = StageOrigin.Conventional(fallback)
          extras <- sequence(
            module.sources.map(root => path(root.value).map(StageRoot.Sources(_, layout, StageOrigin.Declared)))
          )
          sibling <- stage.scope match
            case SourceSet.Main => Right(Nil)
            case SourceSet.Test =>
              for
                root     <- path(s"${module.baseDirectory}/test")
                fallback <- optionalPath("test")
              yield List(
                StageRoot.Sources(root, StageLayout.TestSibling, StageOrigin.Conventional(fallback)),
                StageRoot.Resources(root, StageLayout.TestSibling, StageOrigin.Conventional(fallback)),
              )
          unmanaged <- path(s"target/unmanaged/${module.name.value}/src")
          emitted   <- Emitted.roots(graph, stage).left.map(StageRootError.Contract(_))
        yield
          val scoped = module.roots.filter(_.applies(stage.scope, stage.cell.platform)).map { root =>
            root.kind match
              case RootKind.Sources   => StageRoot.Sources(root.path, StageLayout.Exact, StageOrigin.Declared)
              case RootKind.Resources => StageRoot.Resources(root.path, StageLayout.Exact, StageOrigin.Declared)
          }
          val assetFiles = stage.scope match
            case SourceSet.Main =>
              module.resources.collect { case GeneratedAsset.File(name, path) =>
                StageRoot.ResourceFile(path, name, StageOrigin.Unmanaged)
              }
            case SourceSet.Test => Nil
          (List(StageRoot.Sources(primary, layout, conventional), StageRoot.Resources(primary, layout, conventional)) ++
            extras ++ sibling ++ scoped ++ emitted ++ assetFiles :+ StageRoot.Sources(
              unmanaged,
              layout,
              StageOrigin.Unmanaged,
            )).distinct

  def roots(stage: CompilationStage): Either[StageRootError, List[StageRoot]] = inventory(stage)

  def read(repo: Path, stage: CompilationStage): IO[StageRootError, StageFiles] =
    read(repo, stage, GeneratedRoots.Existing)

  /** Provided locations are validated committed generations, acquired through the producer's read lease. */
  def read(repo: Path, stage: CompilationStage, generated: GeneratedRoots): IO[StageRootError, StageFiles] =
    for
      roots    <- ZIO.fromEither(inventory(stage))
      selected <- ZIO.foreach(roots)(readRoot(repo, stage, generated, _))
      replacements = roots
        .zip(selected)
        .collect { case (root, files) if root.origin == StageOrigin.Unmanaged => files.sources }
        .flatten
      replaced  = replacements.map(_.entry).toSet
      committed = roots
        .zip(selected)
        .collect { case (root, files) if root.origin != StageOrigin.Unmanaged => files.sources }
        .flatten
      sources   = (committed.filterNot(source => replaced.contains(source.entry)) ++ replacements).distinct
      resources = selected.flatMap(_.resources).distinct
      assets    = modules.find(_.name == stage.cell.module).toList.flatMap { module =>
        stage.scope match
          case SourceSet.Main => module.resources.collect { case text: GeneratedAsset.Text => text }
          case SourceSet.Test => Nil
      }
      _ <- ZIO.fromEither(sourceCollisions(sources))
      _ <- ZIO.fromEither(resourceCollisions(resources, assets))
      origins = roots
        .zip(selected)
        .flatMap { (root, files) =>
          (files.sources.map(_.path) ++ files.resources.map(_._2)).map(_ -> root.origin)
        }
        .toMap
    yield StageFiles(sources.sortBy(_.entry), resources.sortBy(_._1.value), assets, origins)

  private def path(raw: String): Either[StageRootError, RepoPath] =
    RepoPath.from(raw).left.map(_ => StageRootError.InvalidPath(raw))

  private def optionalPath(raw: String): Either[StageRootError, Option[RepoPath]] =
    if modules.size == 1 then path(raw).map(Some(_)) else Right(None)

  private def sequence[A](values: List[Either[StageRootError, A]]): Either[StageRootError, List[A]] =
    values.foldRight[Either[StageRootError, List[A]]](Right(Nil))((value, rest) =>
      value.flatMap(item => rest.map(item :: _))
    )

  private def resolve(repo: Path, root: StageRoot, generated: GeneratedRoots): IO[StageRootError, Path] =
    root.origin match
      case StageOrigin.Generated(task, output) =>
        generated match
          case GeneratedRoots.Existing            => ZIO.succeed(repo.resolve(root.path.value))
          case GeneratedRoots.Provided(locations) =>
            val id = GeneratedRootId(task, output)
            locations.filter(_.id == id) match
              case Nil             => ZIO.fail(StageRootError.MissingGenerated(id))
              case location :: Nil =>
                if location.kind == root.extent then ZIO.succeed(location.path)
                else ZIO.fail(StageRootError.OutputKindMismatch(id, root.extent, location.kind))
              case _ :: _ :: _ => ZIO.fail(StageRootError.DuplicateGenerated(id))
      case StageOrigin.Conventional(_) | StageOrigin.Declared | StageOrigin.Unmanaged => ordinary(repo, root)

  private def ordinary(repo: Path, root: StageRoot): IO[StageRootError, Path] =
    val primary = repo.resolve(root.path.value)
    root.origin match
      case StageOrigin.Conventional(Some(fallback)) =>
        ZIO
          .attemptBlocking(if Files.isDirectory(primary) then primary else repo.resolve(fallback.value))
          .mapError(_ => StageRootError.Io(IoFailure(primary, IoFault.Stat)))
      case StageOrigin.Conventional(None) | StageOrigin.Declared | StageOrigin.Unmanaged |
          StageOrigin.Generated(_, _) =>
        ZIO.succeed(primary)

  private def check(repo: Path, root: StageRoot, resolved: Path): IO[StageRootError, Unit] =
    if !resolved.toAbsolutePath.normalize.startsWith(repo.toAbsolutePath.normalize) then
      ZIO.fail(StageRootError.Escaped(resolved))
    else
      ZIO
        .attemptBlocking {
          val base      = repo.toAbsolutePath.normalize
          val absolute  = resolved.toAbsolutePath.normalize
          val ancestors = base
            .relativize(absolute)
            .iterator
            .asScala
            .toList
            .scanLeft(base)((parent, segment) => parent.resolve(segment))
          val present = root.extent match
            case OutputKind.File      => Files.isRegularFile(absolute)
            case OutputKind.Directory => Files.isDirectory(absolute)
          val links = if !present || root.extent == OutputKind.File then ancestors
          else
            val walk = Files.walk(absolute)
            try ancestors ++ walk.iterator.asScala.toList
            finally walk.close()
          links.find(Files.isSymbolicLink(_)) match
            case Some(link)       => Left(StageRootError.Link(link))
            case None if !present =>
              root.origin match
                case StageOrigin.Conventional(_) => Right(())
                case StageOrigin.Unmanaged       =>
                  root.extent match
                    case OutputKind.Directory => Right(())
                    case OutputKind.File      => Left(StageRootError.Missing(absolute, root.extent))
                case StageOrigin.Declared =>
                  root match
                    case StageRoot.Sources(_, StageLayout.Main | StageLayout.Test, _) => Right(())
                    case _ => Left(StageRootError.Missing(absolute, root.extent))
                case StageOrigin.Generated(_, _) => Left(StageRootError.Missing(absolute, root.extent))
            case None => Right(())
        }
        .mapError(_ => StageRootError.Io(IoFailure(resolved, IoFault.Walk)))
        .flatMap(ZIO.fromEither)

  private def readRoot(
      repo: Path,
      stage: CompilationStage,
      generated: GeneratedRoots,
      root: StageRoot,
  ): IO[StageRootError, StageFiles] =
    for
      resolved <- resolve(repo, root, generated)
      _        <- check(repo, root, resolved)
      files    <- root match
        case StageRoot.SourceFile(_, entry, _) =>
          ZIO.succeed(StageFiles(List(PackagedSource(entry.value, resolved)), Nil, Nil))
        case StageRoot.ResourceFile(_, name, _) => ZIO.succeed(StageFiles(Nil, List(name -> resolved), Nil))
        case StageRoot.Sources(_, layout, _)    =>
          val selected = layout match
            case StageLayout.Exact                          => Sources.exact(resolved, stage.cell.platform)
            case StageLayout.Main | StageLayout.TestSibling =>
              Sources.packaged(resolved, stage.cell.version, stage.cell.platform)
            case StageLayout.Test =>
              Sources
                .tests(resolved, stage.cell.version, stage.cell.platform)
                .map(paths => paths.map(path => PackagedSource(Sources.testEntry(resolved, path), path)))
          selected.mapError(StageRootError.Source(_)).map(StageFiles(_, Nil, Nil))
        case StageRoot.Resources(_, layout, _) =>
          val selected = layout match
            case StageLayout.Exact                          => Resources.filesAt(resolved)
            case StageLayout.Main | StageLayout.TestSibling =>
              Resources.selected(resolved, stage.cell.version, stage.cell.platform, SourceSet.Main)
            case StageLayout.Test =>
              Resources.selected(resolved, stage.cell.version, stage.cell.platform, SourceSet.Test)
          selected.mapError(StageRootError.Resource(_)).map(StageFiles(Nil, _, Nil))
    yield files

  private def sourceCollisions(sources: List[PackagedSource]): Either[StageRootError, Unit] =
    sources
      .groupBy(_.entry)
      .toList
      .sortBy(_._1)
      .collectFirst {
        case (entry, files) if files.map(_.path).distinct.size > 1 =>
          StageRootError.SourceCollision(entry, files.map(_.path))
      }
      .toLeft(())

  private def resourceCollisions(
      resources: List[(ResourceName, Path)],
      assets: List[GeneratedAsset],
  ): Either[StageRootError, Unit] =
    val duplicates = resources.groupBy(_._1).toList.sortBy(_._1.value).collectFirst {
      case (name, files) if files.map(_._2).distinct.size > 1 => StageRootError.ResourceCollision(name, files.map(_._2))
    }
    val generated  = assets.map(_.resource)
    val collisions = generated
      .find(name => resources.exists(_._1 == name) || generated.count(_ == name) > 1)
      .map(name => StageRootError.ResourceCollision(name, resources.filter(_._1 == name).map(_._2)))
    duplicates.orElse(collisions).toLeft(())
end StageRoots

object StageRoots:
  def of(graph: ProjectGraph): StageRoots = new StageRoots(graph)
