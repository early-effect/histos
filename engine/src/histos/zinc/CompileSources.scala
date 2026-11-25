package histos.zinc

import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{ArtifactPath, OutputName, PluginTaskId, RepoPath}
import histos.workspace.{IoFailure, IoFault}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

enum SourceIdentity:
  case Checkout(path: RepoPath)
  case Unmanaged(path: RepoPath)
  case Generated(task: PluginTaskId, output: OutputName, entry: ArtifactPath)

  def id: String = this match
    case Checkout(path)                 => s"_checkout_/${path.value}"
    case Unmanaged(path)                => s"_unmanaged_/${path.value}"
    case Generated(task, output, entry) => s"_generated_/${task.render}/${output.value}/${entry.value}"
end SourceIdentity

/** Stable compiler identity paired with the current immutable physical read location. */
final case class CompileSource(identity: SourceIdentity, path: Path)

object CompileSources:
  def of(repo: Path, files: StageFiles): Either[StageRootError, List[CompileSource]] =
    files.sources.foldRight[Either[StageRootError, List[CompileSource]]](Right(Nil)) { (source, rest) =>
      val identity = files.origins.get(source.path) match
        case Some(StageOrigin.Generated(task, output)) =>
          ArtifactPath
            .from(source.entry)
            .left
            .map(_ => StageRootError.InvalidPath(source.entry))
            .map(SourceIdentity.Generated(task, output, _))
        case Some(StageOrigin.Unmanaged) => relative(repo, source.path).map(SourceIdentity.Unmanaged(_))
        case Some(StageOrigin.Conventional(_) | StageOrigin.Declared) | None =>
          relative(repo, source.path).map(SourceIdentity.Checkout(_))
      identity.flatMap(value => rest.map(CompileSource(value, source.path) :: _))
    }

  def digest(inputs: List[CompileSource]): IO[CompileError, Sha256] =
    ZIO
      .foreach(inputs.sortBy(_.identity.id)) { source =>
        ZIO
          .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(source.path))))
          .mapError(_ => CompileError.Io(IoFailure(source.path, IoFault.Read)))
          .map(hash => s"${source.identity.id} ${hash.value}\n")
      }
      .map(rows => Sha256Digest.of(Chunk.fromArray(rows.mkString.getBytes(UTF_8))))

  private def relative(repo: Path, path: Path): Either[StageRootError, RepoPath] =
    val root     = repo.toAbsolutePath.normalize
    val absolute = path.toAbsolutePath.normalize
    if !absolute.startsWith(root) then Left(StageRootError.Escaped(path))
    else
      val raw = root.relativize(absolute).toString.replace('\\', '/')
      RepoPath.from(raw).left.map(_ => StageRootError.InvalidPath(raw))
end CompileSources
