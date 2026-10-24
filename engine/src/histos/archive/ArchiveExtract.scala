package histos.archive

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermission

import scala.jdk.CollectionConverters.*
import scala.util.Try

import zio.{IO, ZIO}
import zio.stream.ZStream

private[archive] final case class ArchiveExtract(
    decoder: ArchiveDecoder,
    budget: ArchiveBudget
):
  def unpack(archive: Path, root: Path): IO[ArchiveStageError, Unit] =
    for
      state <- decoder
        .fold(archive, budget, State())((state, entry) =>
          extract(archive, root, state, entry)
        )
        .mapError:
          case failure: ExtractFailure =>
            ArchiveStageError.Extract(archive, failure)
          case failure: ArchiveStageError => failure
      _ <- ZIO.foreachDiscard(state.links): link =>
        val traverses =
          state.links.exists(other =>
            link.trace.exists(path => samePrefix(path, other.path))
          )
        for
          _ <- ZIO
            .fail(ArchiveStageError.UnsafeLink(link.name, link.raw))
            .when(
              traverses || (link.kind == LinkKind.Symbolic && link.path
                .startsWith(
                  link.target
                )) || (link.kind == LinkKind.Hard && !state.regular
                .contains(link.target))
            )
          present <- ZIO
            .attemptBlocking(
              Files.exists(link.target, LinkOption.NOFOLLOW_LINKS)
            )
            .mapError(_ =>
              ArchiveStageError.Io(IoFailure(link.target, IoFault.Stat))
            )
          _ <- ZIO
            .fail(ArchiveStageError.UnsafeLink(link.name, link.raw))
            .unless(present)
        yield ()
      _ <- ZIO.foreachDiscard(state.links): link =>
        parents(root, link.path, link.name) *> ZIO
          .attemptBlocking:
            link.kind match
              case LinkKind.Symbolic =>
                Files.createSymbolicLink(
                  link.path,
                  Option(link.path.getParent)
                    .fold(link.target)(_.relativize(link.target))
                )
              case LinkKind.Hard => Files.createLink(link.path, link.target)
          .mapError(_ =>
            ArchiveStageError.Io(IoFailure(link.path, IoFault.Create))
          )
          .unit
      _ <- ZIO.foreachDiscard(
        state.directories.sortBy(_._1.getNameCount).reverse
      )((path, mode) => permissions(path, mode))
    yield ()

  private enum LinkKind:
    case Symbolic, Hard

  private final case class Link(
      path: Path,
      target: Path,
      raw: String,
      name: String,
      kind: LinkKind,
      trace: List[Path]
  )

  private final case class State(
      seen: Map[Path, ArchiveEntryKind] = Map.empty,
      regular: Set[Path] = Set.empty,
      links: List[Link] = Nil,
      directories: List[(Path, Int)] = Nil,
      bytes: Long = 0L
  )

  private def extract(
      archive: Path,
      root: Path,
      state: State,
      entry: DecodedEntry
  ): IO[ArchiveStageError, State] =
    for
      _ <- ZIO
        .fail(
          ArchiveStageError.Budget(
            ArchiveBudgetError
              .Exceeded(ArchiveLimit.Entries, budget.entries.toLong)
          )
        )
        .when(state.seen.size >= budget.entries)
      path <- ZIO.fromEither(safe(root, entry.name))
      _ <- ZIO
        .fail(ArchiveStageError.UnsafePath(entry.name))
        .when(
          path.startsWith(root.resolve(ArchiveStage.ManifestName)) || state.seen
            .contains(path) ||
            state.seen.exists((previous, kind) =>
              (path
                .startsWith(previous) && kind != ArchiveEntryKind.Directory) ||
                (previous
                  .startsWith(path) && entry.kind != ArchiveEntryKind.Directory)
            ) || (path == root && entry.kind != ArchiveEntryKind.Directory)
        )
      _ <- parents(root, path, entry.name)
      next <- entry.kind match
        case ArchiveEntryKind.File =>
          for
            count <- write(
              entry.body.mapError(failure =>
                ArchiveStageError.Extract(archive, failure)
              ),
              path,
              state.bytes
            )
            _ <- entry.mode.fold(zipLauncher(path))(permissions(path, _))
          yield state.copy(
            regular = state.regular + path,
            bytes = state.bytes + count
          )
        case ArchiveEntryKind.Directory =>
          for _ <- Workspace
              .createDirectories(path)
              .mapError(ArchiveStageError.Io(_))
          yield state.copy(directories =
            (path -> entry.mode.getOrElse(448)) :: state.directories
          )
        case ArchiveEntryKind.SymbolicLink(raw) =>
          ZIO
            .fromEither(
              linkTarget(root, path, entry.name, raw, LinkKind.Symbolic)
            )
            .map(target =>
              state.copy(links =
                Link(
                  path,
                  target.path,
                  raw,
                  entry.name,
                  LinkKind.Symbolic,
                  target.trace
                ) :: state.links
              )
            )
        case ArchiveEntryKind.HardLink(raw) =>
          ZIO
            .fromEither(linkTarget(root, path, entry.name, raw, LinkKind.Hard))
            .map(target =>
              state.copy(links =
                Link(
                  path,
                  target.path,
                  raw,
                  entry.name,
                  LinkKind.Hard,
                  target.trace
                ) :: state.links
              )
            )
    yield next.copy(seen = next.seen.updated(path, entry.kind))

  private def write(
      stream: ZStream[Any, ArchiveStageError, Byte],
      path: Path,
      previous: Long
  ): IO[ArchiveStageError, Long] =
    ZIO.scoped:
      for
        output <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(
              Files.newOutputStream(
                path,
                StandardOpenOption.CREATE_NEW,
                StandardOpenOption.WRITE
              )
            )
            .mapError:
              case _: java.nio.file.FileAlreadyExistsException =>
                ArchiveStageError.UnsafePath(path.toString)
              case _ => ArchiveStageError.Io(IoFailure(path, IoFault.Write))
        )(output => ZIO.attemptBlocking(output.close()).ignore)
        count <- stream
          .rechunk(32768)
          .chunks
          .runFoldZIO(0L): (count, bytes) =>
            for
              _ <- ZIO
                .fail(
                  ArchiveStageError.Budget(
                    ArchiveBudgetError
                      .Exceeded(ArchiveLimit.EntryBytes, budget.entryBytes)
                  )
                )
                .when(bytes.length.toLong > budget.entryBytes - count)
              _ <- ZIO
                .fail(
                  ArchiveStageError.Budget(
                    ArchiveBudgetError.Exceeded(
                      ArchiveLimit.ExtractedBytes,
                      budget.extractedBytes
                    )
                  )
                )
                .when(
                  bytes.length.toLong > budget.extractedBytes - previous - count
                )
              _ <- ZIO
                .attemptBlocking(output.write(bytes.toArray))
                .mapError(_ =>
                  ArchiveStageError.Io(IoFailure(path, IoFault.Write))
                )
            yield count + bytes.length
      yield count

  private def parents(
      root: Path,
      path: Path,
      name: String
  ): IO[ArchiveStageError, Unit] =
    val ancestors = Iterator
      .iterate(Option(path.getParent))(
        _.flatMap(parent => Option(parent.getParent))
      )
      .takeWhile(_.exists(_.startsWith(root)))
      .flatten
      .toList
      .reverse
    ZIO.foreachDiscard(ancestors): parent =>
      for
        conflict <- ZIO
          .attemptBlocking(
            Files.isSymbolicLink(parent) || (Files.exists(
              parent,
              LinkOption.NOFOLLOW_LINKS
            ) && !Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS))
          )
          .mapError(_ => ArchiveStageError.Io(IoFailure(parent, IoFault.Stat)))
        _ <- ZIO.fail(ArchiveStageError.UnsafePath(name)).when(conflict)
        _ <- Workspace
          .createDirectories(parent)
          .mapError(ArchiveStageError.Io(_))
      yield ()

  private def safe(root: Path, name: String): Either[ArchiveStageError, Path] =
    val cleaned = name.replace('\\', '/')
    val parts =
      cleaned.split("/").toList.filter(part => part.nonEmpty && part != ".")
    if name.isEmpty || cleaned.startsWith("/") || parts.exists(part =>
        part == ".." || part.contains(":") || part.contains('\u0000')
      )
    then Left(ArchiveStageError.UnsafePath(name))
    else
      Try(parts.foldLeft(root)(_.resolve(_)).normalize).toEither.left
        .map(_ => ArchiveStageError.UnsafePath(name))
        .filterOrElse(_.startsWith(root), ArchiveStageError.UnsafePath(name))

  private final case class Target(path: Path, trace: List[Path])

  private def samePrefix(path: Path, prefix: Path): Boolean =
    val candidate = path.toString.toLowerCase(java.util.Locale.ROOT)
    val linked = prefix.toString.toLowerCase(java.util.Locale.ROOT)
    candidate == linked || candidate.startsWith(
      linked + path.getFileSystem.getSeparator
    )

  private def linkTarget(
      root: Path,
      path: Path,
      name: String,
      raw: String,
      kind: LinkKind
  ): Either[ArchiveStageError, Target] =
    Try(Path.of(raw.replace('\\', '/'))).toEither.left
      .map(_ => ArchiveStageError.UnsafeLink(name, raw))
      .flatMap: relative =>
        val base = if kind == LinkKind.Hard then root
        else Option(path.getParent).getOrElse(root)
        val (target, trace) =
          relative.iterator.asScala.foldLeft((base, List.empty[Path])):
            case ((current, trace), component) =>
              val next = current.resolve(component).normalize
              (next, next :: trace)
        Either.cond(
          raw.nonEmpty && !relative.isAbsolute && !raw.contains(":") && target
            .startsWith(root) && target != root && trace
            .forall(_.startsWith(root)),
          Target(target, trace),
          ArchiveStageError.UnsafeLink(name, raw)
        )

  private val permissionBits = List(
    256 -> PosixFilePermission.OWNER_READ,
    128 -> PosixFilePermission.OWNER_WRITE,
    64 -> PosixFilePermission.OWNER_EXECUTE,
    32 -> PosixFilePermission.GROUP_READ,
    16 -> PosixFilePermission.GROUP_WRITE,
    8 -> PosixFilePermission.GROUP_EXECUTE,
    4 -> PosixFilePermission.OTHERS_READ,
    2 -> PosixFilePermission.OTHERS_WRITE,
    1 -> PosixFilePermission.OTHERS_EXECUTE
  )

  private def permissions(path: Path, mode: Int): IO[ArchiveStageError, Unit] =
    ZIO
      .attemptBlocking:
        if Files.getFileStore(path).supportsFileAttributeView("posix") then
          val effective =
            if Files.isDirectory(path) then 448 else 384 | (mode & 73)
          Files.setPosixFilePermissions(
            path,
            permissionBits
              .collect:
                case (bit, permission) if (effective & bit) != 0 => permission
              .toSet
              .asJava
          )
      .mapError(_ => ArchiveStageError.Io(IoFailure(path, IoFault.Write)))
      .unit

  private def zipLauncher(path: Path): IO[ArchiveStageError, Unit] =
    val launcher = Option(path.getFileName).exists(name =>
      Set("java", "javac", "node").contains(name.toString)
    ) &&
      Option(path.getParent)
        .flatMap(parent => Option(parent.getFileName))
        .exists(_.toString == "bin")
    if launcher then permissions(path, 493) else ZIO.unit
