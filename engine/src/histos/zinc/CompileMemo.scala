package histos.zinc

import histos.jdk.{Sha256, Sha256Digest}
import histos.resolve.{ClasspathScope, DependencyLock, Repositories, Resolve}
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, Platform, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Ref, UIO, ULayer, ZIO, ZLayer}

/** Identity of the lock file's bytes, or of the lock value when the file is absent. A rewrite in the same modified tick
  * with the same size is still a new stamp.
  */
final case class LockStamp(token: String)

object LockStamp:
  def file(bytes: Sha256): LockStamp =
    LockStamp(s"f:${bytes.value}")

  def content(hash: Sha256): LockStamp =
    LockStamp(s"c:${hash.value}")
end LockStamp

/** A lock and the stamp that invalidates hashes and classpaths derived from it. */
final case class StampedLock(lock: DependencyLock, stamp: LockStamp)

/** Remembered compile inputs. A value is computed the first time something reads it.
  *
  * Source paths are reused only while every cached path still exists and the directories that contain them are
  * unchanged. A deleted source is not a hit. `clean` and `cleanFull` call [[CompileMemo.forget]].
  */
trait CompileMemo:
  def lock(repo: Path): IO[GraphCompileError, StampedLock]
  def note(lock: DependencyLock): UIO[StampedLock]
  def sources(root: Path, version: ScalaVersion, platform: Platform): IO[CompileError, List[Path]]
  def tests(root: Path, version: ScalaVersion, platform: Platform): IO[CompileError, List[Path]]
  def digest(sources: List[Path]): IO[CompileError, Sha256]
  def hashes(
      lock: DependencyLock,
      modules: List[Module],
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
      stamp: LockStamp,
  ): UIO[Option[List[Sha256]]]
  def classpath(
      repo: Path,
      modules: List[Module],
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories,
      scope: ClasspathScope,
      stamp: LockStamp,
  ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, ScalacClasspath]
  def forget: UIO[Unit]

object CompileMemo:
  def forget: ZIO[CompileMemo, Nothing, Unit] =
    ZIO.serviceWithZIO[CompileMemo](_.forget)

  val live: ULayer[CompileMemo] =
    ZLayer.fromZIO {
      for
        locks      <- Ref.make(Map.empty[String, (LockStamp, DependencyLock)])
        paths      <- Ref.make(Map.empty[PathKey, Listed])
        digests    <- Ref.make(Map.empty[List[FileStamp], Sha256])
        hashes     <- Ref.make(Map.empty[HashKey, Option[List[Sha256]]])
        classpaths <- Ref.make(Map.empty[ClassKey, ScalacClasspath])
      yield Live(locks, paths, digests, hashes, classpaths)
    }

  /** One memo for `effect`, closed over nothing else. The daemon holds one for its lifetime instead. */
  def provide[R, E, A](effect: ZIO[R & CompileMemo, E, A]): ZIO[R, E, A] =
    ZIO.scoped(
      live.build.flatMap(env => effect.provideSomeLayer(ZLayer.succeed(env.get[CompileMemo])))
    )

  private final case class PathKey(root: String, version: String, platform: String, tests: Boolean)
  private final case class DirMark(path: Path, modified: Long)
  private final case class Listed(paths: List[Path], dirs: List[DirMark], missing: Boolean)
  private final case class FileStamp(path: String, modified: Long, size: Long)
  private final case class HashKey(module: String, version: String, platform: String, scope: String, stamp: String)
  private final case class ClassKey(
      repo: String,
      module: String,
      version: String,
      platform: String,
      scope: String,
      stamp: String,
  )

  private final class Live(
      locks: Ref[Map[String, (LockStamp, DependencyLock)]],
      paths: Ref[Map[PathKey, Listed]],
      digests: Ref[Map[List[FileStamp], Sha256]],
      hashes: Ref[Map[HashKey, Option[List[Sha256]]]],
      classpaths: Ref[Map[ClassKey, ScalacClasspath]],
  ) extends CompileMemo:
    def lock(repo: Path): IO[GraphCompileError, StampedLock] =
      val path = Resolve.lockPath(repo)
      val key  = path.toAbsolutePath.normalize.toString
      ZIO
        .attemptBlocking(Option.when(Files.isRegularFile(path))(Files.readAllBytes(path)))
        .mapError(_ => GraphCompileError.LockRead(path))
        .flatMap:
          case Some(bytes) =>
            val stamp = LockStamp.file(Sha256Digest.of(Chunk.fromArray(bytes)))
            locks.get.flatMap: cached =>
              cached.get(key) match
                case Some((known, parsed)) if known == stamp => ZIO.succeed(StampedLock(parsed, stamp))
                case _                                       =>
                  parsed(bytes).flatMap: lock =>
                    locks.update(_.updated(key, (stamp, lock))).as(StampedLock(lock, stamp))
          case None =>
            read(repo).map(parsed => StampedLock(parsed, contentStamp(parsed)))

    def note(lock: DependencyLock): UIO[StampedLock] =
      ZIO.succeed(StampedLock(lock, contentStamp(lock)))

    def sources(root: Path, version: ScalaVersion, platform: Platform): IO[CompileError, List[Path]] =
      listed(root, version, platform, tests = false)

    def tests(root: Path, version: ScalaVersion, platform: Platform): IO[CompileError, List[Path]] =
      listed(root, version, platform, tests = true)

    def digest(sources: List[Path]): IO[CompileError, Sha256] =
      val failed = sources.headOption.getOrElse(Path.of("."))
      ZIO
        .attemptBlocking(fileStamps(sources))
        .mapError(_ => CompileError.Io(IoFailure(failed, IoFault.Read)))
        .flatMap: marks =>
          digests.get.flatMap: cached =>
            cached.get(marks) match
              case Some(hash) => ZIO.succeed(hash)
              case None       =>
                JvmCompile.sourceDigest(sources).tap(hash => digests.update(_.updated(marks, hash)))

    def hashes(
        lock: DependencyLock,
        modules: List[Module],
        module: Module,
        version: ScalaVersion,
        platform: Platform,
        scope: ClasspathScope,
        stamp: LockStamp,
    ): UIO[Option[List[Sha256]]] =
      val key = HashKey(module.name.value, version.value, platform.token, scope.header, stamp.token)
      hashes.get.flatMap: cached =>
        cached.get(key) match
          case Some(found) => ZIO.succeed(found)
          case None        =>
            val found = ScalacClasspath.lockHashes(lock, modules, module, version, platform, scope)
            hashes.update(_.updated(key, found)).as(found)

    def classpath(
        repo: Path,
        modules: List[Module],
        module: Module,
        version: ScalaVersion,
        platform: Platform,
        lock: DependencyLock,
        repositories: Repositories,
        scope: ClasspathScope,
        stamp: LockStamp,
    ): ZIO[Resolve & histos.resolve.LockedTools, GraphCompileError, ScalacClasspath] =
      val key = ClassKey(
        repo.toAbsolutePath.normalize.toString,
        module.name.value,
        version.value,
        platform.token,
        scope.header,
        stamp.token,
      )
      classpaths.get.flatMap: cached =>
        cached.get(key) match
          case Some(found) => ZIO.succeed(found)
          case None        =>
            ScalacClasspath
              .of(repo, modules, module, version, platform, lock, repositories, scope)
              .tap(found => classpaths.update(_.updated(key, found)))

    def forget: UIO[Unit] =
      locks.set(Map.empty) *>
        paths.set(Map.empty) *>
        digests.set(Map.empty) *>
        hashes.set(Map.empty) *>
        classpaths.set(Map.empty)

    private def listed(
        root: Path,
        version: ScalaVersion,
        platform: Platform,
        tests: Boolean,
    ): IO[CompileError, List[Path]] =
      val key = PathKey(root.toAbsolutePath.normalize.toString, version.value, platform.token, tests)
      paths.get.flatMap: cached =>
        cached.get(key) match
          case Some(found) =>
            current(root, found).flatMap:
              case Some(kept) => ZIO.succeed(kept)
              case None       => fill(key, root, version, platform, tests)
          case None => fill(key, root, version, platform, tests)

    private def current(root: Path, found: Listed): IO[CompileError, Option[List[Path]]] =
      ZIO
        .attemptBlocking:
          if found.missing then if Files.exists(root) then None else Some(found.paths)
          else if found.paths.forall(path => Files.isRegularFile(path)) && directoriesMatch(found.dirs) then
            Some(found.paths)
          else None
        .mapError(_ => CompileError.Io(IoFailure(root, IoFault.Read)))

    private def fill(
        key: PathKey,
        root: Path,
        version: ScalaVersion,
        platform: Platform,
        tests: Boolean,
    ): IO[CompileError, List[Path]] =
      val selected =
        if tests then Sources.tests(root, version, platform) else Sources.select(root, version, platform)
      for
        found <- selected
        shot  <- ZIO
          .attemptBlocking(snapshot(root, found.filter(path => Files.isRegularFile(path))))
          .mapError(_ => CompileError.Io(IoFailure(root, IoFault.Read)))
        _ <- paths.update(_.updated(key, shot))
      yield shot.paths

    private def read(repo: Path): IO[GraphCompileError, DependencyLock] =
      Resolve
        .readLock(repo)
        .mapError:
          case histos.resolve.ResolveError.Io(failure) => GraphCompileError.LockRead(failure.path)
          case other                                   => GraphCompileError.Lock(other)

    /** The bytes the stamp hashed, so a rewrite after that read cannot pair this stamp with another lock. */
    private def parsed(bytes: Array[Byte]): IO[GraphCompileError, DependencyLock] =
      ZIO.fromEither(
        DependencyLock
          .parse(new String(bytes, StandardCharsets.UTF_8))
          .left
          .map(error => GraphCompileError.Lock(histos.resolve.ResolveError.Lock(error)))
      )
  end Live

  private def contentStamp(lock: DependencyLock): LockStamp =
    val text = lock.cells
      .map: cell =>
        val artifacts = cell.artifacts.map(_.sha256.value).mkString(",")
        s"${cell.module.value} ${cell.scalaVersion.value} ${cell.platform.token} ${cell.scope.header} $artifacts"
      .mkString("\n")
    LockStamp.content(Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))))

  private def fileStamps(sources: List[Path]): List[FileStamp] =
    sources
      .map: path =>
        val normal = path.toAbsolutePath.normalize
        FileStamp(normal.toString, Files.getLastModifiedTime(normal).toMillis, Files.size(normal))
      .sortBy(_.path)

  private def directoriesMatch(dirs: List[DirMark]): Boolean =
    dirs.forall: dir =>
      Files.isDirectory(dir.path) && Files.getLastModifiedTime(dir.path).toMillis == dir.modified

  private def snapshot(root: Path, present: List[Path]): Listed =
    if !Files.isDirectory(root) then Listed(Nil, Nil, missing = true)
    else
      val walk = Files.walk(root)
      val dirs =
        try
          walk.iterator.asScala
            .filter(dir => Files.isDirectory(dir))
            .map: dir =>
              val normal = dir.toAbsolutePath.normalize
              DirMark(normal, Files.getLastModifiedTime(normal).toMillis)
            .toList
        finally walk.close()
      Listed(present.map(_.toAbsolutePath.normalize), dirs, missing = false)
end CompileMemo
