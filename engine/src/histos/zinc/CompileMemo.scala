package histos.zinc

import histos.jdk.{Sha256, Sha256Digest}
import histos.resolve.{ClasspathScope, DependencyLock, Repositories, Resolve}
import histos.{Module, Platform, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

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

/** Remembered compile inputs that the lock's bytes key. A value is computed the first time something reads it.
  *
  * Nothing the source tree decides is remembered: a compile lists the tree and hashes the bytes every time. `clean` and
  * `cleanFull` call [[CompileMemo.forget]].
  */
trait CompileMemo:
  def lock(repo: Path): IO[GraphCompileError, StampedLock]
  def note(lock: DependencyLock): UIO[StampedLock]
  def hashes(
      lock: DependencyLock,
      module: Module,
      upstream: List[UpstreamCell],
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
      stamp: LockStamp,
  ): UIO[Option[List[Sha256]]]
  def classpath(
      repo: Path,
      module: Module,
      upstream: List[UpstreamCell],
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
        hashes     <- Ref.make(Map.empty[HashKey, Option[List[Sha256]]])
        classpaths <- Ref.make(Map.empty[ClassKey, ScalacClasspath])
      yield Live(locks, hashes, classpaths)
    }

  /** One memo for `effect`, closed over nothing else. The daemon holds one for its lifetime instead. */
  def provide[R, E, A](effect: ZIO[R & CompileMemo, E, A]): ZIO[R, E, A] =
    ZIO.scoped(
      live.build.flatMap(env => effect.provideSomeLayer(ZLayer.succeed(env.get[CompileMemo])))
    )

  private final case class HashKey(
      module: String,
      upstream: List[String],
      version: String,
      platform: String,
      scope: String,
      stamp: String,
  )
  private final case class ClassKey(
      repo: String,
      module: String,
      upstream: List[String],
      version: String,
      platform: String,
      scope: String,
      stamp: String,
  )

  /** Each upstream cell the key's classpath reads, so a different pick is a different entry. */
  private def cells(upstream: List[UpstreamCell]): List[String] =
    upstream.map(cell => s"${cell.module.name.value} ${cell.version.value} ${cell.classes}")

  private final class Live(
      locks: Ref[Map[String, (LockStamp, DependencyLock)]],
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

    def hashes(
        lock: DependencyLock,
        module: Module,
        upstream: List[UpstreamCell],
        version: ScalaVersion,
        platform: Platform,
        scope: ClasspathScope,
        stamp: LockStamp,
    ): UIO[Option[List[Sha256]]] =
      val key = HashKey(module.name.value, cells(upstream), version.value, platform.token, scope.header, stamp.token)
      hashes.get.flatMap: cached =>
        cached.get(key) match
          case Some(found) => ZIO.succeed(found)
          case None        =>
            val found = ScalacClasspath.lockHashes(lock, module, upstream, version, platform, scope)
            hashes.update(_.updated(key, found)).as(found)

    def classpath(
        repo: Path,
        module: Module,
        upstream: List[UpstreamCell],
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
        cells(upstream),
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
              .of(repo, module, upstream, version, platform, lock, repositories, scope)
              .tap(found => classpaths.update(_.updated(key, found)))

    def forget: UIO[Unit] =
      locks.set(Map.empty) *>
        hashes.set(Map.empty) *>
        classpaths.set(Map.empty)

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
end CompileMemo
