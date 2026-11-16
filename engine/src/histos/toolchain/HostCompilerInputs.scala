package histos.toolchain

import histos.cache.ContentStore
import histos.jdk.{Jdk, JdkVersion, Sha256Digest}
import histos.plugin.{ArtifactPath, ContentHash}
import histos.report.WireJson.given
import histos.schedule.Digest
import histos.workspace.{IoFailure, IoFault}
import histos.ScalaVersion

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.JarFile

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{Chunk, Scope, ZIO, ZLayer}

/** Reads the checked host distribution. It does not solve, and it has no project lock. */
trait HostCompilerInputs:
  def materialize: ZIO[Scope, HostCompilerError, HostCompilerSession]

object HostCompilerInputs:
  val ManifestPath: String = "bootstrap/compiler/manifest.json"

  def layer(payload: Path, jdkHome: Path): ZLayer[ContentStore, Nothing, HostCompilerInputs] =
    ZLayer.fromFunction: (store: ContentStore) =>
      new HostCompilerInputs:
        def materialize: ZIO[Scope, HostCompilerError, HostCompilerSession] =
          HostCompilerInputs.materialize(payload, jdkHome, store)

  /** Manifest fingerprint over the ordered tools. The fingerprint field itself is not an input. */
  def sign(
      version: Int,
      scalaVersion: ScalaVersion,
      tools: List[HostToolEntry],
      jdk: JdkVersion,
  ): HostCompilerManifest =
    val hash = ContentHash.trusted(Sha256Digest.of(Chunk.fromArray(canonical(version, scalaVersion, tools, jdk))).value)
    HostCompilerManifest(version, scalaVersion, tools, jdk, hash)

  def materialize(
      payload: Path,
      jdkHome: Path,
      store: ContentStore,
  ): ZIO[Scope, HostCompilerError, HostCompilerSession] =
    for
      opened <- open(payload)
      raw    <- opened.read(ManifestPath).flatMap {
        case None        => ZIO.fail(HostCompilerError.ManifestAbsent)
        case Some(bytes) => ZIO.succeed(bytes)
      }
      manifest <- decode(raw)
      _        <- check(manifest)
      checked  <- jdk(jdkHome, manifest.jdk)
      lease    <- ZIO.acquireRelease(leaseDir(checked.home))(deleteDir(_).ignore)
      tools    <- ZIO.foreach(manifest.tools.zipWithIndex) { (entry, index) =>
        place(opened, entry, lease, index, store)
      }
      _ <- loaders(tools)
    yield HostCompilerSession(
      manifest,
      tools,
      checked.home,
      checked.version,
      histos.load.KnownStageRegistry.names,
      manifest.fingerprint,
    )

  private def canonical(
      version: Int,
      scalaVersion: ScalaVersion,
      tools: List[HostToolEntry],
      jdk: JdkVersion,
  ): Array[Byte] =
    val lines = s"histos-host-compiler/$version" ::
      s"scala ${scalaVersion.value}" ::
      s"jdk ${jdk.value}" ::
      tools.map: entry =>
        val dep = entry.coordinate
        s"${roleName(entry.role)} ${dep.organization}:${dep.name}:${dep.revision.value} ${entry.digest.value} ${entry.relative.value}"
    lines.mkString("\n").getBytes(StandardCharsets.UTF_8)

  private def roleName(role: HostToolRole): String =
    role match
      case HostToolRole.Compiler   => "compiler"
      case HostToolRole.Bridge     => "bridge"
      case HostToolRole.Interfaces => "interfaces"
      case HostToolRole.Runtime    => "runtime"
      case HostToolRole.Support    => "support"

  private val Unique: Set[HostToolRole] = Set(
    HostToolRole.Compiler,
    HostToolRole.Bridge,
    HostToolRole.Interfaces,
    HostToolRole.Runtime,
  )

  private val Required: List[HostToolRole] = List(
    HostToolRole.Compiler,
    HostToolRole.Bridge,
    HostToolRole.Runtime,
    HostToolRole.Interfaces,
  )

  private def check(manifest: HostCompilerManifest): ZIO[Any, HostCompilerError, Unit] =
    if manifest.version != HostCompilerManifest.Current then
      ZIO.fail(HostCompilerError.IncompatibleManifest(manifest.version))
    else
      conflict(manifest.tools) match
        case Some(role) => ZIO.fail(HostCompilerError.ConflictingRole(role))
        case None       =>
          Required.find(role => !manifest.tools.exists(_.role == role)) match
            case Some(role) => ZIO.fail(HostCompilerError.EntryAbsent(role))
            case None       =>
              val expected = sign(manifest.version, manifest.scalaVersion, manifest.tools, manifest.jdk).fingerprint
              if expected != manifest.fingerprint then
                ZIO.fail(HostCompilerError.Fingerprint(manifest.fingerprint, expected))
              else compilerVersion(manifest)

  private def conflict(tools: List[HostToolEntry]): Option[HostToolRole] =
    tools
      .foldLeft((Set.empty[HostToolRole], Option.empty[HostToolRole])):
        case ((_, found @ Some(_)), _)                                                         => (Set.empty, found)
        case ((seen, None), entry) if Unique.contains(entry.role) && seen.contains(entry.role) =>
          (seen, Some(entry.role))
        case ((seen, None), entry) => (seen + entry.role, None)
      ._2

  private def compilerVersion(manifest: HostCompilerManifest): ZIO[Any, HostCompilerError, Unit] =
    manifest.tools.find(_.role == HostToolRole.Compiler) match
      case None        => ZIO.fail(HostCompilerError.EntryAbsent(HostToolRole.Compiler))
      case Some(entry) =>
        val revision = entry.coordinate.revision.value
        if revision == manifest.scalaVersion.value then ZIO.unit
        else
          val found = ScalaVersion.from(revision).fold(_ => manifest.scalaVersion, identity)
          ZIO.fail(HostCompilerError.IncompatibleCompiler(found, manifest.scalaVersion))

  private final case class CheckedJdk(home: Path, version: JdkVersion)

  private def jdk(home: Path, required: JdkVersion): ZIO[Any, HostCompilerError, CheckedJdk] =
    Jdk
      .home(home)
      .mapError(error => HostCompilerError.Io(jdkIo(home, error)))
      .flatMap: found =>
        found.release match
          case Left(_) => ZIO.fail(HostCompilerError.Io(IoFailure(home, IoFault.Read)))
          case Right(release) if release.version.major.value < required.major.value =>
            ZIO.fail(HostCompilerError.UnsupportedJdk(release.version, required))
          case Right(_) if !found.hasJavac =>
            ZIO.fail(HostCompilerError.Io(IoFailure(home.resolve("bin/javac"), IoFault.Stat)))
          case Right(release) => ZIO.succeed(CheckedJdk(found.path, release.version))

  private def jdkIo(home: Path, error: histos.jdk.JdkError): IoFailure =
    error match
      case histos.jdk.JdkError.Io(failure) => failure
      case histos.jdk.JdkError.BadMajor(_) => IoFailure(home, IoFault.Read)

  private def place(
      opened: Opened,
      entry: HostToolEntry,
      lease: Path,
      index: Int,
      store: ContentStore,
  ): ZIO[Any, HostCompilerError, MaterializedTool] =
    val relative = s"bootstrap/compiler/${entry.relative.value}"
    opened.read(relative).flatMap {
      case None        => ZIO.fail(HostCompilerError.EntryAbsent(entry.role))
      case Some(bytes) =>
        val actual = ContentHash.trusted(Sha256Digest.of(Chunk.fromArray(bytes)).value)
        if actual != entry.digest then ZIO.fail(HostCompilerError.BadDigest(entry.role, entry.digest, actual))
        else
          for
            _    <- keep(store, actual, bytes)
            path <- writeLease(lease, index, entry.relative.value, bytes)
          yield MaterializedTool(entry, path)
    }

  private def keep(store: ContentStore, hash: ContentHash, bytes: Array[Byte]): ZIO[Any, HostCompilerError, Unit] =
    Digest.from(hash.value) match
      case None     => ZIO.fail(HostCompilerError.BadDigest(HostToolRole.Compiler, hash, hash))
      case Some(id) =>
        for
          now <- ZIO.clockWith(_.instant)
          _   <- store.put(id, Chunk.fromArray(bytes), now).mapError(error => HostCompilerError.Io(storeIo(error)))
        yield ()

  private def storeIo(error: histos.schedule.CacheError): IoFailure =
    error match
      case histos.schedule.CacheError.Io(failure) => failure
      case histos.schedule.CacheError.Corrupt(id) => IoFailure(Path.of(id.value), IoFault.Read)

  private def writeLease(
      lease: Path,
      index: Int,
      name: String,
      bytes: Array[Byte],
  ): ZIO[Any, HostCompilerError, Path] =
    val dest = lease.resolve(s"$index-$name")
    ZIO
      .attemptBlocking {
        Files.createDirectories(dest.getParent)
        Files.write(dest, bytes)
        dest
      }
      .mapError(_ => HostCompilerError.Io(IoFailure(dest, IoFault.Write)))

  private def loaders(tools: List[MaterializedTool]): ZIO[Scope, HostCompilerError, Unit] =
    val compiler = tools.collect {
      case tool if tool.entry.role == HostToolRole.Compiler || tool.entry.role == HostToolRole.Support => tool.path
    }
    val bridge = tools.collect { case tool if tool.entry.role == HostToolRole.Bridge => tool.path }
    ZIO.foreachDiscard(List(compiler, bridge))(openLoader)

  private def openLoader(jars: List[Path]): ZIO[Scope, HostCompilerError, Unit] =
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking(new URLClassLoader(jars.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader))
          .mapError(failedLoader)
      )(loader => ZIO.attemptBlocking(loader.close()).ignore)
      .unit

  private def failedLoader(thrown: Throwable): HostCompilerError =
    val _ = thrown
    HostCompilerError.Io(IoFailure(Path.of("compiler-loader"), IoFault.Read))

  private def decode(bytes: Array[Byte]): ZIO[Any, HostCompilerError, HostCompilerManifest] =
    val text = new String(bytes, StandardCharsets.UTF_8)
    ZIO
      .fromEither(text.fromJson[HostCompilerManifest])
      .mapError(_ => HostCompilerError.ManifestUnreadable)

  private def leaseDir(home: Path): ZIO[Any, HostCompilerError, Path] =
    val _ = home
    leaseDirectory

  private def leaseDirectory: ZIO[Any, HostCompilerError, Path] =
    ZIO
      .attemptBlocking(Files.createTempDirectory("histos-host-compiler"))
      .mapError(_ => HostCompilerError.Io(IoFailure(Path.of("host-compiler"), IoFault.Temp)))

  private def deleteDir(path: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(path) then
        val walk = Files.walk(path)
        val all  =
          try walk.iterator.asScala.toList
          finally walk.close()
        all.sortBy(_.getNameCount).reverse.foreach(file => Files.deleteIfExists(file))
      ()
    }.ignore

  private def open(payload: Path): ZIO[Scope, HostCompilerError, Opened] =
    ZIO
      .attemptBlocking(Files.isDirectory(payload))
      .mapError(_ => HostCompilerError.Io(IoFailure(payload, IoFault.Stat)))
      .flatMap: directory =>
        if directory then ZIO.succeed(Opened.Directory(payload))
        else
          ZIO
            .acquireRelease(
              ZIO
                .attemptBlocking(new JarFile(payload.toFile))
                .mapError(_ => HostCompilerError.Io(IoFailure(payload, IoFault.Read)))
            )(jar => ZIO.attemptBlocking(jar.close()).ignore)
            .map(Opened.Archive(_))

  private enum Opened:
    case Directory(root: Path)
    case Archive(jar: JarFile)

    def read(name: String): ZIO[Any, HostCompilerError, Option[Array[Byte]]] =
      this match
        case Directory(root) =>
          val path = root.resolve(name)
          if !path.normalize.startsWith(root.normalize) then
            ZIO.fail(HostCompilerError.Io(IoFailure(path, IoFault.Read)))
          else
            ZIO
              .attemptBlocking(if Files.isRegularFile(path) then Some(Files.readAllBytes(path)) else None)
              .mapError(_ => HostCompilerError.Io(IoFailure(path, IoFault.Read)))
        case Archive(jar) =>
          ZIO
            .attemptBlocking {
              Option(jar.getJarEntry(name)).map: entry =>
                val stream = jar.getInputStream(entry)
                try stream.readAllBytes
                finally stream.close()
            }
            .mapError(_ => HostCompilerError.Io(IoFailure(Path.of(name), IoFault.Read)))

  end Opened

end HostCompilerInputs
