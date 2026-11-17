package histos.load

import histos.plugin.{ApiDigest, LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.resolve.{
  ArtifactName,
  BuildResolvers,
  DependencyLock,
  PluginCells,
  Repositories,
  Resolve,
  ResolveError,
  ResolvedArtifact,
}
import histos.toolchain.BuildCompiler
import histos.workspace.IoFault
import histos.{Dep, Platform, ProjectGraph}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.ZipFile

import zio.{IO, ZIO}

/** Jars for ship pins. `Resolve` is the seam: a test supplies a local jar and does not call Coursier. `Api` is refused.
  * A lock fetches the recorded jars. A solve asks `Resolve`. The direct jar must name this process's api digest.
  */
private[load] object PluginShips:
  def jars(
      graph: ProjectGraph,
      resolve: Resolve,
      launcher: ApiDigest,
      lock: Option[DependencyLock],
      hosts: Set[String] = Set.empty,
  ): IO[LoadError, List[Path]] =
    val pins = graph.coordinates.collect:
      case PluginCoordinate.Published(dependency, link) => Pin(dependency, link)
    if pins.isEmpty then ZIO.succeed(Nil)
    else
      val repositories = BuildResolvers(graph.resolvers)
      ZIO.foreach(pins)(pin => one(resolve, repositories, launcher, lock, hosts, pin)).map(_.flatten)

  private final case class Pin(dependency: Dep, link: LinkStrategy)

  private def one(
      resolve: Resolve,
      repositories: Repositories,
      launcher: ApiDigest,
      lock: Option[DependencyLock],
      hosts: Set[String],
      pin: Pin,
  ): IO[LoadError, List[Path]] =
    pin.link match
      case LinkStrategy.Api =>
        ZIO.fail(LoadError.Plugin(PluginLoadError.ApiLink(pin.dependency)))
      case LinkStrategy.ExactBinary | LinkStrategy.Source | LinkStrategy.BinaryOrSource =>
        lock match
          case Some(held) => fetched(resolve, held, launcher, hosts, pin).flatMap(refuse)
          case None       => solved(resolve, repositories, launcher, hosts, pin).flatMap(refuse)

  private def solved(
      resolve: Resolve,
      repositories: Repositories,
      launcher: ApiDigest,
      hosts: Set[String],
      pin: Pin,
  ): IO[LoadError, Stamped] =
    resolve
      .resolvePlugin(pin.dependency, repositories)
      .mapError(LoadError.Resolve(_))
      .flatMap: classpath =>
        stampAll(pin.dependency, classpath.artifacts, launcher, hosts)

  private def fetched(
      resolve: Resolve,
      lock: DependencyLock,
      launcher: ApiDigest,
      hosts: Set[String],
      pin: Pin,
  ): IO[LoadError, Stamped] =
    ZIO
      .fromEither(PluginCells.locate(lock, pin.dependency))
      .mapError(LoadError.Plugin(_))
      .flatMap: locked =>
        ZIO
          .foreach(locked.artifacts)(artifact => resolve.fetch(artifact).mapError(LoadError.Resolve(_)))
          .flatMap: artifacts =>
            stampAll(pin.dependency, artifacts, launcher, hosts)

  /** The coordinate's own jar, not a transitive one. The published name and the `Dep` name both match a stub.
    */
  private def direct(dependency: Dep, artifacts: List[ResolvedArtifact]): IO[LoadError, Path] =
    BuildCompiler.version
      .mapError(LoadError.Resolve(_))
      .flatMap: version =>
        val expected = ArtifactName.of(dependency, version, Platform.Jvm)
        val found    = artifacts.find: artifact =>
          artifact.organization == dependency.organization &&
            artifact.revision == dependency.revision.value &&
            (artifact.name == expected || artifact.name == dependency.name)
        found match
          case Some(artifact) => ZIO.succeed(artifact.path)
          case None           =>
            ZIO.fail(
              LoadError.Resolve(ResolveError.NotFound(dependency.organization, expected, dependency.revision.value))
            )

  /** Paths plus the first binding drift. `jars` refuses that drift. A rebuildable publication may compile it instead.
    */
  private[load] final case class Stamped(paths: List[Path], drift: Option[PluginLoadError.BindingDrift])

  /** Drift stays on the result so a rebuildable jar can be compiled instead of refused. */
  private[load] def inspect(
      graph: ProjectGraph,
      resolve: Resolve,
      launcher: ApiDigest,
      lock: Option[DependencyLock],
      hosts: Set[String] = Set.empty,
  ): IO[LoadError, List[Stamped]] =
    val pins = graph.coordinates.collect:
      case PluginCoordinate.Published(dependency, link) => Pin(dependency, link)
    if pins.isEmpty then ZIO.succeed(Nil)
    else
      val repositories = BuildResolvers(graph.resolvers)
      ZIO.foreach(pins): pin =>
        pin.link match
          case LinkStrategy.Api =>
            ZIO.fail(LoadError.Plugin(PluginLoadError.ApiLink(pin.dependency)))
          case LinkStrategy.ExactBinary | LinkStrategy.Source | LinkStrategy.BinaryOrSource =>
            lock match
              case Some(held) => fetched(resolve, held, launcher, hosts, pin)
              case None       => solved(resolve, repositories, launcher, hosts, pin)

  /** Every jar that carries a version file is checked. A direct jar without one is unversioned. Another jar without one
    * is an ordinary dependency and stays pinned. A host-identity jar is not a second API copy.
    */
  private def stampAll(
      dependency: Dep,
      artifacts: List[ResolvedArtifact],
      launcher: ApiDigest,
      hosts: Set[String],
  ): IO[LoadError, Stamped] =
    direct(dependency, artifacts).flatMap: root =>
      ZIO
        .foldLeft(artifacts)(Option.empty[PluginLoadError.BindingDrift]): (drift, artifact) =>
          drift match
            case some @ Some(_) => ZIO.succeed(some)
            case None           =>
              stamped(artifact.path, dependency, launcher, artifact.path == root, hosts)
        .map(drift => Stamped(artifacts.map(_.path), drift))

  private def refuse(stamped: Stamped): IO[LoadError, List[Path]] =
    stamped.drift match
      case Some(drift) => ZIO.fail(LoadError.Plugin(drift))
      case None        => ZIO.succeed(stamped.paths)

  private def stamped(
      path: Path,
      dependency: Dep,
      launcher: ApiDigest,
      root: Boolean,
      hosts: Set[String],
  ): IO[LoadError, Option[PluginLoadError.BindingDrift]] =
    val coordinate = PluginCoordinate.Published(dependency).render
    fileHash(path).flatMap: hash =>
      if hash == launcher.value || hosts.contains(hash) then ZIO.succeed(None)
      else
        versionText(path).flatMap:
          case None if root => ZIO.fail(LoadError.Plugin(PluginLoadError.Unversioned(coordinate)))
          case None         => ZIO.succeed(None)
          case Some(raw)    =>
            ApiDigest.from(raw) match
              case Left(_) =>
                ZIO.fail(LoadError.Plugin(PluginLoadError.Unversioned(coordinate)))
              case Right(found) if found == launcher =>
                ZIO.succeed(None)
              case Right(found) =>
                ZIO.succeed(Some(PluginLoadError.BindingDrift(path, found, launcher)))

  private def fileHash(path: Path): IO[LoadError, String] =
    ZIO
      .attemptBlocking(java.nio.file.Files.readAllBytes(path))
      .mapError(BuildFile.failed(path, IoFault.Read))
      .map(bytes => histos.jdk.Sha256Digest.of(zio.Chunk.fromArray(bytes)).value)

  private def versionText(path: Path): IO[LoadError, Option[String]] =
    ZIO.scoped:
      ZIO
        .acquireRelease(
          ZIO.attemptBlocking(new ZipFile(path.toFile)).mapError(BuildFile.failed(path, IoFault.Read))
        )(zip => ZIO.attemptBlocking(zip.close()).ignore)
        .flatMap: zip =>
          Option(zip.getEntry(ApiDigest.resource)) match
            case None =>
              ZIO.succeed(None)
            case Some(entry) =>
              ZIO
                .attemptBlocking:
                  val stream = zip.getInputStream(entry)
                  try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
                  finally stream.close()
                .mapError(BuildFile.failed(path, IoFault.Read))
                .map: text =>
                  val raw =
                    if text.endsWith("\n") then text.stripSuffix("\n").stripSuffix("\r")
                    else text
                  Some(raw)
end PluginShips
