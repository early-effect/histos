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
  ): IO[LoadError, List[Path]] =
    val pins = graph.coordinates.collect:
      case PluginCoordinate.Published(dependency, link) => Pin(dependency, link)
    if pins.isEmpty then ZIO.succeed(Nil)
    else
      val repositories = BuildResolvers(graph.resolvers)
      ZIO.foreach(pins)(pin => one(resolve, repositories, launcher, lock, pin)).map(_.flatten)

  private final case class Pin(dependency: Dep, link: LinkStrategy)

  private def one(
      resolve: Resolve,
      repositories: Repositories,
      launcher: ApiDigest,
      lock: Option[DependencyLock],
      pin: Pin,
  ): IO[LoadError, List[Path]] =
    pin.link match
      case LinkStrategy.Api =>
        ZIO.fail(LoadError.Plugin(PluginLoadError.ApiLink(pin.dependency)))
      case LinkStrategy.ExactBinary | LinkStrategy.Source | LinkStrategy.BinaryOrSource =>
        lock match
          case Some(held) => fetched(resolve, held, launcher, pin)
          case None       => solved(resolve, repositories, launcher, pin)

  private def solved(
      resolve: Resolve,
      repositories: Repositories,
      launcher: ApiDigest,
      pin: Pin,
  ): IO[LoadError, List[Path]] =
    resolve
      .resolvePlugin(pin.dependency, repositories)
      .mapError(LoadError.Resolve(_))
      .flatMap: classpath =>
        direct(pin.dependency, classpath.artifacts).flatMap: path =>
          stamped(path, pin.dependency, launcher).as(classpath.artifacts.map(_.path))

  private def fetched(
      resolve: Resolve,
      lock: DependencyLock,
      launcher: ApiDigest,
      pin: Pin,
  ): IO[LoadError, List[Path]] =
    ZIO
      .fromEither(PluginCells.locate(lock, pin.dependency))
      .mapError(LoadError.Plugin(_))
      .flatMap: locked =>
        ZIO
          .foreach(locked.artifacts)(artifact => resolve.fetch(artifact).mapError(LoadError.Resolve(_)))
          .flatMap: artifacts =>
            direct(pin.dependency, artifacts).flatMap: path =>
              stamped(path, pin.dependency, launcher).as(artifacts.map(_.path))

  /** The coordinate's own jar, not a transitive one. The published name and the `Dep` name both match a stub.
    */
  private def direct(dependency: Dep, artifacts: List[ResolvedArtifact]): IO[LoadError, Path] =
    BuildCompiler.version
      .mapError(LoadError.Resolve(_))
      .flatMap: version =>
        val expected = ArtifactName.of(dependency, version, Platform.Jvm)
        val found    = artifacts.find: artifact =>
          artifact.organization == dependency.organization &&
            artifact.revision == dependency.revision &&
            (artifact.name == expected || artifact.name == dependency.name)
        found match
          case Some(artifact) => ZIO.succeed(artifact.path)
          case None           =>
            ZIO.fail(
              LoadError.Resolve(ResolveError.NotFound(dependency.organization, expected, dependency.revision))
            )

  private def stamped(path: Path, dependency: Dep, launcher: ApiDigest): IO[LoadError, Unit] =
    val coordinate = PluginCoordinate.Published(dependency).render
    ZIO.scoped:
      ZIO
        .acquireRelease(
          ZIO.attemptBlocking(new ZipFile(path.toFile)).mapError(BuildFile.failed(path, IoFault.Read))
        )(zip => ZIO.attemptBlocking(zip.close()).ignore)
        .flatMap: zip =>
          zip.getEntry(ApiDigest.resource) match
            case null =>
              ZIO.fail(LoadError.Plugin(PluginLoadError.Unversioned(coordinate)))
            case entry =>
              ZIO
                .attemptBlocking:
                  val stream = zip.getInputStream(entry)
                  try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
                  finally stream.close()
                .mapError(BuildFile.failed(path, IoFault.Read))
                .flatMap: text =>
                  val raw =
                    if text.endsWith("\n") then text.stripSuffix("\n").stripSuffix("\r")
                    else text
                  ApiDigest.from(raw) match
                    case Left(_) =>
                      ZIO.fail(LoadError.Plugin(PluginLoadError.Unversioned(coordinate)))
                    case Right(found) if found == launcher =>
                      ZIO.unit
                    case Right(found) =>
                      ZIO.fail(LoadError.Plugin(PluginLoadError.VersionMismatch(dependency, found, launcher)))
end PluginShips
