package histos.publish

import histos.central.{
  Artifact,
  ArtifactId,
  DependencyVersion,
  MavenDependency,
  PortalBase,
  Preflight,
  PublishError,
  Published,
  Publisher,
  ReleaseMode as CentralRelease,
  RepositorySecret,
  Revision as CentralRevision,
  SnapshotRepository,
}
import histos.jdk.Sha256
import histos.version.Revision as ShipRevision
import histos.{
  FollowUp,
  PublishFault,
  PublishReceipt,
  PublishRequest,
  Sona,
  SonaPublish,
  StagedDependency,
  StagedModule,
}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

object SonaStock:
  def layer(env: Map[String, String]): ULayer[SonaPublish] =
    ZLayer.succeed(Live(env))

  /** Credentials, then the signing key, then the portal base. No socket. */
  def gate(env: Map[String, String]): IO[ReleaseError, Unit] =
    prepared(env).unit

  private def prepared(env: Map[String, String]): IO[ReleaseError, Ready] =
    for
      secret      <- ZIO.fromEither(credentials(env))
      signer      <- Signer.fromEnv(env).mapError(ReleaseError.Signer(_))
      fingerprint <- signer.fingerprint.mapError(ReleaseError.Signer(_))
      base        <- ZIO.fromEither(Preflight.portal(env).left.map(ReleaseError.Portal(_)))
    yield Ready(secret, signer, fingerprint, base)

  private def credentials(env: Map[String, String]): Either[ReleaseError, RepositorySecret] =
    PublishWhere.snapshots(env) match
      case Right(PublishWhere.Snapshots(user, password)) => Right(RepositorySecret(user, password))
      case Left(_)                                       => Left(ReleaseError.Credentials)

  private def central(revision: ShipRevision): CentralRevision =
    revision match
      case ShipRevision.Ci(version)          => CentralRevision.Ci(version)
      case ShipRevision.Pointer(pointer)     => CentralRevision.Pointer(pointer)
      case ShipRevision.Snapshot(coordinate) => CentralRevision.Snapshot(coordinate)
      case ShipRevision.Release(version)     => CentralRevision.Release(version)

  private def mode(automatic: Boolean): CentralRelease =
    if automatic then CentralRelease.Automatic else CentralRelease.Hold

  private final class Live(env: Map[String, String]) extends SonaPublish:
    def publish(request: PublishRequest): IO[PublishFault, PublishReceipt] =
      request.revision match
        case ShipRevision.Ci(_) =>
          ZIO.fail(PublishFault.Version(request.revision))
        case ShipRevision.Pointer(_) | ShipRevision.Snapshot(_) =>
          snapshots(request)
        case ShipRevision.Release(_) =>
          release(request)

    def secretNames: List[String] = Sona.secrets

    def followUp: FollowUp = FollowUp.Finished

    private def snapshots(request: PublishRequest): IO[PublishFault, PublishReceipt] =
      ZIO
        .fromEither(credentials(env).left.map(releaseFault))
        .flatMap: secret =>
          val revision = central(request.revision)
          ZIO
            .foreach(request.modules)(module => put(request, revision, module))
            .provide(Publisher.snapshots(SnapshotRepository.central, secret).mapError(centralFault(request, _)))
            .map(lines => PublishReceipt(lines, Chunk.empty))

    private def put(
        request: PublishRequest,
        revision: CentralRevision,
        module: StagedModule,
    ): ZIO[Publisher, PublishFault, String] =
      for
        jar <- read(module.jar)
        // The snapshot publisher writes the pom from the artifact. The read only checks the staged file is there.
        _       <- read(module.pom)
        sources <- ZIO.foreach(module.sources)(read)
        javadoc <- ZIO.foreach(module.javadoc)(read)
        deps    <- ZIO.fromEither(dependencies(module.dependencies))
        name <- ZIO.fromEither(ArtifactId.from(module.artifact).left.map(_ => PublishFault.Artifact(module.artifact)))
        artifact = Artifact(module.group, name, revision, jar, deps, sources = sources, javadoc = javadoc)
        done <- ZIO
          .serviceWithZIO[Publisher](_.publish(artifact))
          .mapError(centralFault(request, _))
        unique <- done match
          case Published.Snapshot(unique, _) => ZIO.succeed(unique.value)
          case Published.Local(_, _, _, _, _) => ZIO.fail(PublishFault.Remote)
      yield PublishedModule(
        module.module,
        module.group,
        module.artifact,
        module.revision,
        Some(module.jar),
        Some(module.pom),
        module.sources,
        module.javadoc,
        module.ship,
        Sha256.of(jar),
        Some(unique),
        deps,
      ).rendered(None)

    private def release(request: PublishRequest): IO[PublishFault, PublishReceipt] =
      prepared(env)
        .mapError(releaseFault)
        .flatMap: ready =>
          modules(request.modules).flatMap: published =>
            val staged = ReleaseStage(
              request.stage,
              published,
              Nil,
              Nil,
              Nil,
              ready.signer,
              ready.base,
              ready.secret,
              ready.fingerprint,
              env,
            )
            ReleaseRun
              .upload(request.checkout, staged, mode(request.automatic))
              .mapError(releaseFault)
              .map: report =>
                PublishReceipt(
                  Chunk.fromIterable(report.refused ++ report.held ++ report.deployed.map(_.line)),
                  Chunk.fromIterable(report.failed),
                )

    private def modules(listed: Chunk[StagedModule]): IO[PublishFault, List[PublishedModule]] =
      ZIO.foreach(listed.toList): module =>
        for
          jar  <- read(module.jar)
          deps <- ZIO.fromEither(dependencies(module.dependencies))
        yield PublishedModule(
          module.module,
          module.group,
          module.artifact,
          module.revision,
          Some(module.jar),
          Some(module.pom),
          module.sources,
          module.javadoc,
          module.ship,
          Sha256.of(jar),
          None,
          deps,
        )
  end Live

  private def dependencies(listed: Chunk[StagedDependency]): Either[PublishFault, List[MavenDependency]] =
    listed.toList.foldLeft[Either[PublishFault, List[MavenDependency]]](Right(Nil)): (acc, item) =>
      acc.flatMap(seen => dependency(item).map(seen :+ _))

  private def dependency(item: StagedDependency): Either[PublishFault, MavenDependency] =
    for
      name    <- ArtifactId.from(item.artifact).left.map(_ => PublishFault.Artifact(item.artifact))
      version <- DependencyVersion.from(item.revision).left.map(_ => PublishFault.Dependency(item.revision))
    yield MavenDependency(item.group, name, version)

  private def read(path: Path): IO[PublishFault, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => PublishFault.Write(path.toString))

  private def centralFault(request: PublishRequest, error: PublishError): PublishFault =
    error match
      case PublishError.EmptyJar            => PublishFault.Remote
      case PublishError.NotAJar             => PublishFault.Remote
      case PublishError.NotADirectory(path) => PublishFault.Write(path.toString)
      case PublishError.Escaped(path)       => PublishFault.Write(path.toString)
      case PublishError.Io(path, _)         => PublishFault.Write(path.toString)
      case PublishError.Unauthorized        => PublishFault.Credentials
      case PublishError.NotSnapshot(_)      => PublishFault.Version(request.revision)
      case PublishError.Status(_)           => PublishFault.Remote
      case PublishError.Transport(_)        => PublishFault.Remote
      case PublishError.Metadata(_)         => PublishFault.Remote

  private def releaseFault(error: ReleaseError): PublishFault =
    error match
      case ReleaseError.Credentials  => PublishFault.Credentials
      case ReleaseError.Signer(_)    => PublishFault.Signer
      case ReleaseError.Portal(_)    => PublishFault.Portal
      case ReleaseError.Bundle(_)    => PublishFault.Bundle
      case ReleaseError.Client(_)    => PublishFault.Client
      case ReleaseError.Io(path)     => PublishFault.Write(path)
      case ReleaseError.Record(ship) => PublishFault.Record(ship)
      case ReleaseError.Stage(_)     => PublishFault.Remote
      case ReleaseError.Asset(_)     => PublishFault.Remote

  private final case class Ready(
      secret: RepositorySecret,
      signer: Signer,
      fingerprint: histos.central.Fingerprint,
      base: PortalBase,
  )
end SonaStock
