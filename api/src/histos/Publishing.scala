package histos

import histos.version.{Revision, ShipName}

import java.nio.file.Path

import zio.{Chunk, IO, Tag, ZIO, ZLayer}

/** How a release publish finishes. The build chooses it. One run can still force automatic. */
enum ReleaseMode:
  case Automatic
  case Hold

/** What the release workflow does after `publish release`. Hold is a second workflow. Automatic is done. */
enum FollowUp:
  case Finished
  case Deployment

/** One publish. A command provides a layer of this service. A bigger publish is `flatMap`. */
trait Publishing:
  def publish(request: PublishRequest): IO[PublishFault, PublishReceipt]
  def secretNames: List[String]
  def followUp: FollowUp

/** The engine's local stock. The default `local` layer requires it. */
trait LocalPublish extends Publishing

/** The engine's Sonatype stock. `Sona.layer` closes the release branch over a mode. */
trait SonaPublish extends Publishing

/** One ship. `stage` is the unsigned Maven directory. `checkout` is the project. A release writes `.histos/deployments`
  * there.
  */
final case class PublishRequest(
    revision: Revision,
    automatic: Boolean,
    stage: Path,
    checkout: Path,
    modules: Chunk[StagedModule],
)

final case class StagedDependency(group: Organization, artifact: String, revision: String)

final case class StagedModule(
    module: ProjectName,
    ship: ShipName,
    group: Organization,
    artifact: String,
    revision: String,
    jar: Path,
    pom: Path,
    sources: Option[Path],
    javadoc: Option[Path],
    dependencies: Chunk[StagedDependency],
)

/** `failed` lines are ships that did not publish. The effect still succeeds so the others can. */
final case class PublishReceipt(lines: Chunk[String], failed: Chunk[String])

enum PublishFault:
  case Version(found: Revision)
  case Unreadable(raw: String)
  case Artifact(raw: String)
  case Dependency(raw: String)
  case Credentials
  case Signer
  case Portal
  case Bundle
  case Client
  case Write(path: String)
  case Record(ship: String)
  case Remote

  def message: String = this match
    case Version(found)  => s"${found.render} is not a version this publisher accepts"
    case Unreadable(raw) => s"'$raw' is not a revision"
    case Artifact(raw)   => s"'$raw' is not an artifact id"
    case Dependency(raw) => s"'$raw' is not a dependency version"
    case Credentials     => "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    case Signer          => "a release needs a signing key"
    case Portal          => "the portal URL is not usable"
    case Bundle          => "the bundle could not be packed"
    case Client          => "the portal rejected the upload"
    case Write(path)     => s"could not write $path"
    case Record(ship)    => s"$ship deployment record is not readable"
    case Remote          => "the repository rejected the publish"
end PublishFault

object Publishing:
  def fromEffect(
      run: PublishRequest => IO[PublishFault, PublishReceipt],
      secrets: List[String] = Nil,
      after: FollowUp = FollowUp.Finished,
  ): Publishing =
    new Publishing:
      def publish(request: PublishRequest): IO[PublishFault, PublishReceipt] = run(request)
      def secretNames: List[String]                                          = secrets
      def followUp: FollowUp                                                 = after

object Local:
  /** The engine supplies `LocalPublish`. An override that needs nothing ignores it. */
  val layer: ZLayer[LocalPublish, Nothing, Publishing] =
    ZLayer.service[LocalPublish]

object Sona:
  val secrets: List[String] = List(
    "SONATYPE_USERNAME",
    "SONATYPE_PASSWORD",
    "PGP_SECRET",
    "PGP_PASSPHRASE",
    "PGP_KEY_HEX",
  )

  def followUp(mode: ReleaseMode): FollowUp = mode match
    case ReleaseMode.Automatic => FollowUp.Finished
    case ReleaseMode.Hold      => FollowUp.Deployment

  /** The stock, with the release branch closed over `mode`. The snapshot branch ignores `mode`. */
  def layer(mode: ReleaseMode): ZLayer[SonaPublish, Nothing, Publishing] =
    ZLayer.fromFunction { (stock: SonaPublish) =>
      Publishing.fromEffect(
        request => stock.publish(request.copy(automatic = request.automatic || mode == ReleaseMode.Automatic)),
        secrets,
        followUp(mode),
      )
    }

  /** Refuses every revision. A workflow probe reads the layer around this stock, not the stock. */
  val idle: SonaPublish =
    new SonaPublish:
      def publish(request: PublishRequest): IO[PublishFault, PublishReceipt] =
        ZIO.fail(PublishFault.Version(request.revision))
      def secretNames: List[String] = secrets
      def followUp: FollowUp        = FollowUp.Finished

  /** Workflow and tests read this when they have no build. `publish` fails `Version`. */
  def described(mode: ReleaseMode): Publishing =
    Publishing.fromEffect(
      request => ZIO.fail(PublishFault.Version(request.revision)),
      secrets,
      followUp(mode),
    )
end Sona
