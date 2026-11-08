package histos.central

import histos.Organization
import histos.version.ShipNumber

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration as JavaDuration

import zio.{durationInt, IO, ZIO, ZLayer}

/** One Maven artifact whose releases a registry lists. */
final case class MavenModule(group: Organization, artifact: ArtifactId):
  def metadata: String =
    s"${ReleaseFeed.Central}/${group.segments.mkString("/")}/${artifact.value}/maven-metadata.xml"

enum FeedError:
  case Unreachable(url: String)
  case Status(url: String, code: Int)

  def message: String = this match
    case Unreachable(url)  => s"$url did not answer"
    case Status(url, code) => s"$url answered $code"
end FeedError

/** The releases a registry lists for one artifact. Snapshots and other versions are not releases. */
trait ReleaseFeed:
  /** The newest release. `None` when the artifact is not published or lists no release. */
  def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]]

object ReleaseFeed:
  val Central: String = "https://repo1.maven.org/maven2"

  /** The Histos launcher a `histos.version` pin selects. */
  val histos: MavenModule = MavenModule(Organization("rocks.earlyeffect"), ArtifactId.trusted("histos"))

  /** Reads Maven Central. A slow answer is unreachable well inside a doctor check's limit. */
  val live: ZLayer[Any, Nothing, ReleaseFeed] =
    ZLayer.scoped:
      ZIO
        .acquireRelease(
          ZIO.succeed(HttpClient.newBuilder().connectTimeout(JavaDuration.ofSeconds(2)).build())
        )(client => ZIO.succeed(client.close()))
        .map(Live(_))

  def newest(xml: String): Option[ShipNumber] =
    Version
      .findAllMatchIn(xml)
      .flatMap(hit => ShipNumber.from(hit.group(1)).toOption)
      .maxOption

  private val Version = "<version>([^<]+)</version>".r

  private final class Live(client: HttpClient) extends ReleaseFeed:
    def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]] =
      val url = module.metadata
      ZIO
        .attemptBlockingInterrupt:
          val request = HttpRequest.newBuilder(URI.create(url)).timeout(JavaDuration.ofSeconds(3)).GET().build()
          client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
        .orElseFail(FeedError.Unreachable(url))
        .timeoutFail(FeedError.Unreachable(url))(5.seconds)
        .flatMap: response =>
          response.statusCode() match
            case 200  => ZIO.succeed(ReleaseFeed.newest(response.body()))
            case 404  => ZIO.none
            case code => ZIO.fail(FeedError.Status(url, code))
end ReleaseFeed
