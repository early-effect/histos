package histos.publish

import histos.version.ShipNumber
import histos.workspace.Workspace

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.time.{Duration as JavaDuration, Instant}

import zio.{Duration, UIO, ZIO}
import zio.durationInt

/** A release pin does not move. Outside CI, one cached check a day names a newer published release.
  */
object UpgradeNotice:
  val Window: Duration = 1.day

  val Metadata: String =
    "https://repo1.maven.org/maven2/rocks/earlyeffect/histos/maven-metadata.xml"

  final case class Record(at: Instant, release: Option[ShipNumber])

  enum Step:
    case Quiet
    case Remembered
    case Fetch

  def inCi(env: Map[String, String]): Boolean =
    truthy(env.get("CI")) || env.get("GITHUB_ACTIONS").exists(_.trim.equalsIgnoreCase("true"))

  def step(env: Map[String, String], now: Instant, cached: Option[Record]): Step =
    if inCi(env) then Step.Quiet
    else
      cached match
        case Some(record) if !stale(now, record.at) => Step.Remembered
        case _                                      => Step.Fetch

  def text(pin: ShipNumber, published: ShipNumber): String =
    if newer(published, pin) then
      s"histos ${published.render} is a newer release than ${pin.render}. A release pin does not move. Run `histos launcher --latest`."
    else ""

  def newer(published: ShipNumber, pin: ShipNumber): Boolean =
    val publishedRank = (published.major, published.minor, published.patch)
    val pinRank       = (pin.major, pin.minor, pin.patch)
    publishedRank._1 > pinRank._1 ||
    (publishedRank._1 == pinRank._1 && publishedRank._2 > pinRank._2) ||
    (publishedRank._1 == pinRank._1 && publishedRank._2 == pinRank._2 && publishedRank._3 > pinRank._3)

  def newest(xml: String): Option[ShipNumber] =
    val found = Version.findAllMatchIn(xml).flatMap(hit => ShipNumber.from(hit.group(1)).toOption).toList
    found.sortBy(number => (number.major, number.minor, number.patch)).lastOption

  def ship(pin: LauncherPin): Option[ShipNumber] =
    ShipNumber.from(pin.channel.stripSuffix("-SNAPSHOT")).toOption

  def cache(env: Map[String, String], userHome: String): Path =
    val root = env.get("HISTOS_HOME").map(_.trim).filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".histos")
    root.resolve("upgrade")

  def parse(text: String): Option[Record] =
    val lines   = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    val at      = lines.collectFirst { case s"at=$raw" => raw.toLongOption.map(Instant.ofEpochMilli) }.flatten
    val release = lines.collectFirst:
      case "release=-"     => Some(None)
      case s"release=$raw" => ShipNumber.from(raw).toOption.map(Some(_))
    .flatten
    at.zip(release).map(Record.apply)

  def render(record: Record): String =
    val release = record.release.fold("-")(_.render)
    s"at=${record.at.toEpochMilli}\nrelease=$release\n"

  /** Empty when there is nothing to say. A failed read or a failed fetch stays quiet. */
  def line(repo: Path, body: UIO[Option[String]] = fetch): UIO[String] =
    zio.System.envs.orElseSucceed(Map.empty).flatMap: env =>
      if inCi(env) then ZIO.succeed("")
      else
        LauncherPin.read(repo).map(pin => ship(pin)).orElseSucceed(None).flatMap:
          case None      => ZIO.succeed("")
          case Some(pin) =>
            zio.System.property("user.home").orElseSucceed(None).flatMap: userHome =>
              val path = cache(env, userHome.getOrElse(""))
              Workspace.readIfPresent(path).orElseSucceed(None).flatMap: cached =>
                val record = cached.flatMap(parse)
                zio.Clock.instant.flatMap: now =>
                  step(env, now, record) match
                    case Step.Quiet      => ZIO.succeed("")
                    case Step.Remembered =>
                      ZIO.succeed(record.flatMap(_.release).fold("")(published => text(pin, published)))
                    case Step.Fetch =>
                      body.flatMap: xml =>
                        val release = xml.flatMap(newest)
                        val written = Record(now, release)
                        Workspace
                          .writeString(path, render(written))
                          .ignore
                          .as(release.fold("")(published => text(pin, published)))

  private def stale(now: Instant, at: Instant): Boolean =
    val age = JavaDuration.between(at, now)
    age.isNegative || !age.minus(JavaDuration.ofMillis(Window.toMillis)).isNegative

  private def truthy(raw: Option[String]): Boolean =
    raw.map(_.trim.toLowerCase) match
      case Some("1" | "true" | "yes") => true
      case _                          => false

  private val Version = "<version>([^<]+)</version>".r

  private def fetch: UIO[Option[String]] =
    ZIO
      .attemptBlocking:
        val client = HttpClient.newBuilder().connectTimeout(JavaDuration.ofSeconds(2)).build()
        val request =
          HttpRequest.newBuilder(URI.create(Metadata)).timeout(JavaDuration.ofSeconds(3)).GET().build()
        val response = client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
        if response.statusCode() == 200 then Some(response.body()) else None
      .orElseSucceed(None)
end UpgradeNotice
