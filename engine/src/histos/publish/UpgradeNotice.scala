package histos.publish

import histos.central.ReleaseFeed
import histos.version.{PinLag, ShipNumber}
import histos.workspace.Workspace

import java.nio.file.Path
import java.time.{Duration as JavaDuration, Instant}

import zio.{Duration, URIO, ZIO}
import zio.durationInt

/** Outside CI, one cached read a day of the newest published release. A pin that release passed gets a [[PinLag]].
  */
object UpgradeNotice:
  val Window: Duration = 1.day

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

  def cache(env: Map[String, String], userHome: String): Path =
    val root = env.get("HISTOS_HOME").map(_.trim).filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".histos")
    root.resolve("upgrade")

  def parse(text: String): Option[Record] =
    val lines   = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    val at      = lines.collectFirst { case s"at=$raw" => raw.toLongOption.map(Instant.ofEpochMilli) }.flatten
    val release = lines
      .collectFirst:
        case "release=-"     => Some(None)
        case s"release=$raw" => ShipNumber.from(raw).toOption.map(Some(_))
      .flatten
    at.zip(release).map(Record.apply)

  def render(record: Record): String =
    val release = record.release.fold("-")(_.render)
    s"at=${record.at.toEpochMilli}\nrelease=$release\n"

  /** `None` when there is nothing to say. An unread pin and an unreachable registry stay quiet; a failed read is
    * remembered for the window like an answer.
    */
  def advice(repo: Path): URIO[ReleaseFeed, Option[PinLag]] =
    zio.System.envs
      .orElseSucceed(Map.empty)
      .flatMap: env =>
        if inCi(env) then ZIO.none
        else
          LauncherPin
            .read(repo)
            .option
            .flatMap:
              case None      => ZIO.none
              case Some(pin) =>
                zio.System
                  .property("user.home")
                  .orElseSucceed(None)
                  .flatMap: userHome =>
                    val path = cache(env, userHome.getOrElse(""))
                    Workspace
                      .readIfPresent(path)
                      .orElseSucceed(None)
                      .flatMap: cached =>
                        val record = cached.flatMap(parse)
                        zio.Clock.instant.flatMap: now =>
                          step(env, now, record) match
                            case Step.Quiet      => ZIO.none
                            case Step.Remembered => ZIO.succeed(record.flatMap(_.release).flatMap(pin.choice.lag))
                            case Step.Fetch      =>
                              ZIO
                                .serviceWithZIO[ReleaseFeed](_.newest(ReleaseFeed.histos))
                                .orElseSucceed(None)
                                .flatMap: release =>
                                  Workspace
                                    .writeString(path, render(Record(now, release)))
                                    .ignore
                                    .as(release.flatMap(pin.choice.lag))

  private def stale(now: Instant, at: Instant): Boolean =
    val age = JavaDuration.between(at, now)
    age.isNegative || !age.minus(JavaDuration.ofMillis(Window.toMillis)).isNegative

  private def truthy(raw: Option[String]): Boolean =
    raw.map(_.trim.toLowerCase) match
      case Some("1" | "true" | "yes") => true
      case _                          => false
end UpgradeNotice
