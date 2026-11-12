package ci

import java.nio.file.{Files, Path}

import histos.plugin.*

import zio.{Scope, ZIO}

/** This build's client link. The generator does not know the repository. The task links `dist/histos-client` when the
  * pin has no client for this platform. A snapshot from one machine does not ship the other platform. Linking that
  * missing client does not replace the pin: the launcher execs this binary only when the jar is `dist/histos.jar`. When
  * the pin already has the client, linking it again is the waste, and the task does nothing.
  */
object LinkClient extends HistosPlugin:
  val id: PluginId = PluginId("histos.link")

  def contribute: Contribution =
    Contribution(
      List(DeclaredTask(TaskName("link"), Seal.Open, Set.empty, Nil, Nil, Nil)),
      Nil,
      List(TaskJob(JobName("link-client"), TaskName("link"))),
    )

  def decide(client: Installed): Decision =
    client match
      case Installed.Missing => Decision.Link
      case Installed.Present => Decision.Skip

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    installed(request.repo).flatMap: found =>
      decide(found) match
        case Decision.Skip => ZIO.succeed(PluginOutput(Decision.Skip.text, Nil))
        case Decision.Link => link(request.repo)

  private def installed(repo: Path): ZIO[Any, PluginRunError, Installed] =
    ZIO
      .attemptBlocking(locate(repo))
      .mapError(_ => PluginRunError.Failed(PluginDetail("could not read the pin's client")))
      .flatMap:
        case Left(detail) => ZIO.fail(PluginRunError.Failed(detail))
        case Right(found) => ZIO.succeed(found)

  private def locate(repo: Path): Either[PluginDetail, Installed] =
    home.flatMap: root =>
      pin(repo).map: version =>
        val client = root.resolve("toolchains").resolve(version).resolve(ClientFile)
        if Files.isRegularFile(client) && Files.isExecutable(client) then Installed.Present
        else Installed.Missing

  private def home: Either[PluginDetail, Path] =
    sys.env.get("HISTOS_HOME").map(_.trim).filter(_.nonEmpty) match
      case Some(dir) => Right(Path.of(dir))
      case None      =>
        Option(System.getProperty("user.home")).map(_.trim).filter(_.nonEmpty) match
          case Some(dir) => Right(Path.of(dir).resolve(".histos"))
          case None      => Left(PluginDetail("could not read the pin's client"))

  private def pin(repo: Path): Either[PluginDetail, String] =
    val file = repo.resolve("build.properties")
    if !Files.isRegularFile(file) then Left(PluginDetail("could not read histos.version"))
    else
      val version = Files
        .readString(file)
        .linesIterator
        .map(_.trim)
        .collectFirst:
          case line if line.startsWith("histos.version=") => line.stripPrefix("histos.version=").trim
      version.filter(_.nonEmpty).toRight(PluginDetail("could not read histos.version"))

  private def link(repo: Path): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking(
          new java.lang.ProcessBuilder("./histos", "client").directory(repo.toFile).inheritIO().start()
        )
        .mapError(_ => PluginRunError.Failed(PluginDetail("could not start ./histos client")))
    )(process => ZIO.attemptBlocking(process.destroy()).ignore): process =>
      ZIO
        .attemptBlockingInterrupt(process.waitFor())
        .mapError(_ => linkFailed)
        .flatMap:
          case 0 => ZIO.succeed(PluginOutput(Decision.Link.text, Nil))
          case _ => ZIO.fail(linkFailed)

  private val linkFailed: PluginRunError = PluginRunError.Failed(PluginDetail("client link failed"))

  private val ClientFile: String = "histos-client"

  enum Installed:
    case Missing
    case Present

  enum Decision:
    case Skip
    case Link

    def text: String = this match
      case Skip => "the pin already has a client for this platform."
      case Link => "linked dist/histos-client"
  end Decision
end LinkClient
