package histos.schedule

import heddle.client.Client
import heddle.http.{Request, Url}
import histos.process.{ChildError, ChildEvent, ChildOutcome, ChildProcess, ProcessExit, ProcessOutput}
import histos.testrun.Transcript
import histos.version.PullRequestNumber
import histos.workspace.Workspace

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, Schedule, Scope, System, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

/** The real Bazel HTTP server, when a docker daemon is present. The pin is the multi-arch index of
  * `buchgr/bazel-remote-cache`, so a tag cannot move and each platform pulls its own manifest.
  */
object BazelRemoteSpec extends histos.SuiteClock:
  private val image =
    "buchgr/bazel-remote-cache@sha256:10285c1183e606679eb450b725c2495c055a45eb532a6c80f6b6f9771f6d2f67"

  private val hostKeys = Set(
    "PATH",
    "HOME",
    "USER",
    "LOGNAME",
    "TMPDIR",
    "DOCKER_HOST",
    "DOCKER_CONTEXT",
    "DOCKER_CONFIG",
    "DOCKER_CERT_PATH",
    "DOCKER_TLS_VERIFY",
  )

  private val absent =
    test("a non-zero docker status is an absent daemon"):
      assertTrue(
        !daemonUp(ProcessExit(1)),
        !daemonUp(ProcessExit(127)),
        daemonUp(ProcessExit(0)),
      )

  /** `bazel-remote` keeps one action cache unless `--enable_ac_key_instance_mangling` is set, so without that flag a
    * write to `pr/1` is visible to `main`.
    */
  private val namespaces =
    test("a pull request write misses on main, and a main write hits"):
      val bytes  = Chunk.fromArray("namespace".getBytes(StandardCharsets.UTF_8))
      val action = Digest.ofBytes(bytes)
      val pull   = CacheNamespace.PullRequest(PullRequestNumber("1"))
      for
        env       <- hostEnv
        work      <- Workspace.work("histos-bazel-remote").mapError(error => StandFailure.Work(error.message))
        bin       <- binary(env)
        running   <- container(bin, env, work)
        pullCache <- HttpCache.make(running.origin, RemoteMode.Write, pull)
        _         <- pullCache.store(action, bytes).catchAll(error => failed(bin, env, work, running.name, error))
        mainRead  <- HttpCache.make(running.origin, RemoteMode.Read, CacheNamespace.Main)
        missed    <- mainRead.find(action).catchAll(error => failed(bin, env, work, running.name, error))
        mainWrite <- HttpCache.make(running.origin, RemoteMode.Write, CacheNamespace.Main)
        _         <- mainWrite.store(action, bytes).catchAll(error => failed(bin, env, work, running.name, error))
        hit       <- mainRead.find(action).catchAll(error => failed(bin, env, work, running.name, error))
      yield assertTrue(missed.isEmpty, hit == Some(bytes))
    .provideSomeLayer[zio.test.TestEnvironment & Scope](
      ZLayer.succeed(Client.Config.default) >>> Client.layer
    )

  private def daemonUp(exit: ProcessExit): Boolean =
    exit.value == 0

  /** Non-zero `docker info`, or no `docker` binary, is an absent daemon. Any other failure fails the test. */
  private def dockerUp: ZIO[Scope, StandFailure, Boolean] =
    for
      env   <- hostEnv
      work  <- Workspace.work("histos-docker-probe").mapError(error => StandFailure.Work(error.message))
      found <- binary(env).foldZIO(
        {
          case StandFailure.Child(_) => ZIO.succeed(None)
          case other                 => ZIO.fail(other)
        },
        path => ZIO.succeed(Some(path)),
      )
      up <- found match
        case None      => ZIO.succeed(false)
        case Some(bin) =>
          command(List(bin.toString, "info"), work, env).foldZIO(
            {
              case ChildError.Start(_) | ChildError.NoProgram => ZIO.succeed(false)
              case other                                      => ZIO.fail(StandFailure.Child(other.message))
            },
            ran => ZIO.succeed(daemonUp(ran.exit)),
          )
    yield up

  private def container(bin: Path, env: Map[String, String], work: Path): ZIO[Scope & Client, StandFailure, Running] =
    for
      name <- ZIO.attempt(java.util.UUID.randomUUID.toString).orDie.map(id => s"histos-br-$id")
      _    <- ZIO.addFinalizer(remove(bin, env, work, name))
      _    <- started(bin, env, work, name)
      url  <- published(bin, env, work, name)
        .retry(Schedule.spaced(100.millis) && Schedule.recurs(30))
        .catchAll(error => blame(bin, env, work, name, error))
      _ <- ready(url).catchAll(error => failed(bin, env, work, name, error))
    yield Running(name, url)

  /** The suite scope closes after `withLiveSystem` is gone, and `ChildProcess` reads `java.home` from `System`. */
  private def remove(bin: Path, env: Map[String, String], work: Path, name: String): UIO[Unit] =
    val argv = List(bin.toString, "rm", "-f", name)
    ZIO
      .withSystem(zio.System.SystemLive)(command(argv, work, env))
      .flatMap: ran =>
        if ran.exit.value == 0 then ZIO.unit
        else ZIO.dieMessage(s"${argv.mkString(" ")} exited ${ran.exit.value}\n${ran.err.text}${ran.out.text}")
      .catchAll(error => ZIO.dieMessage(error.message))

  private def started(bin: Path, env: Map[String, String], work: Path, name: String): ZIO[Any, StandFailure, Unit] =
    val argv = List(
      bin.toString,
      "run",
      "-d",
      "--name",
      name,
      "-p",
      "127.0.0.1::8080",
      image,
      "--max_size",
      "1",
      "--enable_ac_key_instance_mangling",
    )
    command(argv, work, env)
      .mapError(error => StandFailure.Child(error.message))
      .flatMap: ran =>
        if ran.exit.value == 0 then ZIO.unit
        else ZIO.fail(StandFailure.Command(argv, ran.exit.value, ran.out.text, ran.err.text))

  private def published(bin: Path, env: Map[String, String], work: Path, name: String): ZIO[Any, StandFailure, Url] =
    val argv = List(bin.toString, "port", name, "8080/tcp")
    command(argv, work, env)
      .mapError(error => StandFailure.Child(error.message))
      .flatMap: ran =>
        if ran.exit.value != 0 then ZIO.fail(StandFailure.Command(argv, ran.exit.value, ran.out.text, ran.err.text))
        else
          hostPort(ran.out.text) match
            case Some(url) => ZIO.succeed(url)
            case None      => ZIO.fail(StandFailure.Port(ran.out.text))

  private def ready(origin: Url): ZIO[Client, RemoteError, Unit] =
    val once =
      ZIO
        .serviceWithZIO[Client](_.batched(Request.get(origin / "status")))
        .mapError: error =>
          RemoteError.Exchange(HttpCache.exchange(error))
    once
      .flatMap: response =>
        response.status.code match
          case 200  => ZIO.unit
          case code => ZIO.fail(RemoteError.Status(code))
      .retry(Schedule.spaced(200.millis) && Schedule.recurs(50))

  private def failed(
      bin: Path,
      env: Map[String, String],
      work: Path,
      name: String,
      error: RemoteError,
  ): ZIO[Any, StandFailure, Nothing] =
    blame(bin, env, work, name, StandFailure.Cache(error.message))

  private def blame(
      bin: Path,
      env: Map[String, String],
      work: Path,
      name: String,
      error: StandFailure,
  ): ZIO[Any, StandFailure, Nothing] =
    command(List(bin.toString, "logs", name), work, env)
      .orElseSucceed(ChildOutcome(ProcessExit(0), ProcessOutput.from(""), ProcessOutput.from("")))
      .flatMap: outcome =>
        ZIO.fail(StandFailure.Logs(render(error), outcome.out.text + outcome.err.text))

  private def render(error: StandFailure): String = error match
    case StandFailure.Command(argv, exit, out, err) =>
      s"${argv.mkString(" ")} exited $exit\n$err$out"
    case StandFailure.Child(message)   => message
    case StandFailure.Port(text)       => s"docker port did not name a host port: $text"
    case StandFailure.Cache(message)   => message
    case StandFailure.Work(message)    => message
    case StandFailure.Logs(message, _) => message

  private def hostPort(text: String): Option[Url] =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).map(parseHost).collectFirst { case Some(url) => url }

  private def parseHost(line: String): Option[Url] =
    line.split(":", 2) match
      case Array(host, port) if host.nonEmpty && port.nonEmpty && port.forall(_.isDigit) =>
        Some(Url.parse(s"http://$host:$port"))
      case _ => None

  private def binary(env: Map[String, String]): ZIO[Any, StandFailure, Path] =
    ZIO
      .attemptBlocking(look(env))
      .mapError(_ => StandFailure.Work("could not search PATH for docker"))
      .flatMap:
        case Some(path) => ZIO.succeed(path)
        case None       => ZIO.fail(StandFailure.Child(ChildError.Start("docker").message))

  private def look(env: Map[String, String]): Option[Path] =
    env
      .get("PATH")
      .flatMap: raw =>
        raw
          .split(File.pathSeparator)
          .iterator
          .map(dir => Path.of(dir).resolve("docker"))
          .find(path => Files.isRegularFile(path) && Files.isExecutable(path))

  private def hostEnv: UIO[Map[String, String]] =
    System.envs.orElseSucceed(Map.empty[String, String]).map(_.filter((key, _) => hostKeys.contains(key)))

  private def command(
      argv: List[String],
      work: Path,
      env: Map[String, String],
  ): ZIO[Any, ChildError, ChildOutcome] =
    ChildProcess
      .command(argv, work, env)
      .runFold((Transcript.empty, Option.empty[ProcessExit])):
        case ((heard, exit), ChildEvent.Out(text)) => (heard.withOut(text), exit)
        case ((heard, exit), ChildEvent.Err(text)) => (heard.withErr(text), exit)
        case ((heard, _), ChildEvent.Exited(code)) => (heard, Some(code))
      .flatMap:
        case (heard, Some(code)) => ZIO.succeed(ChildOutcome(code, heard.outText, heard.errText))
        case _                   => ZIO.fail(ChildError.Status)

  private final case class Running(name: String, origin: Url)

  private enum StandFailure:
    case Command(argv: List[String], exit: Int, out: String, err: String)
    case Child(message: String)
    case Port(text: String)
    case Cache(message: String)
    case Work(message: String)
    case Logs(message: String, logs: String)

  def spec = suite("bazel-remote")(
    absent,
    namespaces.whenZIO(dockerUp),
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
end BazelRemoteSpec
