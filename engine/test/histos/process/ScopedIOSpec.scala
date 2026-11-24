package histos.process

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import zio.{Chunk, Queue, Scope, ZIO, durationInt}
import zio.test.*

object ScopedIOSpec extends ZIOSpecDefault:
  private enum ProbeError:
    case Java, Deadline
    case Child(message: String)

  private enum InputEnd:
    case Open, Eof

  private val payload = Chunk.fromArray(" {\"text\":\"π\"} \r\n".getBytes(UTF_8)) ++
    Chunk[Byte](0, 0xff.toByte, 0x80.toByte, 'x'.toByte)

  private val tests = suite("owned scoped IO")(
    test("orderly close wakes both readers with stdin still open") {
      probe("close", InputEnd.Open)
    },
    test("interruption joins both readers with stdin still open") {
      probe("interrupt", InputEnd.Open)
    },
    test("EOF preserves every forwarded byte and releases the signal reader") {
      probe("eof", InputEnd.Eof)
    },
    test("repeated idle scopes release their descriptors") {
      probe("repeat", InputEnd.Open)
    },
    test("close restores the previous signal hook") {
      probe("signal", InputEnd.Open)
    },
  )

  private val verified = tests @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.sequential

  def spec = verified

  private def probe(mode: String, inputEnd: InputEnd) = ZIO.scoped:
    for
      java <- ZIO
        .systemWith(_.property("java.home"))
        .mapError(_ => ProbeError.Java)
        .flatMap(home => ZIO.fromOption(home.filter(_.nonEmpty)).orElseFail(ProbeError.Java))
        .map(home => Path.of(home, "bin", "java").toString)
      classpath <- ZIO
        .systemWith(_.property("java.class.path"))
        .mapError(_ => ProbeError.Java)
        .flatMap(path => ZIO.fromOption(path.filter(_.nonEmpty)).orElseFail(ProbeError.Java))
      env   <- ZIO.systemWith(_.envs).mapError(error => ProbeError.Child(error.getMessage))
      work  <- directory
      pipe  <- relay(inputEnd)
      heard <- ChildProcess
        .command(
          List(
            java,
            "--enable-native-access=ALL-UNNAMED",
            "-cp",
            classpath,
            "histos.process.OwnedInputFixture",
            mode,
          ),
          work,
          env,
          pipe,
          ChildProcess.Stdout.Bytes,
        )
        .runCollect
        .mapError(error => ProbeError.Child(error.message))
        .timeoutFail(ProbeError.Deadline)(15.seconds)
      out  = stdout(heard)
      text = heard.collect { case ChildEvent.Err(line) => line }.mkString("\n")
      code = heard.collect { case ChildEvent.Exited(exit) => exit.value }.toList
    yield assertTrue(code == List(0), out == payload, text.contains("owned input closed"))

  private def relay(inputEnd: InputEnd): ZIO[Scope, Nothing, RunRelay] =
    for
      queue <- ZIO.acquireRelease(Queue.unbounded[Option[RelayCommand]])(_.shutdown)
      _     <- queue.offer(Some(RelayCommand.Stdin(payload)))
      _     <- inputEnd match
        case InputEnd.Eof  => queue.offer(Some(RelayCommand.End))
        case InputEnd.Open => ZIO.unit
      _ <- queue.offer(None)
    yield RunRelay.Open(queue.take)

  private def directory: ZIO[Scope, ProbeError, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "owned-"))
        .mapError(error => ProbeError.Child(error.getMessage))
    )(root =>
      ZIO.attemptBlocking {
        if Files.isDirectory(root) then
          val entries = Files.list(root)
          try entries.forEach(path => Files.deleteIfExists(path))
          finally entries.close()
        Files.deleteIfExists(root)
        ()
      }.ignore
    )

  private def stdout(events: Chunk[ChildEvent]): Chunk[Byte] =
    events.foldLeft(Chunk.empty[Byte]):
      case (all, ChildEvent.Bytes(data)) => all ++ data
      case (all, _)                      => all
end ScopedIOSpec
