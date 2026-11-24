package histos.process

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.util.concurrent.TimeUnit

import zio.{Chunk, Scope, ZIO, durationInt}
import zio.test.*

object ScopedIOSpec extends ZIOSpecDefault:
  private enum ProbeError:
    case Java, Start, IO, Deadline

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

  private def child(mode: String): ZIO[Scope, ProbeError, Process] =
    for
      java <- ZIO
        .systemWith(_.property("java.home"))
        .mapError(_ => ProbeError.Java)
        .flatMap(home => ZIO.fromOption(home.filter(_.nonEmpty)).orElseFail(ProbeError.Java))
        .map(home => Path.of(home, "bin", "java").toString)
      process <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(
            ProcessBuilder(
              Path.of(java).toString,
              "--enable-native-access=ALL-UNNAMED",
              "-cp",
              javaClassPath,
              "histos.process.OwnedInputFixture",
              mode,
            ).start()
          )
          .mapError(_ => ProbeError.Start)
      )(process =>
        ZIO.attemptBlocking {
          process.destroyForcibly()
          process.waitFor()
          process.getOutputStream.close()
          process.getInputStream.close()
          process.getErrorStream.close()
        }.ignore
      )
    yield process

  private def javaClassPath: String = java.lang.System.getProperty("java.class.path")

  private def probe(mode: String, inputEnd: InputEnd) = ZIO.scoped:
    for
      process <- child(mode)
      _       <- ZIO
        .attemptBlockingCancelable {
          process.getOutputStream.write(payload.toArray)
          process.getOutputStream.flush()
          inputEnd match
            case InputEnd.Eof  => process.getOutputStream.close()
            case InputEnd.Open => ()
        }(ZIO.succeed(process.destroyForcibly()))
        .mapError(_ => ProbeError.IO)
        .timeoutFail(ProbeError.Deadline)(5.seconds)
      done   <- ZIO.attemptBlocking(process.waitFor(5, TimeUnit.SECONDS)).mapError(_ => ProbeError.IO)
      result <-
        if done then
          ZIO
            .attemptBlocking {
              val out = Chunk.fromArray(process.getInputStream.readAllBytes())
              val err = new String(process.getErrorStream.readAllBytes(), UTF_8)
              assertTrue(
                process.exitValue() == 0,
                !process.isAlive,
                out == payload,
                err.contains("owned input closed"),
              )
            }
            .mapError(_ => ProbeError.IO)
        else ZIO.succeed(assertTrue(done))
    yield result
end ScopedIOSpec
