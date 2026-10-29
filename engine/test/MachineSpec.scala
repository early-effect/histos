package histos.jdk

import histos.cache.ContentStore
import histos.native.{NativeOrigin, NativeToolchain}
import histos.schedule.{Histos, Parallelism, Source, StepEvent}
import histos.step.{Need, Step, StepId, StepInfo}
import histos.toolchain.Architecture

import java.nio.file.Path
import java.time.Instant

import zio.{Chunk, Duration, IO, Promise, Ref, System, Trace, UIO, ZIO, ZLayer}
import zio.test.*

object MachineSpec extends ZIOSpecDefault:
  private val repo = Path.of("repo")

  private val token = Gen.string1(Gen.alphaNumericChar)

  private val missingOrEmpty: Gen[Any, Option[String]] =
    Gen.elements[Option[String]](None, Some(""))

  private val blankArch: Gen[Any, Option[String]] =
    Gen.elements[Option[String]](None, Some(""), Some(" "), Some("\t"), Some(" \t "))

  private val optionalText: Gen[Any, Option[String]] =
    Gen.oneOf(missingOrEmpty, token.map(Some(_)))

  private val host: Gen[Any, Host] =
    for
      osName <- Gen.string
      arch   <- token
      home   <- token
      jdk    <- token
      xdg    <- optionalText
      native <- optionalText
    yield Host(osName, arch, home, jdk, xdg, native)

  private val gap: Gen[Any, Gap] =
    Gen.oneOf(
      missingOrEmpty.zipWith(Gen.oneOf(blankArch, token.map(Some(_))))((home, arch) => Gap.NoHome(home, arch)),
      token.zipWith(blankArch)((home, arch) => Gap.NoArch(home, arch)),
    )

  private val opened =
    ZLayer.succeed(Parallelism(cpu = 1, io = 1, process = 1)) >>> Histos.live

  def spec =
    suite("machine")(
      test("one system read is the machine"):
        check(host): sample =>
          TestSystem.save.flatMap: restore =>
            (for
              _       <- assign(sample)
              inner   <- ZIO.system
              reads   <- Ref.make((0, 0))
              machine <- ZIO.withSystem(Counting(inner, reads))(Machine.load(repo))
              count   <- reads.get
              _       <- TestSystem.putProperty("os.arch", sample.arch + "x")
            yield assertTrue(
              count == (1, 1),
              machine.userHome == sample.home,
              machine.os == askedOs(sample.osName),
              machine.architecture == Architecture.fromMachine(sample.arch),
              machine.cache == ContentStore.home(sample.xdg.filter(_.nonEmpty), sample.home),
              machine.repo == repo,
              machine.running == Some(Path.of(sample.jdk)),
              machine.native == askedOrigin(sample.native),
              machine.env.get("XDG_CACHE_HOME") == sample.xdg,
              machine.env.get("HISTOS_NATIVE") == sample.native,
            )).ensuring(restore)
      ,
      test("a missing home wins, and a blank architecture is its own error"):
        check(gap): sample =>
          TestSystem.save.flatMap: restore =>
            (for
              _      <- TestSystem.putProperty("os.name", "Linux")
              _      <- TestSystem.putProperty("java.home", "jdk")
              _      <- TestSystem.clearEnv("HISTOS_NATIVE")
              _      <- TestSystem.clearEnv("XDG_CACHE_HOME")
              _      <- put("user.home", homeOf(sample))
              _      <- put("os.arch", archOf(sample))
              loaded <- Machine.load(repo).either
            yield assertTrue(loaded == Left(expected(sample)))).ensuring(restore)
      ,
      test("a finished step reports the clock"):
        check(Gen.long(0L, 60_000_000_000L)): nanos =>
          val info = StepInfo(StepId.Work("work"), Chunk.empty, Set(Need.Cpu))
          for
            restore <- TestClock.save
            entered <- Promise.make[Nothing, Unit]
            release <- Promise.make[Nothing, Unit]
            seen    <- ZIO
              .scoped:
                for
                  _       <- TestClock.setTime(Instant.EPOCH)
                  running <- ZIO.serviceWithZIO[Histos](
                    _.start(Step.local[Unit](info)(entered.succeed(()).unit *> release.await))
                  )
                  _    <- entered.await
                  _    <- TestClock.setTime(Instant.EPOCH.plusNanos(nanos))
                  _    <- release.succeed(())
                  _    <- running.outcome
                  seen <- running.events.take(2).runCollect
                yield seen
              .provide(opened)
              .ensuring(restore)
          yield
            val reported = seen.toList match
              case StepEvent.Started(id) :: StepEvent.Finished(done, Source.Ran, took) :: Nil =>
                id == info.id && done == info.id && took == Duration.fromNanos(nanos)
              case _ => false
            assertTrue(reported),
    )

  private def assign(sample: Host): UIO[Unit] =
    putEnv("XDG_CACHE_HOME", sample.xdg) *>
      putEnv("HISTOS_NATIVE", sample.native) *>
      TestSystem.putProperty("user.home", sample.home) *>
      TestSystem.putProperty("os.name", sample.osName) *>
      TestSystem.putProperty("os.arch", sample.arch) *>
      TestSystem.putProperty("java.home", sample.jdk)

  private def putEnv(name: String, value: Option[String]): UIO[Unit] =
    value match
      case Some(text) => TestSystem.putEnv(name, text)
      case None       => TestSystem.clearEnv(name)

  private def put(name: String, value: Option[String]): UIO[Unit] =
    value match
      case Some(text) => TestSystem.putProperty(name, text)
      case None       => TestSystem.clearProperty(name)

  private def homeOf(sample: Gap): Option[String] =
    sample match
      case Gap.NoHome(home, _) => home
      case Gap.NoArch(home, _) => Some(home)

  private def archOf(sample: Gap): Option[String] =
    sample match
      case Gap.NoHome(_, arch) => arch
      case Gap.NoArch(_, arch) => arch

  private def expected(sample: Gap): ToolchainError =
    sample match
      case _: Gap.NoHome => ToolchainError.NoUserHome
      case _: Gap.NoArch => ToolchainError.NoArchitecture

  private def askedOs(name: String): Os =
    if name.nonEmpty then Os.from(name) else Os.Other

  private def askedOrigin(native: Option[String]): NativeOrigin =
    native match
      case None      => NativeOrigin.Central
      case Some(raw) =>
        NativeToolchain.parse(raw) match
          case Left(error)   => NativeOrigin.Refused(error)
          case Right(source) => NativeOrigin.Chosen(source)

  private final case class Host(
      osName: String,
      arch: String,
      home: String,
      jdk: String,
      xdg: Option[String],
      native: Option[String],
  )

  private enum Gap:
    case NoHome(home: Option[String], arch: Option[String])
    case NoArch(home: String, arch: Option[String])

end MachineSpec

private final class Counting(inner: System, reads: Ref[(Int, Int)]) extends System:
  def env(variable: => String)(implicit trace: Trace): IO[SecurityException, Option[String]] =
    inner.env(variable)

  def envOrElse(variable: => String, alt: => String)(implicit trace: Trace): IO[SecurityException, String] =
    inner.envOrElse(variable, alt)

  def envOrOption(
      variable: => String,
      alt: => Option[String],
  )(implicit trace: Trace): IO[SecurityException, Option[String]] =
    inner.envOrOption(variable, alt)

  def envs(implicit trace: Trace): IO[SecurityException, Map[String, String]] =
    reads.update(count => (count._1 + 1, count._2)) *> inner.envs

  def lineSeparator(implicit trace: Trace): UIO[String] =
    inner.lineSeparator

  def properties(implicit trace: Trace): IO[Throwable, Map[String, String]] =
    reads.update(count => (count._1, count._2 + 1)) *> inner.properties

  def property(prop: => String)(implicit trace: Trace): IO[Throwable, Option[String]] =
    inner.property(prop)

  def propertyOrElse(prop: => String, alt: => String)(implicit trace: Trace): IO[Throwable, String] =
    inner.propertyOrElse(prop, alt)

  def propertyOrOption(
      prop: => String,
      alt: => Option[String],
  )(implicit trace: Trace): IO[Throwable, Option[String]] =
    inner.propertyOrOption(prop, alt)
end Counting
