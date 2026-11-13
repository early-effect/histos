package histos.supply

import histos.doctor.HistosCommand
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.{Files, Path}

import scala.jdk.OptionConverters.*

import zio.*
import zio.test.*

/** A fix's command is the checkout's `./histos` and its words, with no shell. Each law writes its own `./histos`. */
object CommandRunnerLawSpec extends ZIOSpecDefault:
  private val tell =
    """#!/bin/sh
      |for word in "$@"; do echo "word [$word]"; done
      |echo "on stderr" >&2
      |if read line; then echo "read [$line]"; else echo "stdin is empty"; fi
      |exit 3
      |""".stripMargin

  def spec = suite("command runner laws")(
    test("a captured command reads nothing, and its words, output, errors, and status come back to the caller"):
      for
        repo <- Workspace.work("runner-captured")
        _    <- script(repo, tell)
        ran  <- run(repo, HistosCommand.of("workflow", "a b"), CommandRunner.captured)
      yield assertTrue(ran == Ran.Exited(3, List("word [workflow]", "word [a b]", "on stderr", "stdin is empty")))
    ,
    test("a checkout with no ./histos runs nothing, attached or captured"):
      for
        repo     <- Workspace.work("runner-missing")
        attached <- run(repo, HistosCommand.of("workflow", "--update"), CommandRunner.attached)
        captured <- run(repo, HistosCommand.of("workflow", "--update"), CommandRunner.captured)
      yield assertTrue(attached == Ran.Unstarted, captured == Ran.Unstarted)
    ,
    test("an interrupted command ends, attached or captured, so no child outlives its fix"):
      for
        repo     <- Workspace.work("runner-interrupt")
        _        <- script(repo, "#!/bin/sh\necho $$ > \"$1\"\nexec sleep 600\n")
        attached <- interrupted(repo, CommandRunner.attached, "attached.pid")
        captured <- interrupted(repo, CommandRunner.captured, "captured.pid")
      yield assertTrue(!attached, !captured),
  ) @@ TestAspect.timeout(60.seconds)

  private def run(repo: Path, command: HistosCommand, runner: ULayer[CommandRunner]): UIO[Ran] =
    ZIO.serviceWithZIO[CommandRunner](_.run(repo, command)).provideLayer(runner)

  /** Whether the child is alive after its run is interrupted. The child writes its pid, then sleeps. The interrupt may
    * land before the runner's wait begins, and the child must still end. The wait for its exit is bounded by the
    * suite's timeout, so a child that outlives its fix fails the law.
    */
  private def interrupted(repo: Path, runner: ULayer[CommandRunner], name: String): UIO[Boolean] =
    val pid = repo.resolve(name)
    for
      fiber  <- run(repo, HistosCommand.of(pid.toString), runner).fork
      _      <- ZIO.attemptBlocking(Files.isRegularFile(pid) && Files.size(pid) > 0).orDie.repeatUntil(identity)
      number <- ZIO.attemptBlocking(Files.readString(pid).trim.toLong).orDie
      _      <- fiber.interrupt
      handle <- ZIO.succeed(ProcessHandle.of(number).toScala)
      alive  <- ZIO.foreach(handle)(child => ZIO.fromCompletableFuture(child.onExit()).orDie.map(_.isAlive))
    yield alive.contains(true)

  private def script(repo: Path, text: String): IO[IoFailure, Unit] =
    val path = repo.resolve("histos")
    Workspace.writeString(path, text) *> ZIO.attemptBlocking(path.toFile.setExecutable(true)).orDie.unit
end CommandRunnerLawSpec
