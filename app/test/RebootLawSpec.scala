package histos.cli

import histos.Resolution
import histos.server.{Daemon, Fingerprint, Record, ServerError}
import histos.workspace.Workspace

import java.nio.file.Path

import zio.{Promise, Ref, ZIO, durationInt}
import zio.test.{TestClock, ZIOSpecDefault, assertTrue, suite, test}

/** Reboot returns when a server is listening. It does not become that server. The spawn is a law's stand-in, so no
  * process starts, and the wait for the record runs on the test clock.
  */
object RebootLawSpec extends ZIOSpecDefault:
  def spec = suite("reboot laws")(
    test("a missing script is no server, and nothing is spawned"):
      for
        repo    <- Workspace.work("histos-reboot-none")
        spawned <- Ref.make(0)
        found   <- Cli.bootCall(repo, _ => spawned.update(_ + 1)).either
        count   <- spawned.get
      yield assertTrue(found == Left(ServerError.NoServer), count == 0)
    ,
    test("a script with no record spawns one server and returns the socket it records, not before"):
      for
        repo    <- Workspace.work("histos-reboot")
        _       <- Workspace.writeString(repo.resolve("histos"), "#!/bin/sh\n")
        spawned <- Ref.make(0)
        started <- Promise.make[Nothing, Unit]
        socket = repo.resolve("server.sock").toAbsolutePath.normalize
        booting <- Cli.bootCall(repo, _ => spawned.update(_ + 1) *> started.succeed(()).unit).fork
        _       <- started.await
        _       <- TestClock.adjust(50.millis)
        waiting <- booting.poll
        print   <- Fingerprint.read(repo, Resolution.Solve)
        _       <- Record.write(repo, "0.0.0-law", print, socket, None)
        _       <- TestClock.adjust(50.millis)
        found   <- booting.join.either
        count   <- spawned.get
      yield assertTrue(waiting.isEmpty, found == Right(socket), count == 1)
    ,
    test("the successor reads nothing and appends the server log"):
      for
        repo <- Workspace.work("histos-reboot-launch")
        built  = Daemon.launch(repo)
        input  = built.redirectInput()
        output = built.redirectOutput()
        log    = repo.resolve(".histos").resolve("server.log").toFile
      yield assertTrue(
        built.command == java.util.List.of("./histos", "server"),
        built.directory == repo.toFile,
        input.`type`() == java.lang.ProcessBuilder.Redirect.Type.READ,
        Option(input.file()).contains(Path.of("/dev/null").toFile),
        built.redirectErrorStream(),
        output.`type`() == java.lang.ProcessBuilder.Redirect.Type.APPEND,
        Option(output.file()).contains(log),
      ),
  )
