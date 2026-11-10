package histos.cli

import histos.server.{Daemon, Record, ServerError}
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import zio.{UIO, ZIO, durationInt}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** Reboot returns when a server is listening. It does not become that server. */
object RebootLawSpec extends ZIOSpecDefault:
  def spec = suite("reboot laws")(
    test("a missing script is no server"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-reboot-none").catchAll(error => ZIO.dieMessage(error.message))
          found <- Cli.bootCall(repo).timeout(5.seconds).either
        yield assertTrue(found == Left(ServerError.NoServer))
      }
    ,
    test("no record starts a server and returns while that process is alive"):
      ZIO.scoped {
        for
          repo    <- Workspace.work("histos-reboot").catchAll(error => ZIO.dieMessage(error.message))
          _       <- plant(repo)
          _       <- ZIO.addFinalizer(stop(repo))
          found   <- Cli.bootCall(repo).timeout(5.seconds).either
          written <- Record.read(repo)
          alive   <- pidOf(repo).flatMap(running)
        yield assertTrue(
          found == Right(written.map(_.socket)),
          alive,
        )
      }
    ,
    test("the successor reads nothing and appends the server log"):
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-reboot-launch").catchAll(error => ZIO.dieMessage(error.message))
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
        )
      },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds) @@ TestAspect.sequential

  private def plant(repo: Path): UIO[Unit] =
    val script = repo.resolve("histos")
    val body   =
      """|#!/bin/sh
         |mkdir -p .histos
         |echo $$ > .histos/server.pid
         |sock="$PWD/.histos/server.sock"
         |cat > .histos/server.json <<EOF
         |{"version":"0.0.0-law","fingerprint":"abc","socket":"$sock"}
         |EOF
         |exec sleep 30
         |""".stripMargin
    ZIO.attemptBlocking {
      Files.writeString(script, body)
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))
      ()
    }.orDie

  private def pidOf(repo: Path): UIO[Option[Long]] =
    val file = repo.resolve(".histos").resolve("server.pid")
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(file) then Files.readString(file).trim.toLongOption
        else None
      }
      .orElseSucceed(None)

  private def running(pid: Option[Long]): UIO[Boolean] =
    pid match
      case None        => ZIO.succeed(false)
      case Some(value) =>
        ZIO
          .attemptBlocking(new ProcessBuilder("kill", "-0", value.toString).start().waitFor() == 0)
          .orElseSucceed(false)

  private def stop(repo: Path): UIO[Unit] =
    pidOf(repo).flatMap {
      case None      => ZIO.unit
      case Some(pid) =>
        ZIO.attemptBlocking {
          new ProcessBuilder("kill", pid.toString).start().waitFor()
          ()
        }.ignore
    }
end RebootLawSpec
