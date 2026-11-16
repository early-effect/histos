package histos.process

import histos.server.RunLaunch

import java.nio.file.Path

import zio.{UIO, ZIO}

/** The accepting process's environment and working directory. */
object Shells:
  /** Live system, so a test `System` service cannot replace the process environment. */
  def capture: UIO[RunLaunch.Shell] =
    ZIO.withSystem(zio.System.SystemLive):
      for
        env <- zio.System.envs.orElseSucceed(Map.empty[String, String])
        raw <- zio.System.property("user.dir").orElseSucceed(Option.empty[String])
      yield
        val shell: RunLaunch.Shell = RunLaunch.Shell(env, directory(raw))
        shell

  private def directory(raw: Option[String]): Path =
    raw match
      case Some(text) if text.nonEmpty => Path.of(text)
      case _                           => Path.of("").toAbsolutePath
end Shells
