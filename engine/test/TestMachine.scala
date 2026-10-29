package histos

import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.toolchain.Runtimes

import java.nio.file.Path

import zio.{System, UIO, ZIO}

/** This machine as a command sees it, with a temp directory as the repository, the home, and the cache.
  */
object TestMachine:
  def at(root: Path, env: Map[String, String]): Machine =
    Machine(
      env,
      root.toString,
      Os.Other,
      histos.toolchain.Architecture.Unsupported("fixture"),
      root.resolve("cache"),
      root,
      None,
      NativeOrigin.read(env),
    )

  /** Needs `TestAspect.withLiveSystem`. */
  def runtimes(root: Path): UIO[Runtimes] =
    System.envs
      .orElseSucceed(Map.empty[String, String])
      .flatMap(env => Runtimes.read(at(root, env)))
end TestMachine
