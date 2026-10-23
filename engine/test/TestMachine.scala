package histos

import histos.jdk.{ArchiveSource, Machine, Os, Settled, Sha256}
import histos.node.{NodeLock, NodeVersion}
import histos.testrun.{TestEvent, TestProcess}
import histos.toolchain.Runtimes

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import _root_.scala.jdk.CollectionConverters.*

import zio.{Chunk, System, Task, UIO, ZIO}

/** This machine as a command sees it, with a temp directory as the repository,
  * the home, and the cache.
  */
object TestMachine:
  def at(root: Path, env: Map[String, String]): Machine =
    Machine(
      env,
      root.toString,
      Os.Other,
      root.resolve("cache"),
      root,
      None,
      Settled.No
    )

  /** Needs `TestAspect.withLiveSystem`. */
  def runtimes(root: Path): UIO[Runtimes] =
    System.envs
      .orElseSucceed(Map.empty[String, String])
      .flatMap(env => Runtimes.read(at(root, env)))

  /** Pins the `node` on `PATH` through `.histos/node.local`, behind a wrapper
    * that appends each launch's arguments to the returned log and then runs
    * that `node`. The lock names its version. Needs
    * `TestAspect.withLiveSystem`.
    */
  def chosenNode(root: Path): Task[Path] =
    for
      env <- System.envs
      real <- ZIO
        .fromOption(onPath(env, "node"))
        .orElseFail(new IllegalStateException("no node on PATH"))
      _ <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("probe")))
      said <- TestProcess
        .command(
          List(real.toString, "--version"),
          root.resolve("probe"),
          Map.empty
        )
        .runCollect
        .mapError(error => new IllegalStateException(error.message))
      text = said
        .collectFirst { case TestEvent.Out(line) => line.trim }
        .getOrElse("")
      version <- ZIO
        .fromEither(NodeVersion.from(text))
        .mapError(error => new IllegalStateException(error.message))
      log = root.resolve("chosen-node.log")
      home = root.resolve("chosen-node")
      _ <- ZIO.attemptBlocking {
        executable(
          home.resolve("bin").resolve("node"),
          s"printf '%s\\n' \"$$*\" >> '$log'\nexec '$real' \"$$@\"\n"
        )
        Files.createDirectories(root.resolve(".histos"))
        Files.writeString(
          root.resolve(".histos").resolve("node.local"),
          home.toString + "\n"
        )
        val sha =
          Sha256.of(Chunk.fromArray("unused".getBytes(StandardCharsets.UTF_8)))
        val lock = NodeLock(
          version.major,
          version,
          ArchiveSource.Local(root.resolve("node.zip")),
          sha
        )
        Files.writeString(
          root.resolve(".histos").resolve("node.lock"),
          NodeLock.render(lock)
        )
      }
    yield log

  /** A `node` that only logs its arguments and fails. Its home is `home`, so
    * put `home/bin` on `PATH`.
    */
  def fakeNode(home: Path, log: Path): Task[Unit] =
    ZIO.attemptBlocking(
      executable(
        home.resolve("bin").resolve("node"),
        s"printf '%s\\n' \"$$*\" >> '$log'\nexit 3\n"
      )
    )

  /** What a logged `node` was launched with. Node selection's `--version` probe
    * is not a launch.
    */
  def launches(log: Path): Task[List[String]] =
    ZIO.attemptBlocking:
      if !Files.isRegularFile(log) then Nil
      else Files.readAllLines(log).asScala.toList.filterNot(_ == "--version")

  private def executable(path: Path, body: String): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, s"#!/bin/sh\n$body")
    Files.setPosixFilePermissions(
      path,
      PosixFilePermissions.fromString("rwxr-xr-x")
    )

  private def onPath(env: Map[String, String], name: String): Option[Path] =
    env
      .get("PATH")
      .toList
      .flatMap(_.split(File.pathSeparatorChar).toList)
      .filter(_.nonEmpty)
      .map(dir => Path.of(dir, name))
      .find(Files.isExecutable)
