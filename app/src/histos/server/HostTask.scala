package histos.server

import histos.jdk.Machine
import histos.lock.LockSubject
import histos.plugin.{Digests, PluginCoordinate}
import histos.process.ProcessTree
import histos.resolve.{LockDocument, LocatorName, SnapshotLocator, VersionedPin}
import histos.task.{HostIo, HostProof, HostVerdict, TaskError, TaskResult}
import histos.ProjectName

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, UIO, ZIO}

/** The checkout that runs `host` is not the proof repo. The scratch server keeps this process's `CI`. The client does
  * not, and its `HISTOS_VERSION` is the scratch `server.json` version with no added prefix.
  */
object HostTask:
  def fromRepo(repo: Path, machine: Machine): IO[TaskError, TaskResult] =
    prove(repo, machine).mapError(TaskError.Host(_))

  private def prove(repo: Path, machine: Machine): IO[HostProof, TaskResult] =
    for
      jarText <- required(machine, "HISTOS_JAR")
      java    <- required(machine, "HISTOS_JAVA")
      version <- required(machine, "HISTOS_VERSION")
      jar     <- pathOf("HISTOS_JAR", jarText)
      id      <- ZIO.fromEither(toolchainId(version))
      client  <- ZIO.fromEither(clientOf(jar))
      _       <- executable(client)
      result  <- ZIO.scoped(ceremony(repo, id, jar, java, client))
    yield result

  private def ceremony(
      repo: Path,
      id: String,
      jar: Path,
      java: String,
      client: Path,
  ): ZIO[Scope, HostProof, TaskResult] =
    for
      root   <- ZIO.acquireRelease(temp)(remove)
      _      <- place(root, id)
      _      <- ZIO.acquireRelease(boot(repo, root))(stop)
      record <- awaitServer(root)
      result <- ask(root, client, jar, java, record.version)
    yield result

  private def required(machine: Machine, name: String): IO[HostProof, String] =
    ZIO.fromEither:
      machine.env
        .get(name)
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(HostProof.Io(Path.of(name), HostIo.Absent))

  private def pathOf(name: String, raw: String): IO[HostProof, Path] =
    ZIO.attempt(Path.of(raw)).mapError(_ => HostProof.Io(Path.of(name), HostIo.Absent))

  private def toolchainId(version: String): Either[HostProof, String] =
    version match
      case s"histos $id" if oneSegment(id) => Right(id)
      case _                               => Left(HostProof.Io(Path.of("HISTOS_VERSION"), HostIo.Absent))

  private def oneSegment(id: String): Boolean =
    id.nonEmpty && id != "." && id != ".." && id.indexOf('/') < 0 && id.indexOf('\\') < 0 && id.indexOf(' ') < 0

  private def clientOf(jar: Path): Either[HostProof, Path] =
    Option(jar.getParent) match
      case None      => Left(HostProof.Io(jar, HostIo.Stat))
      case Some(dir) => Right(dir.resolve("histos-client"))

  private def executable(path: Path): IO[HostProof, Unit] =
    ZIO
      .attemptBlocking(Files.isRegularFile(path) && Files.isExecutable(path))
      .mapError(_ => HostProof.Io(path, HostIo.Stat))
      .flatMap: ready =>
        if ready then ZIO.unit else ZIO.fail(HostProof.ClientAbsent(path))

  private def temp: IO[HostProof, Path] =
    ZIO
      .attemptBlocking(Files.createTempDirectory("histos-host"))
      .mapError(_ => HostProof.Io(Path.of("histos-host"), HostIo.Write))

  private def remove(root: Path): UIO[Unit] =
    ZIO.attemptBlocking(delete(root)).ignore

  private def delete(path: Path): Unit =
    if Files.isDirectory(path) then
      val listed = Files.list(path)
      try listed.iterator().asScala.foreach(delete)
      finally listed.close()
    if Files.deleteIfExists(path) then () else ()

  private def place(root: Path, id: String): IO[HostProof, Unit] =
    for
      _ <- write(root.resolve("histos.scala"), build)
      // Absent, the launcher exits before a server exists. The toolchain file does not replace it.
      _ <- write(root.resolve("build.properties"), s"histos.version=$id\n")
      _ <- write(root.resolve("dependencies.lock"), lockText)
      _ <- write(root.resolve("shots"), shotsBytes)
      _ <- write(root.resolve("tools"), toolsBytes)
      _ <- write(root.resolve(".histos").resolve("toolchain"), s"$id\n")
    yield ()

  private def write(path: Path, text: String): IO[HostProof, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(Files.createDirectories(_))
        Files.writeString(path, text, StandardCharsets.UTF_8)
        ()
      }
      .mapError(_ => HostProof.Io(path, HostIo.Write))

  private def boot(repo: Path, root: Path): IO[HostProof, Process] =
    val script = repo.resolve("histos").toAbsolutePath
    val log    = root.resolve(".histos").resolve("server.log")
    ZIO
      .attemptBlocking {
        Option(log.getParent).foreach(Files.createDirectories(_))
        if !Files.exists(log) then Files.createFile(log)
        new ProcessBuilder(script.toString, "server")
          .directory(root.toFile)
          .redirectInput(ProcessBuilder.Redirect.from(Path.of("/dev/null").toFile))
          .redirectErrorStream(true)
          .redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile))
          .start()
      }
      .mapError(_ => HostProof.Io(script, HostIo.Spawn))

  private def stop(process: Process): UIO[Unit] =
    ProcessTree.of(process).flatMap(_.destroy)

  private def awaitServer(root: Path): IO[HostProof, ServerFile] =
    val file                                       = root.resolve(".histos").resolve("server.json")
    def poll(left: Int): IO[HostProof, ServerFile] =
      readRecord(file).flatMap:
        case Some(parsed)     => ZIO.succeed(parsed)
        case None if left > 0 => ZIO.sleep(pollGap) *> poll(left - 1)
        case None             => serverNote(root).flatMap(note => ZIO.fail(HostProof.ServerSilent(file, note)))
    poll(polls)

  /** The launcher's own line, when the scratch server exits before `server.json`. */
  private def serverNote(root: Path): UIO[String] =
    ZIO
      .attemptBlocking {
        val log = root.resolve(".histos").resolve("server.log")
        if !Files.isRegularFile(log) then ""
        else
          val text = Files.readString(log).trim
          val end  = text.indexOf('\n')
          val line = if end < 0 then text else text.substring(0, end).trim
          if line.length <= 200 then line else line.substring(0, 200)
      }
      .orElseSucceed("")

  private def readRecord(file: Path): IO[HostProof, Option[ServerFile]] =
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(file) then None
        else ServerFile.parse(Files.readString(file))
      }
      .mapError(_ => HostProof.Io(file, HostIo.Read))

  private def ask(
      root: Path,
      client: Path,
      jar: Path,
      java: String,
      version: String,
  ): ZIO[Scope, HostProof, TaskResult] =
    val out = root.resolve(".histos").resolve("client.out")
    val err = root.resolve(".histos").resolve("client.err")
    for
      process <- ZIO.acquireRelease(startClient(root, client, jar, java, version, out, err))(stop)
      code    <- ZIO.attemptBlocking(process.waitFor()).mapError(_ => HostProof.Io(client, HostIo.Wait))
      text    <- captured(out, err)
      result  <- ZIO.fromEither(decided(HostVerdict.verdict(code, text)))
    yield result

  private def decided(verdict: HostVerdict): Either[HostProof, TaskResult] =
    verdict match
      case HostVerdict.Consent       => Right(TaskResult.Host)
      case HostVerdict.Failed(proof) => Left(proof)

  private def startClient(
      root: Path,
      client: Path,
      jar: Path,
      java: String,
      version: String,
      out: Path,
      err: Path,
  ): IO[HostProof, Process] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder(client.toString, "resolve", "--refresh")
          .directory(root.toFile)
          .redirectInput(ProcessBuilder.Redirect.from(Path.of("/dev/null").toFile))
          .redirectOutput(out.toFile)
          .redirectError(err.toFile)
        val env = builder.environment()
        drop(env, "CI")
        drop(env, "GITHUB_ACTIONS")
        put(env, "HISTOS_JAR", jar.toString)
        put(env, "HISTOS_JAVA", java)
        put(env, "HISTOS_VERSION", version)
        builder.start()
      }
      .mapError(_ => HostProof.Io(client, HostIo.Spawn))

  private def drop(env: java.util.Map[String, String], key: String): Unit =
    if Option(env.remove(key)).isDefined then () else ()

  private def put(env: java.util.Map[String, String], key: String, value: String): Unit =
    if Option(env.put(key, value)).isDefined then () else ()

  private def captured(out: Path, err: Path): IO[HostProof, String] =
    ZIO
      .attemptBlocking {
        val stdout = if Files.isRegularFile(out) then Files.readString(out) else ""
        val stderr = if Files.isRegularFile(err) then Files.readString(err) else ""
        stdout + stderr
      }
      .mapError(_ => HostProof.Io(out, HostIo.Read))

  private val pollGap = zio.Duration.fromMillis(50)
  private val polls   = 1200

  private val build: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |""".stripMargin

  /** The lock records `snap-1`. The file is `snap-2`, so refresh sees one changed locator. */
  private val shotsBytes = "snap-2"
  private val toolsBytes = "tools-1"

  private def lockText: String =
    val pin = VersionedPin(
      PluginCoordinate.Module(ProjectName("app")),
      LockSubject.Artifact,
      Digests.content("app-1".getBytes(StandardCharsets.UTF_8)),
      None,
    )
    val shots = SnapshotLocator(LocatorName("shots"), Digests.content("snap-1".getBytes(StandardCharsets.UTF_8)))
    val tools = SnapshotLocator(LocatorName("tools"), Digests.content(toolsBytes.getBytes(StandardCharsets.UTF_8)))
    LockDocument.render(LockDocument(List(pin), List(shots, tools)))
end HostTask
