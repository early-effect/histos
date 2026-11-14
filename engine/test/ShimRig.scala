package histos.publish

import com.sun.net.httpserver.SimpleFileServer

import histos.version.{ShipNumber, SnapshotPointer}
import histos.workspace.{IoFailure, Workspace}

import java.io.File
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}

/** `install/histos` against a loopback dist, a user home, and a project. A fake `java` on `PATH` answers `-version` and
  * records how it was run. Each served client is a script that records the same. Each law gets its own rig.
  */
final case class ShimRig(root: Path, base: String, shim: Path, path: String):
  import ShimRig.*

  val dist: Path    = root.resolve("dist")
  val home: Path    = root.resolve("home")
  val project: Path = root.resolve("project")
  val user: Path    = root.resolve("user")
  val bin: Path     = root.resolve("bin")

  /** The shim `install/histos` installs itself as. A command run through it is a launch, not an install. */
  val installed: Path = home.resolve("bin").resolve("histos")

  def publish(published: Published): ZIO[Any, ShimFault, Unit] =
    val versions =
      published.releases.map(release => release.number.render -> release.clients) ++
        published.stamps.map(stamp => stamp.render -> false)
    val files =
      versions.flatMap(bundle) ++
        published.channels.toList.map((line, stamp) =>
          DistLayout.channel(SnapshotPointer(line).render) -> s"${stamp.render}\n"
        )
    val assets = files.map(_._1).sorted.mkString("", "\n", "\n")
    ZIO
      .foreachDiscard(("assets" -> assets) :: files)((name, text) => Workspace.writeString(dist.resolve(name), text))
      .mapError(ShimFault.Io(_))

  def pin(raw: String): ZIO[Any, ShimFault, Unit] =
    Workspace.writeString(project.resolve(LauncherPin.file), s"${LauncherPin.Key}=$raw\n").mapError(ShimFault.Io(_))

  def choice: ZIO[Any, ShimFault, LauncherChoice] =
    LauncherPin.read(project).mapBoth(ShimFault.Pin(_), _.choice)

  def pinned: ZIO[Any, ShimFault, Boolean] =
    ZIO.attemptBlocking(Files.exists(project.resolve(LauncherPin.file))).orElseFail(ShimFault.Stat)

  def default: ZIO[Any, ShimFault, LauncherChoice] =
    Workspace
      .readString(home.resolve("default"))
      .mapError(ShimFault.Io(_))
      .flatMap(text => ZIO.fromEither(LauncherPin.from(text.trim)).mapBoth(ShimFault.Pin(_), _.choice))

  def origin(version: String): ZIO[Any, ShimFault, Option[String]] =
    Workspace.readIfPresent(toolchain(version).resolve("client.origin")).mapBoth(ShimFault.Io(_), _.map(_.trim))

  def installedJar(version: String): ZIO[Any, ShimFault, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(toolchain(version).resolve("histos.jar"))).orElseFail(ShimFault.Stat)

  /** The client goes, as a shim that fetched only the jar would have left it. */
  def dropClient(version: String): ZIO[Any, ShimFault, Unit] =
    ZIO
      .attemptBlocking:
        Files.deleteIfExists(toolchain(version).resolve("histos-client"))
        Files.deleteIfExists(toolchain(version).resolve("client.origin"))
      .unit
      .orElseFail(ShimFault.Stat)

  def toolchain(version: String): Path = home.resolve("toolchains").resolve(version)

  /** The fake `java` reports `version` for `-version`. */
  def jdk(version: String): ZIO[Any, ShimFault, Unit] =
    script(
      bin.resolve("java"),
      s"""#!/bin/sh
         |if [ "$${1:-}" = -version ]; then echo 'openjdk version "$version" 2026-10-24' >&2; exit 0; fi
         |${record("java")}
         |""".stripMargin,
    )

  /** What the last exec recorded: the program, `HISTOS_JAR`, `HISTOS_VERSION`, and the arguments. */
  def executed: ZIO[Any, ShimFault, Option[Executed]] =
    Workspace
      .readIfPresent(root.resolve("exec"))
      .mapBoth(ShimFault.Io(_), _.flatMap(Executed.parse))

  def update(flags: String*): ZIO[Any, ShimFault, Ran] = run(shim, "update" :: flags.toList, Map.empty)

  def install(version: String): ZIO[Any, ShimFault, Ran] = run(shim, List("toolchain", "install", version), Map.empty)

  def launch(args: String*): ZIO[Any, ShimFault, Ran] = run(installed, args.toList, Map.empty)

  /** `extra` is the parent environment. The shim replaces `HISTOS_VERSION` before it execs. */
  def launchWith(extra: Map[String, String], args: String*): ZIO[Any, ShimFault, Ran] =
    run(installed, args.toList, extra)

  /** `sh <script> <args>` in the project, with stdin from `/dev/null` so the shim never sees a terminal. */
  private def run(script: Path, args: List[String], extra: Map[String, String]): ZIO[Any, ShimFault, Ran] =
    val err = root.resolve("stderr")
    ZIO.scoped:
      ZIO.attemptBlocking(Files.deleteIfExists(root.resolve("exec"))).orElseFail(ShimFault.Stat) *>
        ZIO
          .acquireRelease(start(script, err, args, extra))(process => ZIO.succeed(process.destroyForcibly()).unit)
          .flatMap(process => ZIO.attemptBlockingInterrupt(process.waitFor()).orElseFail(ShimFault.Wait))
          .flatMap: exit =>
            Workspace.readString(err).mapBoth(ShimFault.Io(_), text => Ran(exit, text.linesIterator.toList))

  private def start(
      script: Path,
      err: Path,
      args: List[String],
      extra: Map[String, String],
  ): ZIO[Any, ShimFault, java.lang.Process] =
    ZIO
      .attemptBlocking:
        val builder = new ProcessBuilder(("sh" :: script.toString :: args).asJava)
        builder.directory(project.toFile)
        val env = builder.environment()
        env.clear()
        env.put("PATH", s"$bin${File.pathSeparator}$path")
        env.put("HOME", user.toString)
        env.put("HISTOS_HOME", home.toString)
        env.put("HISTOS_DIST", base)
        extra.foreach((key, value) => env.put(key, value))
        builder.redirectInput(ProcessBuilder.Redirect.from(new File("/dev/null")))
        builder.redirectOutput(ProcessBuilder.Redirect.DISCARD)
        builder.redirectError(err.toFile)
        builder.start()
      .orElseFail(ShimFault.Start)

  /** The manifest, the jar, and with `clients` one client script per platform the shim names. */
  private def bundle(version: String, clients: Boolean): List[(String, String)] =
    val jar      = s"jar $version\n"
    val binaries =
      if clients then
        DistPlatform.values.toList.map(platform => platform -> s"#!/bin/sh\n${record(s"client-${platform.token}")}\n")
      else Nil
    val manifest = DistLayout.manifestJson(version, sha(jar), binaries.map((platform, body) => platform -> sha(body)))
    List(DistLayout.manifest(version) -> manifest, DistLayout.jar(version) -> jar) ++
      binaries.map((platform, body) => DistLayout.client(version, platform) -> body)

  private def record(program: String): String =
    val out = root.resolve("exec")
    s"""{ echo '$program'; echo "HISTOS_JAR=$${HISTOS_JAR:-}"; echo "HISTOS_VERSION=$${HISTOS_VERSION:-}"; for arg in "$$@"; do echo "$$arg"; done; } > '$out'"""

  private def script(file: Path, text: String): ZIO[Any, ShimFault, Unit] =
    Workspace.writeString(file, text).mapError(ShimFault.Io(_)) *>
      ZIO
        .attemptBlocking(Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rwxr-xr-x")))
        .unit
        .orElseFail(ShimFault.Stat)
end ShimRig

object ShimRig:
  /** One timestamped build of a snapshot line. A higher build is a later stamp. */
  final case class Stamp(line: ShipNumber, build: Int):
    def channel: LauncherChannel = LauncherChannel.Line(SnapshotPointer(line))
    def render: String           = f"${line.render}-20261107.0100$build%02d-$build"

  final case class Release(number: ShipNumber, clients: Boolean)

  /** What the dist lists. Each line's channel is its highest build. */
  final case class Published(releases: List[Release], stamps: List[Stamp]):
    def channels: Map[ShipNumber, Stamp] =
      stamps.groupMapReduce(_.line)(identity)((left, right) => if left.build >= right.build then left else right)

  final case class Ran(exit: Int, err: List[String])

  /** The program the shim exec'd (`java` or `client-<platform>`), `HISTOS_JAR`, `HISTOS_VERSION`, and the arguments. */
  final case class Executed(program: String, jar: String, version: String, args: List[String])

  object Executed:
    def parse(text: String): Option[Executed] =
      text.linesIterator.toList match
        case program :: s"HISTOS_JAR=$jar" :: s"HISTOS_VERSION=$version" :: args =>
          Some(Executed(program, jar, version, args))
        case _ => None

  enum ShimFault:
    case NoRepo
    case Io(failure: IoFailure)
    case Serve
    case Start
    case Wait
    case Stat
    case Pin(error: LauncherError)

  /** A rig whose fake `java` is 25. */
  def loopback: ZIO[Scope, ShimFault, ShimRig] =
    for
      repo <- ZIO.systemWith(_.env("HISTOS_REPO")).orElseFail(ShimFault.NoRepo).someOrFail(ShimFault.NoRepo)
      path <- ZIO.systemWith(_.env("PATH")).orElseFail(ShimFault.NoRepo).someOrFail(ShimFault.NoRepo)
      root <- Workspace.work("histos-shim").mapError(ShimFault.Io(_))
      dirs = List("dist", "home", "project", "user", "bin").map(root.resolve)
      _    <- ZIO.foreachDiscard(dirs)(Workspace.createDirectories).mapError(ShimFault.Io(_))
      port <- serve(root.resolve("dist"))
      rig = ShimRig(root, s"http://127.0.0.1:$port", Path.of(repo, "install", "histos"), path)
      _ <- rig.jdk("25.0.1")
    yield rig

  private def serve(dist: Path): ZIO[Scope, ShimFault, Int] =
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking:
            val server = SimpleFileServer.createFileServer(
              new InetSocketAddress("127.0.0.1", 0),
              dist.toAbsolutePath,
              SimpleFileServer.OutputLevel.NONE,
            )
            server.start()
            server
          .orElseFail(ShimFault.Serve)
      )(server => ZIO.succeed(server.stop(0)))
      .map(_.getAddress.getPort)

  private def sha(text: String): String = DistLayout.sha256(text.getBytes(StandardCharsets.UTF_8))
end ShimRig
