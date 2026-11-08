package histos.publish

import com.sun.net.httpserver.SimpleFileServer

import histos.version.{ShipNumber, SnapshotPointer}
import histos.workspace.{IoFailure, Workspace}

import java.io.File
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

/** `histos update` through `install/histos` against a loopback dist. A release moves to the newest release. A snapshot
  * moves to the newest line at or above its own and keeps its form. A release at or above that line is offered.
  */
object ShimUpdateLawSpec extends ZIOSpecDefault:
  def spec = suite("shim update laws")(
    test("a release pin moves to the newest release, with its client when the manifest names one"):
      check(releases): published =>
        ZIO.scoped:
          for
            rig <- loopback
            _   <- rig.publish(Published(published, Nil))
            oldest = published.minBy(_.number)
            newest = published.maxBy(_.number)
            _      <- rig.pin(oldest.number.render)
            ran    <- rig.update()
            choice <- rig.choice
            origin <- rig.origin(newest.number.render)
            jar    <- rig.installed(newest.number.render)
          yield assertTrue(
            ran.exit == 0,
            choice == LauncherChoice.Release(newest.number),
            jar,
            origin == Option.when(newest.clients)("prebuilt"),
            ran.err.exists(clientless(newest.number.render)) == !newest.clients,
          )
    ,
    test("a timestamp pin moves to the newest timestamp of the newest line, and a later build moves it again"):
      check(lines): stamps =>
        ZIO.scoped:
          val oldest = stamps.minBy(stamp => (stamp.line, stamp.build))
          val newest = stamps.maxBy(stamp => (stamp.line, stamp.build))
          val later  = newest.copy(build = newest.build + 1)
          for
            rig    <- loopback
            _      <- rig.publish(Published(Nil, stamps))
            _      <- rig.pin(oldest.render)
            first  <- rig.update()
            moved  <- rig.choice
            origin <- rig.origin(newest.render)
            again  <- rig.update()
            held   <- rig.choice
            _      <- rig.publish(Published(Nil, later :: stamps))
            _      <- rig.update()
            next   <- rig.choice
          yield assertTrue(
            first.exit == 0,
            moved == LauncherChoice.Revision(newest.channel, newest.render),
            origin.isEmpty,
            first.err == List(
              s"histos ${newest.render} is a snapshot. Snapshots have no prebuilt client. java -jar will run."
            ),
            again.exit == 0,
            held == moved,
            next == LauncherChoice.Revision(later.channel, later.render),
          )
    ,
    test("a pointer stays a pointer, and without a terminal the release is a hint"):
      ZIO.scoped:
        for
          rig    <- loopback
          _      <- rig.publish(cut)
          _      <- rig.pin(SnapshotPointer(line020).render)
          ran    <- rig.update()
          choice <- rig.choice
          jar    <- rig.installed(build021.render)
          origin <- rig.origin(build021.render)
        yield assertTrue(
          ran.exit == 0,
          choice == LauncherChoice.Pointer(build021.channel),
          jar,
          origin.isEmpty,
          ran.err == List(
            s"histos ${release021.render} is a stable release. histos update --stable switches.",
            s"histos ${build021.render} is a snapshot. Snapshots have no prebuilt client; ${release021.render} has one. java -jar will run.",
          ),
        )
    ,
    test("--stable switches a snapshot pin to the release and installs its client"):
      ZIO.scoped:
        for
          rig     <- loopback
          _       <- rig.publish(cut)
          _       <- rig.pin(build020.render)
          ran     <- rig.update("--stable", "--make-default")
          choice  <- rig.choice
          default <- rig.default
          origin  <- rig.origin(release021.render)
        yield assertTrue(
          ran.exit == 0,
          ran.err.isEmpty,
          choice == LauncherChoice.Release(release021),
          default == choice,
          origin.contains("prebuilt"),
        )
    ,
    test("--freeze writes the timestamp, --follow writes the pointer, and both together is a usage failure"):
      ZIO.scoped:
        val snapshots = Published(Nil, List(build020, build021))
        for
          rig      <- loopback
          _        <- rig.publish(snapshots)
          _        <- rig.pin(SnapshotPointer(line020).render)
          frozen   <- rig.update("--freeze").zipRight(rig.choice)
          followed <- rig.update("--follow").zipRight(rig.choice)
          both     <- rig.update("--freeze", "--follow")
          kept     <- rig.choice
        yield assertTrue(
          frozen == LauncherChoice.Revision(build021.channel, build021.render),
          followed == LauncherChoice.Pointer(build021.channel),
          both.exit != 0,
          both.err == List("histos: --freeze and --follow are opposites. Pass one."),
          kept == followed,
        )
    ,
    test("outside a project the default follows the same rules"):
      ZIO.scoped:
        for
          rig <- loopback
          _   <- rig.publish(Published(Nil, List(build020, build021)))
          _   <- Workspace.writeString(rig.home.resolve("default"), s"${build020.render}\n").mapError(ShimFault.Io(_))
          ran <- rig.update()
          default <- rig.default
          pin     <- rig.pinned
        yield assertTrue(
          ran.exit == 0,
          default == LauncherChoice.Revision(build021.channel, build021.render),
          !pin,
        ),
  ) @@ TestAspect.withLiveSystem @@ TestAspect.samples(4)

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

  enum ShimFault:
    case NoRepo
    case Io(failure: IoFailure)
    case Serve
    case Start
    case Wait
    case Stat
    case Pin(error: LauncherError)

  /** A dist served on loopback, a user home, and a project. Each test gets its own. */
  final case class Rig(root: Path, base: String, shim: Path, path: String):
    val dist: Path    = root.resolve("dist")
    val home: Path    = root.resolve("home")
    val project: Path = root.resolve("project")
    val user: Path    = root.resolve("user")

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

    def installed(version: String): ZIO[Any, ShimFault, Boolean] =
      ZIO.attemptBlocking(Files.isRegularFile(toolchain(version).resolve("histos.jar"))).orElseFail(ShimFault.Stat)

    /** `sh install/histos update` in the project, with stdin from `/dev/null` so the shim never sees a terminal. A
      * project with no pin is outside a project.
      */
    def update(flags: String*): ZIO[Any, ShimFault, Ran] =
      val err = root.resolve("stderr")
      ZIO.scoped:
        ZIO
          .acquireRelease(start(err, flags.toList))(process => ZIO.succeed(process.destroyForcibly()).unit)
          .flatMap(process => ZIO.attemptBlockingInterrupt(process.waitFor()).orElseFail(ShimFault.Wait))
          .flatMap: exit =>
            Workspace.readString(err).mapBoth(ShimFault.Io(_), text => Ran(exit, text.linesIterator.toList))

    private def start(err: Path, flags: List[String]): ZIO[Any, ShimFault, java.lang.Process] =
      ZIO
        .attemptBlocking:
          val builder = new ProcessBuilder(("sh" :: shim.toString :: "update" :: flags).asJava)
          builder.directory(project.toFile)
          val env = builder.environment()
          env.clear()
          env.put("PATH", path)
          env.put("HOME", user.toString)
          env.put("HISTOS_HOME", home.toString)
          env.put("HISTOS_DIST", base)
          builder.redirectInput(ProcessBuilder.Redirect.from(new File("/dev/null")))
          builder.redirectOutput(ProcessBuilder.Redirect.DISCARD)
          builder.redirectError(err.toFile)
          builder.start()
        .orElseFail(ShimFault.Start)

    private def toolchain(version: String): Path = home.resolve("toolchains").resolve(version)
  end Rig

  private def loopback: ZIO[Scope, ShimFault, Rig] =
    for
      repo <- ZIO.systemWith(_.env("HISTOS_REPO")).orElseFail(ShimFault.NoRepo).someOrFail(ShimFault.NoRepo)
      path <- ZIO.systemWith(_.env("PATH")).orElseFail(ShimFault.NoRepo).someOrFail(ShimFault.NoRepo)
      root <- Workspace.work("histos-shim-update").mapError(ShimFault.Io(_))
      dirs = List("dist", "home", "project", "user").map(root.resolve)
      _    <- ZIO.foreachDiscard(dirs)(Workspace.createDirectories).mapError(ShimFault.Io(_))
      port <- serve(root.resolve("dist"))
    yield Rig(root, s"http://127.0.0.1:$port", Path.of(repo, "install", "histos"), path)

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

  /** The manifest, the jar, and with `clients` one client per platform the shim names. */
  private def bundle(version: String, clients: Boolean): List[(String, String)] =
    val jar      = s"jar $version\n"
    val binaries =
      if clients then DistPlatform.values.toList.map(platform => platform -> s"client $version ${platform.token}\n")
      else Nil
    val manifest = DistLayout.manifestJson(
      version,
      sha(jar),
      binaries.map((platform, body) => platform -> sha(body)),
      None,
    )
    List(DistLayout.manifest(version) -> manifest, DistLayout.jar(version) -> jar) ++
      binaries.map((platform, body) => DistLayout.client(version, platform) -> body)

  private def sha(text: String): String = DistLayout.sha256(text.getBytes(StandardCharsets.UTF_8))

  private def clientless(version: String)(line: String): Boolean =
    line.startsWith(s"histos $version has no prebuilt client for ") &&
      line.endsWith(". java -jar will run. --compile-client links one when clang is available.")

  private val line020    = ShipNumber("0.2.0")
  private val release021 = ShipNumber("0.2.1")
  private val build020   = Stamp(line020, 4)
  private val build021   = Stamp(release021, 1)

  /** The cut that froze a laptop on 0.2.0: release 0.2.1 with clients, and both snapshot lines. */
  private val cut = Published(List(Release(release021, clients = true)), List(build020, build021))

  private val number: Gen[Any, ShipNumber] =
    (Gen.int(0, 2) <*> Gen.int(0, 3) <*> Gen.int(0, 3))
      .map((major, minor, patch) => ShipNumber.from(s"$major.$minor.$patch"))
      .collect:
        case Right(parsed) => parsed

  private val releases: Gen[Any, List[Release]] =
    Gen
      .setOfBounded(1, 4)(number)
      .flatMap: numbers =>
        Gen.listOfN(numbers.size)(Gen.boolean).map(numbers.toList.zip(_).map(Release(_, _)))

  private val lines: Gen[Any, List[Stamp]] =
    Gen
      .setOfBounded(1, 3)(number)
      .flatMap: numbers =>
        Gen.listOfN(numbers.size)(Gen.int(1, 8)).map(numbers.toList.zip(_).map(Stamp(_, _)))
end ShimUpdateLawSpec
