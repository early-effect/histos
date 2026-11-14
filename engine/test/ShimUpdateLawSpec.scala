package histos.publish

import histos.version.{ShipNumber, SnapshotPointer}
import histos.workspace.Workspace

import zio.ZIO
import zio.test.*

import ShimRig.*

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
            jar    <- rig.installedJar(newest.number.render)
          yield assertTrue(
            ran.exit == 0,
            choice == LauncherChoice.Release(newest.number),
            jar,
            origin == Option.when(newest.clients)("prebuilt"),
            ran.err.exists(clientless(newest.number.render, None)) == !newest.clients,
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
            first.err.size == 1,
            first.err.forall(clientless(newest.render, None)),
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
          jar    <- rig.installedJar(build021.render)
          origin <- rig.origin(build021.render)
        yield assertTrue(
          ran.exit == 0,
          choice == LauncherChoice.Pointer(build021.channel),
          jar,
          origin.isEmpty,
          ran.err.size == 2,
          ran.err.take(1) == List(s"histos ${release021.render} is a stable release. histos update --stable switches."),
          ran.err.drop(1).forall(clientless(build021.render, Some(release021.render))),
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
          ranExec <- rig.executed
        yield assertTrue(
          ran.exit == 0,
          default == LauncherChoice.Revision(build021.channel, build021.render),
          !pin,
          ranExec.isEmpty,
        )
    ,
    test("inside a project, update runs version on the new driver"):
      ZIO.scoped:
        val older = Release(line020, clients = true)
        val newer = Release(release021, clients = true)
        for
          rig    <- loopback
          _      <- rig.publish(Published(List(older, newer), Nil))
          _      <- rig.pin(line020.render)
          ran    <- rig.update()
          choice <- rig.choice
          exec   <- rig.executed
          jar = rig.toolchain(release021.render).resolve("histos.jar").toString
        yield assertTrue(
          ran.exit == 0,
          choice == LauncherChoice.Release(release021),
          exec.exists: hit =>
            hit.program.startsWith("client-") &&
              hit.version == s"histos ${release021.render}" &&
              hit.jar == jar &&
              hit.args == List("version"),
        )
    ,
    test("a local toolchain stays the driver while the pin moves"):
      ZIO.scoped:
        val older = Release(line020, clients = true)
        val newer = Release(release021, clients = true)
        for
          rig <- loopback
          _   <- rig.publish(Published(List(older, newer), Nil))
          _   <- rig.pin(line020.render)
          _   <- rig.install(line020.render)
          home = rig.project.resolve(".histos")
          _ <- Workspace.createDirectories(home).mapError(ShimFault.Io(_))
          _ <- Workspace
            .writeString(home.resolve("toolchain"), s"${line020.render}\n")
            .mapError(ShimFault.Io(_))
          ran    <- rig.update()
          choice <- rig.choice
          exec   <- rig.executed
          jar = rig.toolchain(line020.render).resolve("histos.jar").toString
        yield assertTrue(
          ran.exit == 0,
          choice == LauncherChoice.Release(release021),
          exec.exists: hit =>
            hit.program.startsWith("client-") &&
              hit.version == s"histos ${line020.render}" &&
              hit.jar == jar &&
              hit.args == List("version"),
        ),
  ) @@ TestAspect.withLiveSystem @@ TestAspect.samples(4)

  /** The line the shim prints when this platform has no client, naming the release that has one when `update` saw it.
    */
  private def clientless(version: String, release: Option[String])(line: String): Boolean =
    val hint = release.fold("")(number => s"; $number has one")
    line.startsWith(s"histos $version has no prebuilt client for ") &&
    line.endsWith(s"$hint. java -jar will run.")

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
