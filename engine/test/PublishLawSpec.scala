package histos.publish

import histos.cache.ContentStore
import histos.central.{MavenModule, Publisher, ReleaseFeed}
import histos.jdk.{JdkRuntime, JdkVersion, Sha256Digest}
import histos.plugin.ApiDigest
import histos.resolve.Resolve
import histos.select.SelectError
import histos.toolchain.RuntimeIdentity
import histos.version.{
  CiVersion,
  CommitSha,
  PinLag,
  Refusal,
  ShipName,
  ShipNumber,
  SnapshotCoordinate,
  SnapshotPointer,
  VersionError,
}
import histos.workspace.Workspace
import histos.`scala`
import histos.{
  Dep,
  Jvm,
  LocalPublish as HistosLocal,
  MainClass,
  ProjectGraph,
  ProjectName,
  Projects,
  Publishing,
  Ship,
  SonaPublish,
  asset,
  module,
  organization,
  ship,
  ships,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

import slayer.{any, stub, stubbed}

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object PublishLawSpec extends ZIOSpecDefault:
  private val number  = ship"1.2.3"
  private val scala3  = scala"3.10.0"
  private val hostApi = ApiDigest.trusted("ab" * 32)
  private val sha     = CommitSha("0123456789abcdef0123456789abcdef01234567")
  private val stored  = SnapshotCoordinate(number, sha.abbreviate)
  private val pointer = SnapshotPointer(number)

  private val stock =
    stubbed[Signer] ++
      stubbed[Assets] ++
      stubbed[Publishing] ++
      stubbed[SonaPublish] ++
      stubbed[HistosLocal] ++
      stubbed[Resolve] ++
      stubbed[ContentStore] ++
      stubbed[Publisher]

  def spec = suite("publish laws")(
    suite("a publish value")(
      test("a channel file is one body, and that body is the revision"):
        check(Gen.string(Gen.alphaNumericChar), Gen.string(Gen.alphaNumericChar)): (name, revision) =>
          val written  = DistChannel.files(name, revision, Chunk.single(1.toByte), Nil)
          val channels = written.filter { case (file, _) => file.startsWith("channel-") }
          val body     = Chunk.fromArray((revision + "\n").getBytes(StandardCharsets.UTF_8))
          assertTrue(
            channels.length == 1,
            channels.exists { case (file, bytes) => file == DistLayout.channel(name) && bytes == body },
            written.exists { case (file, _) => file == DistLayout.manifest(revision) },
            written.exists { case (file, bytes) => file == DistLayout.jar(revision) && bytes == Chunk.single(1.toByte) },
          )
      ,
      test("a timestamp is a channel revision, and a stored commit snapshot is not a pointer timestamp"):
        val stamp    = s"${number.render}-20261106.153000-1"
        val shaStamp = s"${number.render}-${sha.abbreviate.value}-20261106.153000-1"
        val bad      = s"${number.render}-20261306.153000-1"
        assertTrue(
          LauncherPin.validRevision(pointer.render, stamp),
          LauncherPin.validRevision(stored.render, shaStamp),
          !LauncherPin.validRevision(pointer.render, stored.render),
          !LauncherPin.validRevision(pointer.render, shaStamp),
          !LauncherPin.validRevision(pointer.render, bad),
        )
      ,
      test("a stored commit snapshot has its own channel, and two assets are refused"):
        val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")
        val cli    = module("cli", Jvm)(scala3).assembly(MainClass("demo.Cli")).asset(asset"dist/cli.jar")
        val commit = published(ProjectName("app"), ShipName("demo"), stored.render)
        val moved  = published(ProjectName("app"), ShipName("demo"), pointer.render)
        val cliHit = published(ProjectName("cli"), ShipName("demo"), pointer.render)
        for
          single <- ZIO.fromEither(shipped(app))
          both   <- ZIO.fromEither(shipped(app, cli))
        yield assertTrue(
          DistChannel.plan(single, List(commit)).map(_.map(_.revision)) == Right(List(stored.render)),
          DistChannel.plan(single, List(moved)).map(_.map(_.revision)) == Right(List(pointer.render)),
          DistChannel.plan(single, List(commit, moved)).map(_.map(_.revision)) ==
            Right(List(stored.render, pointer.render)),
          DistChannel.plan(single, List(moved, moved)) ==
            Left(LocalPublishError.Stock(s"demo app published more than one ${pointer.render}")),
          DistChannel.plan(both, List(moved, cliHit)) == Left(LocalPublishError.Stock("demo names two assets")),
          DistChannel.plan(both, List(commit)) == Left(LocalPublishError.Stock("demo names two assets")),
        )
      ,
      test("a ship token wins, a snapshot dependency is refused, and an assembly may pack an unpublished module"):
        val demo    = ShipName("demo")
        val widgets = ShipName("widgets")
        val writing = List(
          ShipWrite(demo, List(ProjectName("app"), ProjectName("fat"), ProjectName("plain")), false),
          ShipWrite(widgets, List(ProjectName("lib")), true),
        )
        val named = module("demo", Jvm)(scala3)
        for
          dep <- ZIO.fromEither(Dep.from("com.acme:widgets:1.0.0-SNAPSHOT"))
          extra = module("extra", Jvm)(scala3)
          lib   = module("lib", Jvm)(scala3)
          app   = module("app", Jvm)(scala3).dependsOn(lib).libraryDependencies(dep)
          fat   = module("fat", Jvm)(scala3).dependsOn(extra).assembly(MainClass("demo.Main"))
          plain = module("plain", Jvm)(scala3).dependsOn(extra)
          graph <- ZIO.fromEither(
            Projects.organization(organization"example.com")(
              ships(
                Ship("demo").next(1, 2, 3)(app, fat, plain),
                Ship("widgets").next(1, 2, 3)(lib),
              ),
              app,
              lib,
              extra,
              fat,
              plain,
            )
          )
          same <- ZIO.fromEither(
            Projects.organization(organization"example.com")(ships(Ship("demo").next(1, 2, 3)(named)), named)
          )
          library = s"${dep.organization}:${dep.name}:${dep.revision}"
        yield assertTrue(
          PublishSelect.resolve(same, List("demo")) == Right(List(PublishName.Ship(demo))),
          PublishSelect.resolve(graph, List("missing")) == Left(LocalPublishError.Unknown("missing")),
          PublishSelect.resolve(graph, List("app", "app")) ==
            Left(LocalPublishError.Target(SelectError.Duplicate(ProjectName("app")))),
          PublishSelect.pick(graph, List(PublishName.Module(ProjectName("app"))), writing).exists { picked =>
            picked.roots.isEmpty && picked.modules.isEmpty &&
            picked.refused == List(LocalPublishError.SnapshotDependency(demo, library))
          },
          PublishSelect.pick(graph, List(PublishName.Module(ProjectName("extra"))), writing) ==
            Left(LocalPublishError.NotPublished(ProjectName("extra"))),
          PublishSelect.pick(graph, List(PublishName.Ship(ShipName("other"))), writing) ==
            Left(LocalPublishError.UnknownShip(ShipName("other"))),
          PublishSelect.pick(graph, List(PublishName.Module(ProjectName("plain"))), writing) ==
            Left(LocalPublishError.UnpublishedDependency(ProjectName("plain"), ProjectName("extra"))),
          PublishSelect.pick(graph, List(PublishName.Module(ProjectName("fat"))), writing).exists { picked =>
            picked.modules == Set(ProjectName("fat")) && picked.refused.isEmpty
          },
          PublishSelect
            .pick(graph, Nil, List(ShipWrite(demo, List(ProjectName("fat")), false)))
            .exists(_.modules == Set(ProjectName("fat"))),
        )
      ,
      test("snapshot credentials are a user and a password, or they are refused"):
        assertTrue(
          PublishWhere.snapshots(Map.empty) == Left(LocalPublishError.Credentials),
          PublishWhere.snapshots(Map("SONATYPE_USERNAME" -> " ", "SONATYPE_PASSWORD" -> "pw")) ==
            Left(LocalPublishError.Credentials),
          PublishWhere.snapshots(Map("SONATYPE_USERNAME" -> "a:b", "SONATYPE_PASSWORD" -> "pw")) ==
            Left(LocalPublishError.Credentials),
          PublishWhere.snapshots(Map("SONATYPE_USERNAME" -> "user", "SONATYPE_PASSWORD" -> "")) ==
            Left(LocalPublishError.Credentials),
          PublishWhere.snapshots(Map("SONATYPE_USERNAME" -> "user", "SONATYPE_PASSWORD" -> "pw")).exists {
            case PublishWhere.Snapshots(user, _) => user.value == "user"
          },
        )
      ,
      test("a launcher pin is a pointer, one timestamp, or a release"):
        val stamp = s"${number.render}-20261106.153000-1"
        for
          channel  <- ZIO.fromEither(LauncherPin.from(pointer.render))
          revision <- ZIO.fromEither(LauncherPin.from(stamp))
          release  <- ZIO.fromEither(LauncherPin.from(number.render))
        yield assertTrue(
          channel.choice == LauncherChoice.Pointer(LauncherChannel.Line(pointer)),
          revision.choice == LauncherChoice.Revision(LauncherChannel.Line(pointer), stamp),
          release.choice == LauncherChoice.Release(number),
          LauncherPin.parse(LauncherPin.render(revision)).map(_.choice) == Right(revision.choice),
          LauncherPin.parse("") == Left(LauncherError.Missing),
          LauncherPin.parse(s"${LauncherPin.Key}=${pointer.render}\n${LauncherPin.Key}=again\n") ==
            Left(LauncherError.Duplicate),
          LauncherPin.parse("other=1\n") == Left(LauncherError.Unknown("other")),
          LauncherPin.from("not a version") == Left(LauncherError.Version("not a version")),
          LauncherPin.renderBase("https://example.test/maven") == Right("https://example.test/maven"),
          LauncherPin.renderBase("http://127.0.0.1:9/maven") == Right("http://127.0.0.1:9/maven"),
          LauncherPin.renderBase("http://localhost:9/maven") == Right("http://localhost:9/maven"),
          LauncherPin.renderBase("file:///tmp/repo") == Right("file:///tmp/repo"),
          LauncherPin.renderBase("http://example.test/maven").isLeft,
          LauncherPin.renderBase("https://example.test/a b").isLeft,
        )
      ,
      test("a notice is quiet in CI and remembered inside a day"):
        val published = ship"1.3.0"
        val at        = Instant.parse("2026-11-06T00:00:00Z")
        val record    = UpgradeNotice.Record(at, Some(published))
        check(Gen.long(-86_400_000L, 172_800_000L)): millis =>
          val now   = at.plusMillis(millis)
          val step  = UpgradeNotice.step(Map.empty, now, Some(record))
          val fresh = millis >= 0 && millis < UpgradeNotice.Window.toMillis
          assertTrue(
            step == (if fresh then UpgradeNotice.Step.Remembered else UpgradeNotice.Step.Fetch),
            UpgradeNotice.step(Map("CI" -> "1"), now, None) == UpgradeNotice.Step.Quiet,
            UpgradeNotice.inCi(Map("CI" -> "yes")),
            UpgradeNotice.inCi(Map("GITHUB_ACTIONS" -> "true")),
            !UpgradeNotice.inCi(Map("CI" -> "no")),
            UpgradeNotice.parse(UpgradeNotice.render(record)) == Some(record),
            ReleaseFeed.newest("<version>1.2.3</version><version>1.3.0</version><version>nope</version>") ==
              Some(published),
          )
      ,
      test("a release pin trails a newer release, and a snapshot pin trails a release of its line or later"):
        val part    = Gen.int(0, 3)
        val numbers = (part <*> part <*> part).map((major, minor, patch) => ShipNumber.trusted(major, minor, patch))
        check(numbers, numbers): (pinned, published) =>
          val order    = Ordering[ShipNumber]
          val release  = LauncherChoice.Release(pinned)
          val snapshot = LauncherChoice.Pointer(LauncherChannel.Line(SnapshotPointer(pinned)))
          val stamp    = LauncherChoice.Revision(
            LauncherChannel.Line(SnapshotPointer(pinned)),
            s"${pinned.render}-20261106.153000-1",
          )
          val lineLag = Option.when(order.gteq(published, pinned))(PinLag.LineReleased(pinned, published))
          assertTrue(
            release.lag(published) == Option.when(order.gt(published, pinned))(PinLag.NewerRelease(pinned, published)),
            snapshot.lag(published) == lineLag,
            stamp.lag(published) == lineLag,
            release.lag(published).forall(_.command.value == "histos update"),
            snapshot.lag(published).forall(_.command.value == "histos update --stable"),
          ),
    ),
    suite("a publish that must not compile")(
      test("an empty catalog publishes nothing"):
        attempt(Projects(module("core", Jvm)(scala3)), Map.empty, None, PublishWrite.Local).map: found =>
          assertTrue(found == Left(LocalPublishError.NothingToPublish))
      ,
      test("a development session is not a release"):
        attempt(demo(module("app", Jvm)(scala3)), Map.empty, None, PublishWrite.Release).map: found =>
          assertTrue(found == Left(LocalPublishError.Development(ShipName("demo"), CiVersion(number))))
      ,
      test("a directory with no commit does not publish snapshots"):
        attempt(demo(module("app", Jvm)(scala3)), Map.empty, None, PublishWrite.Snapshots).map: found =>
          assertTrue(found == Left(LocalPublishError.NoCommit))
      ,
      test("a snapshot session without a registry is refused"):
        val session = Map(
          "HISTOS_SESSION" -> "snapshot",
          "HISTOS_SHA"     -> sha.value,
          "HISTOS_BRANCH"  -> "default",
        )
        attempt(demo(module("app", Jvm)(scala3)), session, None, PublishWrite.Snapshots).map: found =>
          assertTrue(found == Left(LocalPublishError.Version(VersionError.Refused(::(Refusal.Unchecked, Nil)))))
      ,
      test("a release of a snapshot coordinate writes nothing"):
        val session = Map(
          "HISTOS_SESSION" -> "snapshot",
          "HISTOS_SHA"     -> sha.value,
          "HISTOS_BRANCH"  -> "default",
        )
        attempt(demo(module("app", Jvm)(scala3)), session, Some(""), PublishWrite.Release).map: found =>
          assertTrue(
            found.map(_.modules) == Right(Nil),
            found.map(_.refused) == Right(List(s"demo is ${stored.render}, which is not a release")),
          )
      ,
      test("a release of a ship that already matches is refused"):
        val session = Map("HISTOS_SESSION" -> "release")
        attempt(demo(module("app", Jvm)(scala3)), session, Some("demo matches\n"), PublishWrite.Release).map: found =>
          assertTrue(found == Left(LocalPublishError.Version(VersionError.Refused(::(Refusal.NothingToRelease, Nil)))))
      ,
      test("a missing asset file is refused before credentials, and a present file still needs a token"):
        val app   = module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")
        val token = Map("GITHUB_TOKEN" -> "token", "GITHUB_REPOSITORY" -> "example/demo")
        for
          graph <- ZIO.fromEither(demo(app))
          plans <- ZIO.fromEither(
            DistChannel.plan(graph, List(published(ProjectName("app"), ShipName("demo"), pointer.render)))
          )
          repo <- Workspace.work("histos-ready")
          missing = DistChannel.ready(repo, token, plans)
          _ <- ZIO.attemptBlocking {
            val file = repo.resolve("dist").resolve("app.jar")
            Option(file.getParent).foreach(Files.createDirectories(_))
            Files.write(file, Array[Byte](1))
          }.orDie
        yield assertTrue(
          missing == Left(
            LocalPublishError.Stock("dist/app.jar is not a file. ./histos assembly writes the install jar.")
          ),
          DistChannel.ready(repo, Map.empty, plans) ==
            Left(LocalPublishError.Stock("GITHUB_TOKEN and GITHUB_REPOSITORY are required")),
          DistChannel.ready(repo, token, plans) == Right(()),
        )
      ,
      test("latest rewrites a timestamp back to its pointer and leaves a release"):
        val stamp = s"${number.render}-20261106.153000-1"
        for
          repo     <- Workspace.work("histos-pin")
          revision <- ZIO.fromEither(LauncherPin.from(stamp))
          release  <- ZIO.fromEither(LauncherPin.from(number.render))
          file = repo.resolve(LauncherPin.file)
          _      <- Workspace.writeString(file, LauncherPin.render(revision))
          moved  <- LauncherPin.latest(repo)
          text   <- Workspace.readString(file)
          _      <- Workspace.writeString(file, LauncherPin.render(release))
          stayed <- LauncherPin.latest(repo)
        yield assertTrue(
          moved.choice == LauncherChoice.Pointer(LauncherChannel.Line(pointer)),
          text == LauncherPin.render(moved),
          !text.contains(stamp),
          stayed.choice == LauncherChoice.Release(number),
        )
      ,
      test("a notice asks the feed once, is quiet in CI, and remembers the answer inside a day"):
        val newer   = ship"1.3.0"
        val program =
          for
            repo <- Workspace.work("histos-notice")
            home = repo.resolve("home")
            pin  = repo.resolve(LauncherPin.file)
            at   = Instant.parse("2026-11-06T00:00:00Z")
            gone = ZIO.die(new IllegalStateException("fetched"))
            _          <- TestClock.setTime(at)
            _          <- TestSystem.putEnv("HISTOS_HOME", home.toString)
            _          <- TestSystem.putProperty("user.home", home.toString)
            _          <- Workspace.writeString(pin, s"histos.version=${number.render}\n")
            _          <- stub[ReleaseFeed](_.newest(any[MavenModule]))(ZIO.succeed(Some(newer)))
            fetched    <- UpgradeNotice.advice(repo)
            _          <- stub[ReleaseFeed](_.newest(any[MavenModule]))(gone)
            _          <- TestSystem.putEnv("CI", "1")
            quiet      <- UpgradeNotice.advice(repo)
            _          <- TestSystem.putEnv("CI", "")
            remembered <- UpgradeNotice.advice(repo)
          yield assertTrue(
            fetched == Some(PinLag.NewerRelease(number, newer)),
            quiet.isEmpty,
            remembered == fetched,
          )
        program.provideSomeLayer[TestEnvironment & Scope](stubbed[ReleaseFeed]),
    ).provideSomeLayerShared[TestEnvironment & Scope](stock) @@ TestAspect.sequential,
  )

  /** `LocalPublish.run` on a fresh directory, with `releases` written when it is set. */
  private def attempt(
      built: Either[histos.BuildError, ProjectGraph],
      env: Map[String, String],
      releases: Option[String],
      write: PublishWrite,
  ) =
    for
      graph   <- ZIO.fromEither(built)
      runtime <- ZIO.fromEither(jdk)
      repo    <- Workspace.work("histos-publish")
      _       <- ZIO.foreachDiscard(releases)(text => Workspace.writeString(repo.resolve("releases"), text))
      found   <- LocalPublish.run(repo, graph, Nil, runtime, env, write, hostApi).either
    yield found

  private def published(module: ProjectName, ship: ShipName, revision: String): PublishedModule =
    PublishedModule(
      module,
      organization"example.com",
      module.value,
      revision,
      None,
      None,
      None,
      None,
      ship,
      Sha256Digest.of(Chunk.empty),
      None,
      Nil,
    )

  private def demo(member: histos.ProjectDef): Either[histos.BuildError, ProjectGraph] =
    shipped(member)

  private def shipped(first: histos.ProjectDef, rest: histos.ProjectDef*): Either[histos.BuildError, ProjectGraph] =
    val org  = Projects.organization(organization"example.com")
    val spec = ships(Ship("demo").next(1, 2, 3)(first, rest*))
    org(spec, (first :: rest.toList)*)

  private def jdk =
    JdkVersion
      .from("21")
      .map: version =>
        JdkRuntime(Path.of("jdk"), version, RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)))
end PublishLawSpec
