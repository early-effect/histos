package histos.publish

import histos.cache.ContentStore
import histos.central.Publisher
import histos.jdk.{JdkRuntime, JdkVersion, Sha256, VersionError as JdkVersionError}
import histos.resolve.Resolve
import histos.select.SelectError
import histos.toolchain.RuntimeIdentity
import histos.version.{CiVersion, CommitSha, Refusal, ShipName, SnapshotCoordinate, SnapshotPointer, VersionError}
import histos.`scala`
import histos.{
  Dep,
  Jvm,
  LocalPublish as HistosLocal,
  MainClass,
  ProjectGraph,
  ProjectName,
  Projects,
  Splits,
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
import java.util.zip.ZipFile

import _root_.scala.jdk.CollectionConverters.*

import slayer.stubbed

import zio.{Chunk, Clock, Scope, ZIO}
import zio.test.*

object PublishLawSpec extends ZIOSpecDefault:
  private val number  = ship"1.2.3"
  private val scala3  = scala"3.10.0"
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
          val written  = DistChannel.files(name, revision, Chunk.single(1.toByte))
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
        val app = module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")
        val cli = module("cli", Jvm)(scala3).assembly(MainClass("demo.Cli")).asset(asset"dist/cli.jar")
        val one = shipped(app)
        val two = shipped(app, cli)
        (one, two, ProjectName("app"), ShipName("demo")) match
          case (Right(single), Right(both), moduleName, ship) =>
            val commit = published(moduleName, ship, stored.render)
            val moved  = published(moduleName, ship, pointer.render)
            val again  = published(moduleName, ship, pointer.render)
            val cliHit = published(ProjectName("cli"), ship, pointer.render)
            assertTrue(
              DistChannel.plan(single, List(commit)).exists(_.map(_.revision) == List(stored.render)),
              DistChannel.plan(single, List(moved)).exists(_.map(_.revision) == List(pointer.render)),
              DistChannel.plan(single, List(commit, moved)).exists(
                _.map(_.revision) == List(stored.render, pointer.render)
              ),
              !DistChannel.plan(single, List(commit)).exists(_.exists(_.revision == pointer.render)),
              DistChannel.plan(single, List(moved, again)) ==
                Left(LocalPublishError.Stock(s"demo app published more than one ${pointer.render}")),
              DistChannel.plan(both, List(moved, cliHit)) == Left(LocalPublishError.Stock("demo names two assets")),
              DistChannel.plan(both, List(commit)) == Left(LocalPublishError.Stock("demo names two assets")),
            )
          case _ => assertTrue(one.isRight, two.isRight)
      ,
      test("a dist bundle names the jar, the clients, and the sources under that revision"):
        val revision = s"${number.render}-20261106.153000-1"
        val jar      = Array[Byte](1, 2)
        val sources  = Array[Byte](3)
        Dist.clientPlatforms match
          case platform :: _ =>
            val client = Array[Byte](4)
            val files  = Dist.bundle(revision, jar, List(platform -> client), sources).toMap
            val text = files.get(DistLayout.manifest(revision)).map(bytes => new String(bytes, StandardCharsets.UTF_8))
            assertTrue(
              files.get(DistLayout.jar(revision)).exists(_.sameElements(jar)),
              files.get(DistLayout.sources(revision)).exists(_.sameElements(sources)),
              files.get(DistLayout.client(revision, platform)).exists(_.sameElements(client)),
              text.exists(body =>
                body.contains(s""""version": "$revision"""") &&
                  body.contains(DistLayout.sha256(jar)) &&
                  body.contains(DistLayout.sha256(sources)) &&
                  body.contains(platform)
              ),
            )
          case Nil => assertTrue(Dist.clientPlatforms.nonEmpty)
      ,
      test("a snapshot coordinate is a revision that ends in -SNAPSHOT"):
        check(Gen.string(Gen.alphaNumericChar)): raw =>
          assertTrue(
            PublishSelect.snapshotCoordinate(s"$raw-SNAPSHOT"),
            !PublishSelect.snapshotCoordinate(s"${raw.stripSuffix("-SNAPSHOT")}-release"),
          )
      ,
      test("a ship token wins, a snapshot dependency is refused, and an assembly may pack an unpublished module"):
        Dep.from("com.acme:widgets:1.0.0-SNAPSHOT") match
          case Left(_)    => assertTrue(false)
          case Right(dep) =>
            val extra = module("extra", Jvm)(scala3)
            val lib   = module("lib", Jvm)(scala3)
            val app   = module("app", Jvm)(scala3).dependsOn(lib).libraryDependencies(dep)
            val fat   = module("fat", Jvm)(scala3).dependsOn(extra).assembly(MainClass("demo.Main"))
            val plain = module("plain", Jvm)(scala3).dependsOn(extra)
            val built = Projects.organization(organization"example.com")(
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
            built match
              case Left(_)      => assertTrue(built.isRight)
              case Right(graph) =>
                val demo    = ShipName("demo")
                val widgets = ShipName("widgets")
                val writing = List(
                  ShipWrite(demo, List(ProjectName("app"), ProjectName("fat"), ProjectName("plain")), false),
                  ShipWrite(widgets, List(ProjectName("lib")), true),
                )
                val library = s"${dep.organization}:${dep.name}:${dep.revision}"
                val named   = module("demo", Jvm)(scala3)
                val same    = Projects.organization(organization"example.com")(
                  ships(Ship("demo").next(1, 2, 3)(named)),
                  named,
                )
                assertTrue(
                  same.flatMap(PublishSelect.resolve(_, List("demo"))) == Right(List(PublishName.Ship(demo))),
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
                    .exists(
                      _.modules == Set(ProjectName("fat"))
                    ),
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
      test("a bundle path stays inside the archive, and a size past the cap is refused before the body is read"):
        assertTrue(
          BundlePath.from("note.txt").exists(_.name == "note.txt"),
          BundlePath.from("..").isLeft,
          BundlePath.from("").isLeft,
          BundlePath.from("/abs").isLeft,
          BundlePath.from("a/../b").isLeft,
        )
      ,
      test("a launcher pin is a pointer, one timestamp, or a release"):
        val stamp = s"${number.render}-20261106.153000-1"
        (LauncherPin.from(pointer.render), LauncherPin.from(stamp), LauncherPin.from(number.render)) match
          case (Right(channel), Right(revision), Right(release)) =>
            assertTrue(
              channel.choice == LauncherChoice.Pointer(pointer.render),
              revision.choice == LauncherChoice.Revision(pointer.render, stamp),
              release.choice == LauncherChoice.Release(number.render),
              LauncherPin.parse(LauncherPin.render(revision)).exists(_.choice == revision.choice),
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
          case _ => assertTrue(false)
      ,
      test("a notice is quiet in CI, remembered inside a day, and it names a newer release"):
        val pin       = number
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
            UpgradeNotice.newer(published, pin),
            !UpgradeNotice.newer(pin, published),
            UpgradeNotice.text(pin, published).contains(published.render),
            UpgradeNotice.text(published, pin).isEmpty,
            UpgradeNotice.parse(UpgradeNotice.render(record)) == Some(record),
            UpgradeNotice.newest("<version>1.2.3</version><version>1.3.0</version><version>nope</version>") ==
              Some(published),
          ),
    ),
    suite("a publish that must not compile")(
      test("an empty catalog publishes nothing"):
        refuse(Projects(module("core", Jvm)(scala3)), Map.empty, None, PublishWrite.Local) { found =>
          assertTrue(found == Left(LocalPublishError.NothingToPublish))
        }
      ,
      test("a development session is not a release"):
        refuse(demo(module("app", Jvm)(scala3)), Map.empty, None, PublishWrite.Release) {
          case Left(LocalPublishError.Development(ship, compile)) =>
            assertTrue(ship == ShipName("demo"), compile == CiVersion(number))
          case other => assertTrue(other == Left(LocalPublishError.NothingToPublish))
        }
      ,
      test("a directory with no commit does not publish snapshots"):
        refuse(demo(module("app", Jvm)(scala3)), Map.empty, None, PublishWrite.Snapshots) { found =>
          assertTrue(found == Left(LocalPublishError.NoCommit))
        }
      ,
      test("a snapshot session without a registry is refused"):
        val session = Map(
          "HISTOS_SESSION" -> "snapshot",
          "HISTOS_SHA"     -> sha.value,
          "HISTOS_BRANCH"  -> "default",
        )
        refuse(demo(module("app", Jvm)(scala3)), session, None, PublishWrite.Snapshots) {
          case Left(LocalPublishError.Version(VersionError.Refused(refusals))) =>
            assertTrue(refusals == ::(Refusal.Unchecked, Nil))
          case other => assertTrue(other == Left(LocalPublishError.NothingToPublish))
        }
      ,
      test("a release of a snapshot coordinate writes nothing"):
        val session = Map(
          "HISTOS_SESSION" -> "snapshot",
          "HISTOS_SHA"     -> sha.value,
          "HISTOS_BRANCH"  -> "default",
        )
        checked(demo(module("app", Jvm)(scala3)), session, Some(""), PublishWrite.Release) { output =>
          assertTrue(
            output.modules.isEmpty,
            output.refused.contains(s"demo is ${stored.render}, which is not a release"),
          )
        }
      ,
      test("a release of a ship that already matches is refused"):
        val session = Map("HISTOS_SESSION" -> "release")
        refuse(demo(module("app", Jvm)(scala3)), session, Some("demo matches\n"), PublishWrite.Release) {
          case Left(LocalPublishError.Version(VersionError.Refused(refusals))) =>
            assertTrue(refusals == ::(Refusal.NothingToRelease, Nil))
          case other => assertTrue(other == Left(LocalPublishError.NothingToPublish))
        }
      ,
      test("a missing asset file is refused before credentials, and a present file still needs a token"):
        val app = module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")
        demo(app) match
          case Left(_)      => ZIO.succeed(assertTrue(false))
          case Right(graph) =>
            DistChannel.plan(graph, List(published(ProjectName("app"), ShipName("demo"), pointer.render))) match
              case Left(_)      => ZIO.succeed(assertTrue(false))
              case Right(plans) =>
                ZIO.acquireRelease(directory("histos-ready"))(delete).flatMap { repo =>
                  val missing =
                    DistChannel.ready(repo, Map("GITHUB_TOKEN" -> "t", "GITHUB_REPOSITORY" -> "example/demo"), plans)
                  val clock = Clock.instant
                  val file  = repo.resolve("dist").resolve("app.jar")
                  ZIO
                    .attemptBlocking {
                      Option(file.getParent).foreach(Files.createDirectories(_))
                      Files.write(file, Array[Byte](1))
                    }
                    .orDie
                    .zipRight(clock)
                    .as {
                      val bare = DistChannel.ready(repo, Map.empty, plans)
                      val held = DistChannel.ready(
                        repo,
                        Map("GITHUB_TOKEN" -> "token", "GITHUB_REPOSITORY" -> "example/demo"),
                        plans,
                      )
                      assertTrue(
                        missing == Left(
                          LocalPublishError
                            .Stock("dist/app.jar is not a file. ./histos assembly writes the install jar.")
                        ),
                        bare == Left(LocalPublishError.Stock("GITHUB_TOKEN and GITHUB_REPOSITORY are required")),
                        held == Right(()),
                      )
                    }
                }
      ,
      test("a bundle over the cap is refused before its body is read, and a written entry is timestamped at zero"):
        ZIO.acquireRelease(directory("histos-bundle"))(delete).flatMap { root =>
          val dest       = root.resolve("out.zip")
          val stage      = root.resolve("stage")
          val emptyStage = root.resolve("empty-stage")
          val packedPath = root.resolve("packed.zip")
          val inside     = stage.resolve("inside.zip")
          val missing    = root.resolve("missing")
          zio.Ref.make(false).flatMap { reads =>
            val body = reads.set(true).as(Chunk.fromArray("no".getBytes(StandardCharsets.UTF_8)))
            BundlePath.from("note.txt") match
              case Left(_)     => ZIO.succeed(assertTrue(false))
              case Right(path) =>
                val over = Staged(path, Bundle.Cap + 1, body)
                val half = Bundle.Cap / 2 + 1
                val one  = ZIO.succeed(Chunk.single(1.toByte))
                for
                  _         <- Clock.instant
                  tooLarge  <- Bundle.zip(Chunk(over), dest).either
                  summed    <- Bundle.zip(Chunk(over.copy(bytes = half), Staged(path, half, body)), dest).either
                  empty     <- Bundle.zip(Chunk.empty, dest).either
                  sized     <- Bundle.zip(Chunk(over.copy(bytes = -1)), dest).either
                  duplicate <- Bundle.zip(Chunk(Staged(path, 1, one), Staged(path, 1, one)), dest).either
                  _         <- ZIO.attemptBlocking {
                    Files.createDirectories(stage)
                    Files.createDirectories(emptyStage)
                    Files.writeString(stage.resolve("note.txt"), "hi")
                  }.orDie
                  zipped <- Bundle.zip(stage, packedPath).either
                  packed <- zipped match
                    case Left(_)  => ZIO.succeed(None)
                    case Right(_) => entry(packedPath, "note.txt")
                  contained <- Bundle.zip(stage, inside).either
                  absent    <- Bundle.zip(missing, dest).either
                  bare      <- Bundle.zip(emptyStage, dest).either
                  read      <- reads.get
                yield assertTrue(
                  tooLarge == Left(BundleError.TooLarge(Bundle.Cap + 1)),
                  summed == Left(BundleError.TooLarge(half + half)),
                  empty == Left(BundleError.Empty),
                  sized == Left(BundleError.Size(-1)),
                  duplicate == Left(BundleError.Duplicate("note.txt")),
                  zipped == Right(()),
                  packed.exists { case (time, bytes) =>
                    time == 0L && new String(bytes, StandardCharsets.UTF_8) == "hi"
                  },
                  contained == Left(BundleError.Contained(inside.toString)),
                  absent == Left(BundleError.Missing(missing.toString)),
                  bare == Left(BundleError.Empty),
                  !read,
                )
          }
        }
      ,
      test("latest rewrites a timestamp back to its pointer and leaves a release"):
        ZIO.acquireRelease(directory("histos-pin"))(delete).flatMap { repo =>
          val stamp = s"${number.render}-20261106.153000-1"
          val clock = Clock.instant
          val file  = repo.resolve(LauncherPin.file)
          for
            revision <- ZIO.fromEither(LauncherPin.from(stamp))
            release  <- ZIO.fromEither(LauncherPin.from(number.render))
            _        <- clock
            _        <- ZIO.attemptBlocking(Files.writeString(file, LauncherPin.render(revision))).orDie
            moved    <- LauncherPin.latest(repo)
            text     <- ZIO.attemptBlocking(Files.readString(file)).orDie
            _        <- ZIO.attemptBlocking(Files.writeString(file, LauncherPin.render(release))).orDie
            stayed   <- LauncherPin.latest(repo)
          yield assertTrue(
            moved.choice == LauncherChoice.Pointer(pointer.render),
            text == LauncherPin.render(moved),
            !text.contains(stamp),
            stayed.choice == LauncherChoice.Release(number.render),
          )
        }
      ,
      test("a notice fetches only through the body it was given"):
        ZIO.acquireRelease(directory("histos-notice"))(delete).flatMap { repo =>
          val home = repo.resolve("home")
          val pin  = repo.resolve(LauncherPin.file)
          val xml  = "<version>1.2.3</version><version>1.3.0</version>"
          val at   = Instant.parse("2026-11-06T00:00:00Z")
          for
            _          <- TestClock.setTime(at)
            _          <- TestSystem.putEnv("HISTOS_HOME", home.toString)
            _          <- TestSystem.putProperty("user.home", home.toString)
            _          <- ZIO.attemptBlocking(Files.writeString(pin, s"histos.version=${number.render}\n")).orDie
            fetched    <- UpgradeNotice.line(repo, ZIO.succeed(Some(xml)))
            _          <- TestSystem.putEnv("CI", "1")
            quiet      <- UpgradeNotice.line(repo, ZIO.die(new IllegalStateException("fetched")))
            _          <- TestSystem.putEnv("CI", "")
            remembered <- UpgradeNotice.line(repo, ZIO.die(new IllegalStateException("fetched")))
          yield assertTrue(
            fetched.contains(ship"1.3.0".render),
            fetched.contains(number.render),
            quiet.isEmpty,
            remembered.contains(ship"1.3.0".render),
          )
        },
    ).provideSomeLayerShared[TestEnvironment & Scope](stock) @@ TestAspect.sequential,
  )

  private def refuse(
      built: Either[histos.BuildError, ProjectGraph],
      env: Map[String, String],
      releases: Option[String],
      write: PublishWrite,
  )(
      claim: Either[LocalPublishError, PublishOutput] => TestResult
  ): ZIO[Resolve & Publisher & ContentStore & Scope, Nothing, TestResult] =
    built match
      case Left(_)      => ZIO.succeed(assertTrue(built.isRight))
      case Right(graph) =>
        jdk match
          case Left(_)        => ZIO.succeed(assertTrue(false))
          case Right(runtime) =>
            ZIO.acquireRelease(directory("histos-publish"))(delete).flatMap { repo =>
              registry(repo, releases) *>
                Clock.instant *>
                LocalPublish.run(repo, graph, Nil, runtime, env, write).either.map(claim)
            }

  private def checked(
      built: Either[histos.BuildError, ProjectGraph],
      env: Map[String, String],
      releases: Option[String],
      write: PublishWrite,
  )(claim: PublishOutput => TestResult): ZIO[Resolve & Publisher & ContentStore & Scope, LocalPublishError, TestResult] =
    built match
      case Left(_)      => ZIO.succeed(assertTrue(built.isRight))
      case Right(graph) =>
        jdk match
          case Left(_)        => ZIO.succeed(assertTrue(false))
          case Right(runtime) =>
            ZIO.acquireRelease(directory("histos-publish"))(delete).flatMap { repo =>
              registry(repo, releases) *>
                Clock.instant *>
                LocalPublish.run(repo, graph, Nil, runtime, env, write).map(claim)
            }

  private def registry(repo: Path, releases: Option[String]): ZIO[Any, Nothing, Unit] =
    releases match
      case None       => ZIO.unit
      case Some(text) =>
        ZIO.attemptBlocking {
          Files.writeString(repo.resolve("releases"), text)
          ()
        }.orDie

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
      Sha256.of(Chunk.empty),
      None,
      Nil,
    )

  private def demo(member: histos.ProjectDef): Either[histos.BuildError, ProjectGraph] =
    shipped(member)

  private def shipped(first: histos.ProjectDef, rest: histos.ProjectDef*): Either[histos.BuildError, ProjectGraph] =
    val org  = Projects.organization(organization"example.com")
    val spec = ships(Ship("demo").next(1, 2, 3)(first, rest*))
    rest.toList match
      case Nil           => org(spec, first)
      case second :: Nil => org(spec, first, second)
      case _             =>
        Projects.assemble(Some(organization"example.com"), Nil, Splits.aggregate, spec, first :: rest.toList)

  private def jdk: Either[JdkVersionError, JdkRuntime] =
    JdkVersion
      .from("21")
      .map: version =>
        JdkRuntime(Path.of("jdk"), version, RuntimeIdentity.Local(Path.of("jdk"), Sha256.of(Chunk.empty)))

  private def directory(prefix: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie

  private def entry(path: Path, name: String): ZIO[Any, Nothing, Option[(Long, Array[Byte])]] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(path.toFile)
      try
        Option(zip.getEntry(name)).map { item =>
          val in = zip.getInputStream(item)
          try (item.getTime, in.readAllBytes)
          finally in.close()
        }
      finally zip.close()
    }.orDie

  private def delete(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.orDie

end PublishLawSpec
