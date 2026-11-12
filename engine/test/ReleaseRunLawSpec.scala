package histos.publish

import histos.cache.ContentStore
import histos.central.{
  DeploymentId,
  DeploymentState,
  Fingerprint,
  PortalBase,
  ReleaseMode,
  RepositorySecret,
  RepositoryUser,
}
import histos.jdk.{JdkRuntime, JdkVersion, Sha256Digest}
import histos.plugin.ApiDigest
import histos.resolve.Resolve
import histos.toolchain.RuntimeIdentity
import histos.version.ShipName
import histos.workspace.Workspace
import histos.{Jvm, ProjectName, Projects, Ship, module, organization, ship, ships}
import histos.`scala`

import java.nio.file.{Files, Path}

import slayer.{any, stub, stubbed}

import zio.{Chunk, Config, Ref, Scope, UIO, ZIO}
import zio.test.*

/** A portal URL in these laws is a [[RefusingLoopback]], and each law that names one asserts nothing reached it. */
object ReleaseRunLawSpec extends ZIOSpecDefault:
  private val scala3   = scala"3.10.0"
  private val number   = ship"1.2.3"
  private val group    = organization"example.com"
  private val app      = module("app", Jvm)(scala3)
  private val creds    = Map("SONATYPE_USERNAME" -> "portal-user", "SONATYPE_PASSWORD" -> "not-for-the-note")
  private val insecure = creds + ("HISTOS_PORTAL" -> "http://example.test")
  private val stock    = stubbed[Resolve] ++ stubbed[ContentStore]
  private val first    = DeploymentId("dep-1")

  def spec = suite("release run laws")(
    test("a deployment file records validated and published, refuses a state with no token, and drops only that id"):
      ZIO.scoped {
        Workspace.work("histos-release-file").flatMap { repo =>
          val quiet = List(
            DeploymentState.Pending,
            DeploymentState.Validating,
            DeploymentState.Publishing,
            DeploymentState.Failed,
          )
          val validated = "id dep-1\nstate VALIDATED\nversion 1.2.3\n"
          val published = "id dep-1\nstate PUBLISHED\nversion 1.2.3\n"
          val otherText = "id dep-2\nstate PUBLISHED\nversion 9.9.9\n"
          val id        = first
          val other     = DeploymentId("dep-2")
          val spare     = DeploymentId("dep-3")
          for
            gone    <- DeploymentFile.remove(repo, id)
            _       <- DeploymentFile.mark(repo, id, DeploymentState.Published)
            bare    <- touched(repo)
            refused <- ZIO.foreach(quiet) { state =>
              DeploymentFile.write(repo, ShipName("third"), spare, state, "1.0.0").either
            }
            third     <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("third")))
            stillBare <- touched(repo)
            _         <- DeploymentFile.write(repo, ShipName("demo"), id, DeploymentState.Validated, number.render)
            _         <- DeploymentFile.write(repo, ShipName("other"), other, DeploymentState.Published, "9.9.9")
            read      <- DeploymentFile.read(repo, ShipName("demo"))
            text      <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("demo")))
            _         <- DeploymentFile.mark(repo, id, DeploymentState.Published)
            marked    <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("demo")))
            stuck     <- DeploymentFile.mark(repo, id, DeploymentState.Failed).either
            stayed    <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("demo")))
            _         <- Workspace.createDirectories(DeploymentFile.location(repo, ShipName("dirship")))
            directory <- DeploymentFile.read(repo, ShipName("dirship"))
            removed   <- DeploymentFile.remove(repo, id)
            demoGone  <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("demo")))
            kept      <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("other")))
            again     <- DeploymentFile.remove(repo, id)
            _         <- Workspace.writeString(DeploymentFile.location(repo, ShipName("demo")), "nope\n")
            short     <- DeploymentFile.read(repo, ShipName("demo")).either
            _         <- Workspace.writeString(
              DeploymentFile.location(repo, ShipName("demo")),
              "id dep-1\nstate PENDING\nversion 1.2.3\n",
            )
            pending <- DeploymentFile.read(repo, ShipName("demo")).either
          yield assertTrue(
            gone == None,
            bare == false,
            refused == quiet.map(_ => Left(ReleaseError.Record("third"))),
            third == None,
            stillBare == false,
            read == Some(DeploymentFile.Record(id, DeploymentState.Validated, number.render)),
            text == Some(validated),
            marked == Some(published),
            stuck == Left(ReleaseError.Record("demo")),
            stayed == Some(published),
            directory == None,
            removed == Some(ShipName("demo") -> DeploymentFile.Record(id, DeploymentState.Published, number.render)),
            demoGone == None,
            kept == Some(otherText),
            again == None,
            short == Left(ReleaseError.Record("demo")),
            pending == Left(ReleaseError.Record("demo")),
          )
        }
      }
    ,
    test("stage refuses missing credentials before a missing key, and neither writes classes"):
      ZIO
        .scoped {
          Workspace.work("histos-release-stage").flatMap { repo =>
            for
              portal  <- RefusingLoopback.bound
              graph   <- ZIO.fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
              runtime <- ZIO.fromEither(jdk)
              missing <- ReleaseRun
                .stage(repo, graph, Nil, runtime, Map.empty, ApiDigest.trusted("ab" * 32), _ => ZIO.unit)
                .either
              bare   <- touched(repo)
              signer <- ReleaseRun
                .stage(repo, graph, Nil, runtime, at(portal), ApiDigest.trusted("ab" * 32), _ => ZIO.unit)
                .either
              still   <- touched(repo)
              reached <- portal.connections
            yield assertTrue(
              missing == Left(ReleaseError.Credentials),
              signer == Left(ReleaseError.Signer(SignerError.NoSecret)),
              !bare,
              !still,
              reached == 0,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("a published version is held, an unreadable record fails the ship, and validated is not a hold"):
      ZIO
        .scoped {
          Workspace.work("histos-release-upload").flatMap { repo =>
            for
              portal  <- RefusingLoopback.bound
              _       <- stub[Signer](_.sign(any[Chunk[Byte]]))(ZIO.die(new IllegalStateException("signed")))
              _       <- stub[Signer](_.fingerprint)(ZIO.die(new IllegalStateException("asked")))
              signer  <- ZIO.service[Signer]
              staged  <- ready(repo, signer, portal)
              _       <- DeploymentFile.write(repo, ShipName("demo"), first, DeploymentState.Published, number.render)
              held    <- ReleaseRun.upload(repo, staged, ReleaseMode.Hold, _ => ZIO.unit)
              _       <- Workspace.writeString(DeploymentFile.location(repo, ShipName("demo")), "nope\n")
              broken  <- ReleaseRun.upload(repo, staged, ReleaseMode.Hold, _ => ZIO.unit)
              _       <- DeploymentFile.write(repo, ShipName("demo"), first, DeploymentState.Validated, number.render)
              open    <- ReleaseRun.upload(repo, staged, ReleaseMode.Hold, _ => ZIO.unit)
              reached <- portal.connections
            yield assertTrue(
              held == ReleaseReport(
                List("left out"),
                List("prior", s"demo ${number.render} is already published"),
                Nil,
                Nil,
              ),
              broken == ReleaseReport(
                List("left out"),
                List("prior"),
                List("demo deployment record is not readable"),
                Nil,
              ),
              open == ReleaseReport(List("left out"), List("prior"), List("demo could not write app"), Nil),
              reached == 0,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stubbed[Signer])
    ,
    test("a drop that is refused leaves the deployment file, and the note does not carry the password"):
      ZIO.scoped {
        Workspace.work("histos-release-drop").flatMap { repo =>
          val text = s"id dep-1\nstate VALIDATED\nversion ${number.render}\n"
          for
            portal  <- RefusingLoopback.bound
            graph   <- ZIO.fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
            notes   <- Ref.make(List.empty[String])
            _       <- DeploymentFile.write(repo, ShipName("demo"), first, DeploymentState.Validated, number.render)
            missing <- ReleaseRun.drop(repo, graph, Map("HISTOS_PORTAL" -> portal.url), "dep-1", note(notes)).either
            missingNotes <- notes.getAndUpdate(_ => Nil)
            portalled    <- ReleaseRun.drop(repo, graph, insecure, "dep-1", note(notes)).either
            portalNotes  <- notes.getAndUpdate(_ => Nil)
            spaced       <- ReleaseRun.drop(repo, graph, at(portal), "has space", note(notes)).either
            spacedNotes  <- notes.get
            stayed       <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("demo")))
            reached      <- portal.connections
          yield assertTrue(
            missing == Left(ReleaseError.Credentials),
            missingNotes == List("dropping dep-1", ReleaseError.Credentials.message),
            portalled == Left(ReleaseError.Portal(PortalBase.Error.Insecure("example.test"))),
            portalNotes == List("dropping dep-1", "http portal 'example.test' is not loopback"),
            spaced == Left(ReleaseError.Deployment(DeploymentId.Error.Spaced("has space"))),
            spacedNotes == List("dropping has space", "deployment id 'has space' contains whitespace"),
            stayed == Some(text),
            !missingNotes.exists(_.contains("not-for-the-note")),
            !portalNotes.exists(_.contains("not-for-the-note")),
            !spacedNotes.exists(_.contains("not-for-the-note")),
            reached == 0,
          )
        }
      }
    ,
    test("a promote names missing credentials, and an insecure portal is refused before a deployment file exists"):
      ZIO.scoped {
        Workspace.work("histos-release-promote").flatMap { repo =>
          for
            notes     <- Ref.make(List.empty[String])
            unsigned  <- ReleaseRun.promote(repo, Map.empty, "dep-1", note(notes)).either
            bareNotes <- notes.getAndUpdate(_ => Nil)
            found     <- ReleaseRun.promote(repo, insecure, "dep-1", note(notes)).either
            said      <- notes.get
            wrote     <- touched(repo)
          yield assertTrue(
            unsigned == Left(ReleaseError.Credentials),
            bareNotes == List("promoting dep-1", ReleaseError.Credentials.message),
            found == Left(ReleaseError.Portal(PortalBase.Error.Insecure("example.test"))),
            said == List("promoting dep-1", "http portal 'example.test' is not loopback"),
            !said.exists(_.contains("not-for-the-note")),
            !wrote,
          )
        }
      },
  )

  private def at(portal: RefusingLoopback): Map[String, String] =
    creds + ("HISTOS_PORTAL" -> portal.url)

  private def ready(repo: Path, signer: Signer, portal: RefusingLoopback) =
    for
      user <- ZIO.fromEither(RepositoryUser.from("portal-user"))
      base <- ZIO.fromEither(PortalBase.from(portal.url))
    yield ReleaseStage(
      repo,
      List(published(Path.of("app.jar"))),
      List("left out"),
      List("prior"),
      Nil,
      signer,
      base,
      RepositorySecret(user, Config.Secret(Chunk.fromArray("not-for-the-note".toCharArray))),
      Fingerprint("0123456789abcdef"),
      Map("HISTOS_PORTAL" -> portal.url),
    )

  private def published(jar: Path): PublishedModule =
    PublishedModule(
      ProjectName("app"),
      group,
      "app",
      number.render,
      Some(jar),
      None,
      None,
      None,
      ShipName("demo"),
      Sha256Digest.of(Chunk.empty),
      None,
      Nil,
    )

  private def jdk =
    JdkVersion
      .from("21")
      .map(version =>
        JdkRuntime(Path.of("jdk"), version, RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)))
      )

  private def note(notes: Ref[List[String]])(line: String): UIO[Unit] =
    notes.update(_ :+ line)

  private def touched(repo: Path): UIO[Boolean] =
    ZIO.attemptBlocking(Files.exists(repo.resolve(".histos"))).orDie
end ReleaseRunLawSpec
