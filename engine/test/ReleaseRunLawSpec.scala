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
import histos.jdk.{JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.plugin.ApiDigest
import histos.resolve.Resolve
import histos.toolchain.RuntimeIdentity
import histos.version.ShipName
import histos.workspace.Workspace
import histos.{Jvm, ProjectGraph, ProjectName, Projects, Ship, module, organization, ship, ships}
import histos.`scala`

import java.nio.file.{Files, Path}

import slayer.{any, stub, stubbed}

import zio.{Chunk, Config, Ref, Scope, ZIO}
import zio.test.*

object ReleaseRunLawSpec extends ZIOSpecDefault:
  private val scala3   = scala"3.10.0"
  private val number   = ship"1.2.3"
  private val group    = organization"example.com"
  private val app      = module("app", Jvm)(scala3)
  private val creds    = Map("SONATYPE_USERNAME" -> "portal-user", "SONATYPE_PASSWORD" -> "not-for-the-note")
  private val loop     = creds + ("HISTOS_PORTAL" -> "http://127.0.0.1:9")
  private val insecure = creds + ("HISTOS_PORTAL" -> "http://example.test")
  private val stock    = stubbed[Resolve] ++ stubbed[ContentStore]

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
          for
            id      <- identifier("dep-1")
            other   <- identifier("dep-2")
            spare   <- identifier("dep-3")
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
            refused.forall(_ == Left(ReleaseError.Record("third"))),
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
              graph   <- load(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
              runtime <- jdk
              missing <- ReleaseRun
                .stage(repo, graph, Nil, runtime, Map.empty, ApiDigest.trusted("ab" * 32), _ => ZIO.unit)
                .either
              bare   <- touched(repo)
              signer <- ReleaseRun
                .stage(repo, graph, Nil, runtime, loop, ApiDigest.trusted("ab" * 32), _ => ZIO.unit)
                .either
              still <- touched(repo)
            yield assertTrue(
              missing == Left(ReleaseError.Credentials),
              signer == Left(ReleaseError.Signer(SignerError.NoSecret)),
              !bare,
              !still,
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
              _      <- stub[Signer](_.sign(any[Chunk[Byte]]))(ZIO.die(new IllegalStateException("signed")))
              _      <- stub[Signer](_.fingerprint)(ZIO.die(new IllegalStateException("asked")))
              signer <- ZIO.service[Signer]
              id     <- identifier("dep-1")
              staged <- ready(repo, signer)
              _      <- DeploymentFile.write(repo, ShipName("demo"), id, DeploymentState.Published, number.render)
              held   <- ReleaseRun.upload(repo, staged, ReleaseMode.Hold, _ => ZIO.unit)
              _      <- Workspace.writeString(DeploymentFile.location(repo, ShipName("demo")), "nope\n")
              broken <- ReleaseRun.upload(repo, staged, ReleaseMode.Hold, _ => ZIO.unit)
              _      <- DeploymentFile.write(repo, ShipName("demo"), id, DeploymentState.Validated, number.render)
              open   <- ReleaseRun.upload(repo, staged, ReleaseMode.Hold, _ => ZIO.unit)
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
            graph   <- load(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(app)), app))
            id      <- identifier("dep-1")
            notes   <- Ref.make(List.empty[String])
            _       <- DeploymentFile.write(repo, ShipName("demo"), id, DeploymentState.Validated, number.render)
            missing <- ReleaseRun
              .drop(
                repo,
                graph,
                Map("HISTOS_PORTAL" -> "http://127.0.0.1:9"),
                "dep-1",
                note(notes),
              )
              .either
            missingNotes <- notes.getAndUpdate(_ => Nil)
            portal       <- ReleaseRun.drop(repo, graph, insecure, "dep-1", note(notes)).either
            portalNotes  <- notes.getAndUpdate(_ => Nil)
            unnamed      <- ReleaseRun.drop(repo, graph, loop, "has space", note(notes)).either
            unnamedNotes <- notes.get
            stayed       <- Workspace.readIfPresent(DeploymentFile.location(repo, ShipName("demo")))
          yield assertTrue(
            missing == Left(ReleaseError.Credentials),
            missingNotes == List("dropping dep-1", ReleaseError.Credentials.message),
            portal == Left(ReleaseError.Portal(PortalBase.Error.Insecure("example.test"))),
            portalNotes == List("dropping dep-1", "http portal 'example.test' is not loopback"),
            unnamed == Left(ReleaseError.Deployment(DeploymentId.Error.Spaced("has space"))),
            unnamedNotes == List("dropping has space", "deployment id 'has space' contains whitespace"),
            stayed == Some(text),
            !missingNotes.exists(_.contains("not-for-the-note")),
            !portalNotes.exists(_.contains("not-for-the-note")),
            !unnamedNotes.exists(_.contains("not-for-the-note")),
          )
        }
      }
    ,
    test("a promote with an insecure portal is refused before a deployment file exists"):
      ZIO.scoped {
        Workspace.work("histos-release-promote").flatMap { repo =>
          for
            notes <- Ref.make(List.empty[String])
            found <- ReleaseRun.promote(repo, insecure, "dep-1", note(notes)).either
            said  <- notes.get
            wrote <- touched(repo)
          yield assertTrue(
            found == Left(ReleaseError.Portal(PortalBase.Error.Insecure("example.test"))),
            said == List("promoting dep-1", "http portal 'example.test' is not loopback"),
            !said.exists(_.contains("not-for-the-note")),
            !wrote,
          )
        }
      },
  )

  private def ready(repo: Path, signer: Signer): ZIO[Any, Nothing, ReleaseStage] =
    for
      user        <- ZIO.fromEither(RepositoryUser.from("portal-user")).orDieWith(blow)
      base        <- ZIO.fromEither(PortalBase.from("http://127.0.0.1:9")).orDieWith(blow)
      fingerprint <- ZIO.fromEither(Fingerprint.from("0123456789abcdef")).orDieWith(blow)
    yield ReleaseStage(
      repo,
      List(published(Path.of("app.jar"))),
      List("left out"),
      List("prior"),
      Nil,
      signer,
      base,
      RepositorySecret(user, Config.Secret(Chunk.fromArray("not-for-the-note".toCharArray))),
      fingerprint,
      Map("HISTOS_PORTAL" -> "http://127.0.0.1:9"),
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

  private def identifier(raw: String): ZIO[Any, Nothing, DeploymentId] =
    ZIO.fromEither(DeploymentId.from(raw)).orDieWith(blow)

  private def load(built: Either[histos.BuildError, ProjectGraph]): ZIO[Any, Nothing, ProjectGraph] =
    ZIO.fromEither(built).orDieWith(blow)

  private def jdk: ZIO[Any, Nothing, JdkRuntime] =
    ZIO.fromEither(JdkVersion.from("21")).orDieWith(blow).map { version =>
      JdkRuntime(Path.of("jdk"), version, RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)))
    }

  private def note(notes: Ref[List[String]])(line: String): ZIO[Any, Nothing, Unit] =
    notes.update(_ :+ line)

  private def touched(repo: Path): ZIO[Any, Nothing, Boolean] =
    ZIO.attemptBlocking(Files.exists(repo.resolve(".histos"))).orDie

  private def blow(error: Any): IllegalStateException =
    new IllegalStateException(error.toString)
end ReleaseRunLawSpec
