package histos.publish

import histos.version.{CiVersion, Revision, ShipName, ShipNumber}
import histos.{Organization, ProjectName, StagedModule}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.UUID

import zio.{Chunk, ZIO}
import zio.test.*

object IvyLocalSpec extends ZIOSpecDefault:
  def spec = suite("ivy local")(
    test("default repositories fetch the jar publishLocal installed"):
      installed.provideLayer(LocalStock.layer(_ => ZIO.unit))
  )

  private val installed =
    for
      organization <- ZIO.fromEither(Organization.from("rocks.earlyeffect.histos.probe"))
      revision = s"0.0.0${UUID.randomUUID().toString.replace("-", "")}"
      jarBytes = s"histos-ivy-local $revision".getBytes(StandardCharsets.UTF_8)
      pomBytes = s"<project>$revision</project>".getBytes(StandardCharsets.UTF_8)
      directory <- ZIO.attemptBlocking(Files.createTempDirectory("histos-ivy"))
      jar = directory.resolve("probe.jar")
      pom = directory.resolve("probe.pom")
      _ <- ZIO.attemptBlocking {
        Files.write(jar, jarBytes)
        Files.write(pom, pomBytes)
        ()
      }
      request = histos.PublishRequest(
        Revision.Ci(CiVersion(ShipNumber("0.0.0"))),
        automatic = false,
        stage = directory,
        checkout = directory,
        modules = Chunk(
          StagedModule(
            ProjectName("probe"),
            ShipName("probe"),
            organization,
            artifact = "probe",
            revision = revision,
            jar = jar,
            pom = pom,
            sources = None,
            javadoc = None,
            dependencies = Chunk.empty,
          )
        ),
      )
      receipt <- ZIO.serviceWithZIO[histos.LocalPublish](_.publish(request))
      names   <- ZIO.serviceWith[histos.LocalPublish](_.secretNames)
      after   <- ZIO.serviceWith[histos.LocalPublish](_.followUp)
      dependency = coursier.Dependency(
        coursier.Module(
          coursier.Organization(organization.value),
          coursier.ModuleName("probe"),
        ),
        coursier.version.VersionConstraint(revision),
      )
      resolved <- ZIO.attemptBlockingInterrupt(coursier.Resolve().addDependencies(dependency).either())
      fetched  <- ZIO.attemptBlockingInterrupt(coursier.Fetch().addDependencies(dependency).either())
      matched  <- fetched match
        case Right(files) =>
          files.toList match
            case file :: Nil =>
              ZIO.attemptBlocking(java.util.Arrays.equals(Files.readAllBytes(file.toPath), jarBytes))
            case _ => ZIO.succeed(false)
        case Left(_) => ZIO.succeed(false)
      count = fetched.fold(_ => -1, _.length)
    yield assertTrue(
      receipt.lines == Chunk(s"${organization.value} probe $revision"),
      names.isEmpty,
      after == histos.FollowUp.Finished,
      resolved.isRight,
      fetched.isRight,
      count == 1,
      matched,
    )
end IvyLocalSpec
