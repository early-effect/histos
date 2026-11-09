package histos.publish

import histos.version.{AbbrevSha, Revision, ShipNumber, SnapshotCoordinate}
import histos.workspace.Workspace
import histos.version.ShipName
import histos.{
  FollowUp,
  Organization,
  ProjectName,
  PublishFault,
  PublishReceipt,
  PublishRequest,
  Sona,
  SonaPublish,
  StagedDependency,
  StagedModule,
}

import java.nio.file.{Files, Path}

import zio.{Chunk, ZIO}
import zio.test.*

object SonaPublishLawSpec extends ZIOSpecDefault:
  private val creds  = Map("SONATYPE_USERNAME" -> "user", "SONATYPE_PASSWORD" -> "pw")
  private val number = ShipNumber("1.2.3")
  private val group  = Organization.trusted("org.example")
  private val module = ProjectName.trusted("core")

  def spec = suite("sonatype publish laws")(
    test("no staged modules is an empty receipt, and the stock names the sonatype secrets"):
      val snapshot = Revision.Snapshot(SnapshotCoordinate(number, AbbrevSha.trusted("abcdefabcdef")))
      val pointer  = Revision.Pointer(histos.version.SnapshotPointer(number))
      for
        empty   <- publish(creds, PublishRequest(snapshot, false, Path.of("stage"), Path.of("checkout"), Chunk.empty))
        pointed <- publish(creds, PublishRequest(pointer, true, Path.of("stage"), Path.of("checkout"), Chunk.empty))
        secrets <- ZIO.serviceWith[SonaPublish](_.secretNames).provide(SonaStock.layer(creds, _ => ZIO.unit))
        follow  <- ZIO.serviceWith[SonaPublish](_.followUp).provide(SonaStock.layer(creds, _ => ZIO.unit))
      yield assertTrue(
        empty == Right(PublishReceipt(Chunk.empty, Chunk.empty)),
        pointed == Right(PublishReceipt(Chunk.empty, Chunk.empty)),
        secrets == Sona.secrets,
        follow == FollowUp.Finished,
      )
    ,
    test("a missing jar, a bad artifact, and a bad dependency fail before an upload"):
      Workspace
        .work("histos-sona-stage")
        .flatMap: root =>
          val jar = root.resolve("core.jar")
          val pom = root.resolve("core.pom")
          for
            _       <- write(jar, Array(1, 2))
            _       <- write(pom, Array(3))
            missing <- publish(creds, request(root.resolve("missing.jar"), pom, "core", Chunk.empty))
            named   <- publish(creds, request(jar, pom, "1bad", Chunk.empty))
            ranged  <- publish(
              creds,
              request(
                jar,
                pom,
                "core",
                Chunk(
                  StagedDependency(group, "core", "1.0.0"),
                  StagedDependency(group, "kit", ".."),
                ),
              ),
            )
          yield assertTrue(
            missing == Left(PublishFault.Write(root.resolve("missing.jar").toString)),
            named == Left(PublishFault.Artifact("1bad")),
            ranged == Left(PublishFault.Dependency("..")),
          ),
  )

  private def request(
      jar: Path,
      pom: Path,
      artifact: String,
      dependencies: Chunk[StagedDependency],
  ): PublishRequest =
    val revision = Revision.Snapshot(SnapshotCoordinate(number, AbbrevSha.trusted("abcdefabcdef")))
    PublishRequest(
      revision,
      false,
      jar.getParent,
      Path.of("checkout"),
      Chunk(staged(jar, pom, artifact, dependencies)),
    )

  private def staged(
      jar: Path,
      pom: Path,
      artifact: String,
      dependencies: Chunk[StagedDependency],
  ): StagedModule =
    StagedModule(
      module,
      ShipName("core"),
      group,
      artifact,
      "1.0.0",
      jar,
      pom,
      None,
      None,
      dependencies,
    )

  private def publish(
      env: Map[String, String],
      request: PublishRequest,
  ): ZIO[Any, Nothing, Either[PublishFault, PublishReceipt]] =
    ZIO
      .serviceWithZIO[SonaPublish](_.publish(request))
      .provide(SonaStock.layer(env, _ => ZIO.unit))
      .either

  private def write(path: Path, bytes: Array[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
        ()
      }
      .catchAll(error => ZIO.dieMessage(error.getClass.getName))
end SonaPublishLawSpec
