package histos.publish

import histos.central.SnapshotRepository
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

import java.nio.file.Path

import zio.{Chunk, Scope, ZIO, ZLayer}
import zio.test.*

/** The snapshot repository is a loopback that fails every request, so a law that reaches it fails on its count. */
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
        loop    <- RefusingLoopback.bound
        empty   <- publish(loop, PublishRequest(snapshot, false, Path.of("stage"), Path.of("checkout"), Chunk.empty))
        pointed <- publish(loop, PublishRequest(pointer, true, Path.of("stage"), Path.of("checkout"), Chunk.empty))
        stock   <- ZIO.service[SonaPublish].provide(stocked(loop))
        reached <- loop.connections
      yield assertTrue(
        empty == Right(PublishReceipt(Chunk.empty, Chunk.empty)),
        pointed == Right(PublishReceipt(Chunk.empty, Chunk.empty)),
        stock.secretNames == Sona.secrets,
        stock.followUp == FollowUp.Finished,
        reached == 0,
      )
    ,
    test("a missing jar, a bad artifact, and a bad dependency fail before an upload"):
      for
        root <- Workspace.work("histos-sona-stage")
        loop <- RefusingLoopback.bound
        jar = root.resolve("core.jar")
        pom = root.resolve("core.pom")
        _       <- Workspace.writeString(jar, "jar")
        _       <- Workspace.writeString(pom, "pom")
        missing <- publish(loop, request(root.resolve("missing.jar"), pom, "core", Chunk.empty))
        named   <- publish(loop, request(jar, pom, "1bad", Chunk.empty))
        ranged  <- publish(
          loop,
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
        reached <- loop.connections
      yield assertTrue(
        missing == Left(PublishFault.Write(root.resolve("missing.jar").toString)),
        named == Left(PublishFault.Artifact("1bad")),
        ranged == Left(PublishFault.Dependency("..")),
        reached == 0,
      )
    ,
    test("a staged module that passes every check is a transport fault at the loopback"):
      for
        root <- Workspace.work("histos-sona-upload")
        loop <- RefusingLoopback.bound
        jar = root.resolve("core.jar")
        pom = root.resolve("core.pom")
        _       <- Workspace.writeString(pom, "pom")
        _       <- ZIO.attemptBlocking(java.nio.file.Files.write(jar, Array[Byte](0x50, 0x4b, 3, 4))).orDie
        sent    <- publish(loop, request(jar, pom, "core", Chunk.empty))
        reached <- loop.connections
      yield assertTrue(
        sent match
          case Left(PublishFault.Transport(_)) => true
          case _                               => false
        ,
        reached > 0,
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

  private def stocked(loop: RefusingLoopback): ZLayer[Any, SnapshotRepository.Error, SonaPublish] =
    ZLayer.fromZIO(ZIO.fromEither(SnapshotRepository.from(s"${loop.url}/repository"))) >>>
      SonaStock.layer(creds, _ => ZIO.unit)

  private def publish(
      loop: RefusingLoopback,
      request: PublishRequest,
  ): ZIO[Scope, SnapshotRepository.Error, Either[PublishFault, PublishReceipt]] =
    ZIO
      .serviceWithZIO[SonaPublish](_.publish(request).either)
      .provide(stocked(loop))
end SonaPublishLawSpec
