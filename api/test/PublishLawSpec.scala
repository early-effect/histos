package histos

import histos.doctor.Level
import histos.version.{ReleaseVersion, Revision, ShipNumber}

import java.nio.file.Path

import zio.*
import zio.test.*

object PublishLawSpec extends ZIOSpecDefault:
  private val revision = Revision.Release(ReleaseVersion(ShipNumber.trusted(1, 4, 2)))
  private val request  = PublishRequest(revision, false, Path.of("stage"), Path.of("checkout"), Chunk.empty)

  def spec = suite("publish laws")(
    test("automatic mode forces the request, and hold keeps a false request false"):
      for
        forced <- automatic(ReleaseMode.Automatic, false)
        held   <- automatic(ReleaseMode.Hold, false)
        kept   <- automatic(ReleaseMode.Hold, true)
      yield assertTrue(forced == Some(true), held == Some(false), kept == Some(true))
    ,
    test("follow up is finished for automatic and a deployment for hold"):
      assertTrue(
        Sona.followUp(ReleaseMode.Automatic) == FollowUp.Finished,
        Sona.followUp(ReleaseMode.Hold) == FollowUp.Deployment,
        Sona.secrets == List(
          "SONATYPE_USERNAME",
          "SONATYPE_PASSWORD",
          "PGP_SECRET",
          "PGP_PASSPHRASE",
          "PGP_KEY_HEX",
        ),
        Sona.described(ReleaseMode.Automatic).followUp == FollowUp.Finished,
        Sona.described(ReleaseMode.Hold).followUp == FollowUp.Deployment,
        Sona.described(ReleaseMode.Hold).secretNames == Sona.secrets,
        Sona.idle.followUp == FollowUp.Finished,
        Sona.idle.secretNames == Sona.secrets,
      )
    ,
    test("idle and a described stock refuse every revision as version"):
      for
        idle      <- Sona.idle.publish(request).flip
        described <- Sona.described(ReleaseMode.Hold).publish(request).flip
      yield assertTrue(
        idle == PublishFault.Version(revision),
        described == PublishFault.Version(revision),
      )
    ,
    test("from effect threads the secrets and the follow up it was given"):
      val publishing = Publishing.fromEffect(
        request => ZIO.succeed(PublishReceipt(Chunk(request.revision.render), Chunk.empty)),
        secrets = List("ONE"),
        after = FollowUp.Deployment,
      )
      for receipt <- publishing.publish(request)
      yield assertTrue(
        publishing.secretNames == List("ONE"),
        publishing.followUp == FollowUp.Deployment,
        receipt.lines == Chunk(revision.render),
        receipt.failed.isEmpty,
      )
    ,
    test("each publish fault diagnoses as an error on its own check"):
      val faults = List(
        PublishFault.Credentials       -> "publish.credentials",
        PublishFault.Rejected          -> "publish.rejected",
        PublishFault.Status(500)       -> "publish.status",
        PublishFault.Transport("down") -> "publish.transport",
        PublishFault.Metadata("gap")   -> "publish.metadata",
        PublishFault.Version(revision) -> "publish.failed",
        PublishFault.Signer            -> "publish.failed",
        PublishFault.Refused("no")     -> "publish.failed",
      )
      assertTrue(
        faults.forall: (fault, check) =>
          val noted = fault.diagnose
          noted.check.value == check && noted.level == Level.Error && noted.title == noted.summary
      ),
  )

  /** `api` also builds for Native, and slayer has no Native artifact. This stock only records the flag the layer
    * forwarded.
    */
  private final class Stock(seen: Ref[Option[Boolean]]) extends SonaPublish:
    def publish(request: PublishRequest): IO[PublishFault, PublishReceipt] =
      seen.set(Some(request.automatic)) *> ZIO.succeed(PublishReceipt(Chunk.empty, Chunk.empty))
    def secretNames: List[String] = Nil
    def followUp: FollowUp        = FollowUp.Finished

  private def automatic(mode: ReleaseMode, flag: Boolean): IO[PublishFault, Option[Boolean]] =
    for
      seen   <- Ref.make[Option[Boolean]](None)
      stored <-
        (for
          publishing <- ZIO.service[Publishing]
          _          <- publishing.publish(request.copy(automatic = flag))
          stored     <- seen.get
        yield stored).provide(ZLayer.succeed[SonaPublish](Stock(seen)) >>> Sona.layer(mode))
    yield stored
end PublishLawSpec
