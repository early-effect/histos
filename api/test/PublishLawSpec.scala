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
    test("the sona layer names the five release secrets, and follows up as its mode says, not as the stock says"):
      val secrets = List("SONATYPE_USERNAME", "SONATYPE_PASSWORD", "PGP_SECRET", "PGP_PASSPHRASE", "PGP_KEY_HEX")
      for
        finished <- shown(ReleaseMode.Automatic)
        deployed <- shown(ReleaseMode.Hold)
      yield assertTrue(
        finished == (secrets, FollowUp.Finished),
        deployed == (secrets, FollowUp.Deployment),
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

  /** `api` also builds for Native, and slayer has no Native artifact. This stock records the flag the layer forwarded,
    * names no secret, and finishes.
    */
  private final class Stock(seen: Ref[Option[Boolean]]) extends SonaPublish:
    def publish(request: PublishRequest): IO[PublishFault, PublishReceipt] =
      seen.set(Some(request.automatic)) *> ZIO.succeed(PublishReceipt(Chunk.empty, Chunk.empty))
    def secretNames: List[String] = Nil
    def followUp: FollowUp        = FollowUp.Finished

  /** The secrets and the follow up the layer shows a workflow, around a stock that names neither. */
  private def shown(mode: ReleaseMode): UIO[(List[String], FollowUp)] =
    for
      seen  <- Ref.make[Option[Boolean]](None)
      shown <- ZIO
        .serviceWith[Publishing](publishing => (publishing.secretNames, publishing.followUp))
        .provide(ZLayer.succeed[SonaPublish](Stock(seen)) >>> Sona.layer(mode))
    yield shown

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
