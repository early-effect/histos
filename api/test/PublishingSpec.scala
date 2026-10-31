package histos

import histos.version.{ReleaseVersion, Revision, ShipNumber}

import java.nio.file.Path

import zio.{Chunk, Ref, ZIO, ZLayer}
import zio.test.*

object PublishingSpec extends ZIOSpecDefault:
  private val revision = Revision.Release(ReleaseVersion(ShipNumber("1.0.0")))

  private def request(automatic: Boolean): PublishRequest =
    PublishRequest(revision, automatic, Path.of("stage"), Path.of("checkout"), Chunk.empty)

  private final class Seen(ref: Ref[Chunk[PublishRequest]]) extends SonaPublish with LocalPublish:
    def publish(asked: PublishRequest) =
      ref.update(_ :+ asked).as(PublishReceipt(Chunk("wrote"), Chunk.empty))
    def secretNames: List[String] = Nil
    def followUp: FollowUp        = FollowUp.Finished

  def spec = suite("publishing")(
    test("hold stays held unless this run forces automatic, and automatic always publishes"):
      for
        seen <- Ref.make(Chunk.empty[PublishRequest])
        stock = Seen(seen)
        hold  = ZLayer.succeed(stock) >>> Sona.layer(ReleaseMode.Hold)
        auto  = ZLayer.succeed(stock) >>> Sona.layer(ReleaseMode.Automatic)
        _          <- ZIO.serviceWithZIO[Publishing](_.publish(request(false))).provideLayer(hold)
        _          <- ZIO.serviceWithZIO[Publishing](_.publish(request(true))).provideLayer(hold)
        _          <- ZIO.serviceWithZIO[Publishing](_.publish(request(false))).provideLayer(auto)
        recorded   <- seen.get
        heldFollow <- ZIO.serviceWith[Publishing](_.followUp).provideLayer(hold)
        autoFollow <- ZIO.serviceWith[Publishing](_.followUp).provideLayer(auto)
        names      <- ZIO.serviceWith[Publishing](_.secretNames).provideLayer(hold)
      yield assertTrue(
        recorded.length == 3,
        recorded.lift(0).exists(item => !item.automatic),
        recorded.lift(1).exists(_.automatic),
        recorded.lift(2).exists(_.automatic),
        heldFollow == FollowUp.Deployment,
        autoFollow == FollowUp.Finished,
        names == Sona.secrets,
      )
    ,
    test("local passes the request to the stock"):
      for
        seen <- Ref.make(Chunk.empty[PublishRequest])
        stock = Seen(seen)
        _ <- ZIO
          .serviceWithZIO[Publishing](_.publish(request(false)))
          .provideLayer(ZLayer.succeed(stock) >>> Local.layer)
        got <- seen.get
      yield assertTrue(got == Chunk(request(false)))
    ,
    test("a description and an idle stock refuse the revision"):
      for
        described <- Sona.described(ReleaseMode.Hold).publish(request(false)).either
        idle      <- Sona.idle.publish(request(false)).either
      yield assertTrue(
        Sona.described(ReleaseMode.Hold).followUp == FollowUp.Deployment,
        Sona.described(ReleaseMode.Automatic).followUp == FollowUp.Finished,
        Sona.described(ReleaseMode.Automatic).secretNames == Sona.secrets,
        described == Left(PublishFault.Version(revision)),
        idle == Left(PublishFault.Version(revision)),
      ),
  )
end PublishingSpec
