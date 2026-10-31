package histos.publish

import histos.version.Revision
import histos.{FollowUp, PublishFault, PublishReceipt, StagedModule}

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** Installs into the ivy2 local repository Coursier already resolves. It does not sign. */
object LocalStock:
  val layer: ULayer[histos.LocalPublish] =
    ZLayer.succeed(stock)

  private val stock: histos.LocalPublish =
    new histos.LocalPublish:
      def publish(request: histos.PublishRequest): IO[PublishFault, PublishReceipt] =
        ZIO.succeed(accept(request.revision)) *>
          ZIO.foreach(request.modules)(publishOne).map(lines => PublishReceipt(lines, Chunk.empty))

      def secretNames: List[String] = Nil

      def followUp: FollowUp = FollowUp.Finished

  private def publishOne(module: StagedModule): IO[PublishFault, String] =
    IvyLocal.install(module).as(s"${module.group.value} ${module.artifact} ${module.revision}")

  private def accept(revision: Revision): Unit =
    revision match
      case Revision.Ci(_) | Revision.Snapshot(_) | Revision.Pointer(_) | Revision.Release(_) => ()
end LocalStock
