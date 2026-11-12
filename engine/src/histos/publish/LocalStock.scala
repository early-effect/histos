package histos.publish

import histos.version.Revision
import histos.{FollowUp, PublishFault, PublishReceipt, StagedModule}

import zio.{Chunk, IO, UIO, URLayer, ZIO, ZLayer}

/** Installs into the [[IvyRoot]] Coursier resolves. It does not sign. */
object LocalStock:
  def layer(note: String => UIO[Unit]): URLayer[IvyRoot, histos.LocalPublish] =
    ZLayer(ZIO.serviceWith[IvyRoot](stock(_, note)))

  private def stock(root: IvyRoot, note: String => UIO[Unit]): histos.LocalPublish =
    new histos.LocalPublish:
      def publish(request: histos.PublishRequest): IO[PublishFault, PublishReceipt] =
        ZIO.succeed(accept(request.revision)) *>
          ZIO
            .foreach(request.modules)(module => publishOne(root, note, module))
            .map(lines => PublishReceipt(lines, Chunk.empty))

      def secretNames: List[String] = Nil

      def followUp: FollowUp = FollowUp.Finished

  private def publishOne(root: IvyRoot, note: String => UIO[Unit], module: StagedModule): IO[PublishFault, String] =
    note(s"installing ${module.artifact} ${module.revision}") *>
      IvyLocal
        .install(root, module)
        .tapError(error => note(error.message))
        .as(
          s"${module.group.value} ${module.artifact} ${module.revision}"
        )

  private def accept(revision: Revision): Unit =
    revision match
      case Revision.Ci(_) | Revision.Snapshot(_) | Revision.Pointer(_) | Revision.Release(_) => ()
end LocalStock
