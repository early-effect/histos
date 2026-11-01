package histos.publish

import histos.version.Revision
import histos.{FollowUp, PublishFault, PublishReceipt, StagedModule}

import zio.{Chunk, IO, UIO, ULayer, ZIO, ZLayer}

/** Installs into the ivy2 local repository Coursier already resolves. It does not sign. */
object LocalStock:
  def layer(note: String => UIO[Unit]): ULayer[histos.LocalPublish] =
    ZLayer.succeed(stock(note))

  private def stock(note: String => UIO[Unit]): histos.LocalPublish =
    new histos.LocalPublish:
      def publish(request: histos.PublishRequest): IO[PublishFault, PublishReceipt] =
        ZIO.succeed(accept(request.revision)) *>
          ZIO
            .foreach(request.modules)(module => publishOne(note, module))
            .map(lines => PublishReceipt(lines, Chunk.empty))

      def secretNames: List[String] = Nil

      def followUp: FollowUp = FollowUp.Finished

  private def publishOne(note: String => UIO[Unit], module: StagedModule): IO[PublishFault, String] =
    note(s"installing ${module.artifact} ${module.revision}") *>
      IvyLocal
        .install(module)
        .tapError(error => note(error.message))
        .as(
          s"${module.group.value} ${module.artifact} ${module.revision}"
        )

  private def accept(revision: Revision): Unit =
    revision match
      case Revision.Ci(_) | Revision.Snapshot(_) | Revision.Pointer(_) | Revision.Release(_) => ()
end LocalStock
