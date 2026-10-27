package histos.central

import java.nio.file.Path

import zio.{Chunk, ZIO, ZIOAppDefault}

/** Test main. A second process promotes or drops one deployment on a store directory. */
object PortalDisk extends ZIOAppDefault:
  def run =
    getArgs.flatMap(call)

  private def call(args: Chunk[String]) =
    args match
      case Chunk(action, root, raw) =>
        ZIO.attempt(Path.of(root)).mapError(_ => DiskError.Arguments).flatMap { path =>
          ZIO.fromEither(DeploymentId.from(raw)).mapError(_ => DiskError.Arguments).flatMap { id =>
            action match
              case "promote" => PortalStore.promote(path, id).mapError(DiskError.Store(_))
              case "drop"    => PortalStore.drop(path, id).mapError(DiskError.Store(_))
              case _         => ZIO.fail(DiskError.Arguments)
          }
        }
      case _ => ZIO.fail(DiskError.Arguments)
end PortalDisk

enum DiskError:
  case Arguments
  case Store(error: PortalStore.Error)
