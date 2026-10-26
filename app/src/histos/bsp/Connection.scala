package histos.bsp

import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.json.*
import zio.{IO, ZIO}

/** The `.bsp/histos.json` file Metals reads. Later fields come from the build. */
final case class Connection(
    name: String,
    argv: List[String],
    version: String,
    bspVersion: String,
    languages: List[String],
) derives JsonCodec

object Connection:
  def ensure(repo: Path): IO[IoFailure, Unit] =
    val path = repo.resolve(".bsp").resolve("histos.json")
    ZIO
      .attemptBlocking {
        val wanted  = here(launch(repo))
        val present =
          if Files.isRegularFile(path) then Files.readString(path).fromJson[Connection].toOption
          else None
        if !present.contains(wanted) then
          Files.createDirectories(repo.resolve(".bsp"))
          Files.writeString(path, wanted.toJson)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))

  /** `./histos` when the project ships that wrapper. Otherwise the `histos` on `PATH`. */
  def launch(repo: Path): List[String] =
    if Files.isRegularFile(repo.resolve("histos")) then List("./histos", "bsp")
    else List("histos", "bsp")

  def here(argv: List[String]): Connection =
    Connection(Bsp.ServerName, argv, Bsp.ServerVersion, Bsp.BspVersion, List("scala"))
end Connection
