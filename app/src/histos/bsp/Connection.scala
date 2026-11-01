package histos.bsp

import histos.publish.LauncherPin
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.json.*
import zio.{IO, UIO, ZIO}

/** The `.bsp/histos.json` file Metals reads. The version is `histos.version` when this checkout has one. */
final case class Connection(
    name: String,
    argv: List[String],
    version: String,
    bspVersion: String,
    languages: List[String],
) derives JsonCodec

object Connection:
  def ensure(repo: Path): IO[IoFailure, Unit] =
    version(repo).flatMap:
      case None        => ZIO.unit
      case Some(value) => write(repo, value)

  /** The properties line when it parses. A missing or bad line writes nothing. */
  def version(repo: Path): UIO[Option[String]] =
    LauncherPin.read(repo).map(pin => Some(pin.version)).orElseSucceed(None)

  /** `./histos` when the project ships that wrapper. Otherwise the `histos` on `PATH`. */
  def launch(repo: Path): List[String] =
    if Files.isRegularFile(repo.resolve("histos")) then List("./histos", "bsp")
    else List("histos", "bsp")

  def here(argv: List[String], version: String): Connection =
    Connection(Bsp.ServerName, argv, version, Bsp.BspVersion, List("scala"))

  private def write(repo: Path, version: String): IO[IoFailure, Unit] =
    val path = repo.resolve(".bsp").resolve("histos.json")
    ZIO
      .attemptBlocking {
        val wanted  = here(launch(repo), version)
        val present =
          if Files.isRegularFile(path) then Files.readString(path).fromJson[Connection].toOption
          else None
        if !present.contains(wanted) then
          Files.createDirectories(repo.resolve(".bsp"))
          Files.writeString(path, wanted.toJson)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
end Connection
