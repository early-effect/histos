package histos.bsp

import histos.Resolution
import histos.publish.LauncherPin
import histos.server.Fingerprint
import histos.watch.SourceWatch
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.json.*
import zio.{IO, Scope, UIO, ZIO}

/** The `.bsp/histos.json` file Metals reads. `version` is `histos.version`. `graph` is the build fingerprint. Metals
  * ignores `graph` and hashes the file, so a new fingerprint is a build change.
  */
final case class Connection(
    name: String,
    argv: List[String],
    version: String,
    bspVersion: String,
    languages: List[String],
    graph: String,
) derives JsonCodec

object Connection:
  def ensure(repo: Path): IO[IoFailure, Unit] =
    version(repo).flatMap:
      case None        => ZIO.unit
      case Some(value) =>
        Fingerprint
          .read(repo, Resolution.Solve)
          .foldZIO(_ => ZIO.unit, fingerprint => write(repo, value, fingerprint.value))

  /** While a BSP session is up, rewrite the connection file once the fingerprint changes. */
  def follow(repo: Path): ZIO[Scope, Nothing, Unit] =
    watch(repo).forkScoped.unit

  /** The properties line when it parses. A missing or bad line writes nothing. */
  def version(repo: Path): UIO[Option[String]] =
    LauncherPin.read(repo).map(pin => Some(pin.version)).orElseSucceed(None)

  /** `./histos` when the project ships that wrapper. Otherwise the `histos` on `PATH`. */
  def launch(repo: Path): List[String] =
    if Files.isRegularFile(repo.resolve("histos")) then List("./histos", "bsp")
    else List("histos", "bsp")

  def here(argv: List[String], version: String, graph: String): Connection =
    Connection(Bsp.ServerName, argv, version, Bsp.BspVersion, List("scala"), graph)

  /** The connection file's fingerprint does not hash the lock, so the lock is not a root. */
  private def watch(repo: Path): UIO[Unit] =
    Fingerprint.roots(repo, Resolution.Solve).flatMap { paths =>
      SourceWatch
        .take(paths)
        .foldZIO(
          _ => ZIO.sleep(SourceWatch.Poll) *> watch(repo),
          start =>
            SourceWatch
              .since(paths, start)
              .foldZIO(
                _ => ZIO.sleep(SourceWatch.Poll) *> watch(repo),
                _ => ensure(repo).ignore *> watch(repo),
              ),
        )
    }

  private def write(repo: Path, version: String, graph: String): IO[IoFailure, Unit] =
    val path = repo.resolve(".bsp").resolve("histos.json")
    ZIO
      .attemptBlocking {
        val wanted  = here(launch(repo), version, graph)
        val present =
          if Files.isRegularFile(path) then Files.readString(path).fromJson[Connection].toOption
          else None
        if !present.contains(wanted) then
          Files.createDirectories(repo.resolve(".bsp"))
          Files.writeString(path, wanted.toJson)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
end Connection
