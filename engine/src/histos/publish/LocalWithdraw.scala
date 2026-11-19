package histos.publish

import histos.jdk.Machine
import histos.version.{CommitSha, Ship, SnapshotCoordinate}
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Remove the ivy revisions and toolchain directory `publish local` wrote for HEAD. The pin is left alone: the same
  * coordinate may already be on Central.
  */
object LocalWithdraw:
  def revisions(ships: List[Ship], sha: CommitSha): List[String] =
    ships.map: ship =>
      val number = ship match
        case Ship.Line(_, number, _, _, _, _) => number
      SnapshotCoordinate(number, sha.abbreviate).render

  def run(
      repo: Path,
      graph: ProjectGraph,
      machine: Machine,
  ): ZIO[IvyRoot, LocalPublishError, PublishRun.Report] =
    for
      sha <- histos.SnapshotId.head(repo).someOrFail(LocalPublishError.NoCommit)
      names = revisions(graph.catalog.ships, sha)
      root <- ZIO.service[IvyRoot]
      ivy  <- ZIO.foreach(names)(name =>
        IvyLocal.removeRevision(root, name).mapError(error => LocalPublishError.Stock(error.message))
      )
      tools <- ZIO.foreach(names)(name => toolchain(Path.of(machine.userHome), name))
      gone  = ivy.flatten ++ tools.flatten
      lines =
        if gone.isEmpty then List("nothing local to withdraw")
        else gone.map(path => s"withdrew $path")
    yield PublishRun.Report(lines)

  /** The installed launcher for one snapshot id. A missing directory is nothing to remove. */
  private def toolchain(home: Path, id: String): IO[LocalPublishError, Option[Path]] =
    if id.contains('/') || id.contains('\\') || id == "." || id == ".." then
      ZIO.fail(LocalPublishError.Stock(s"'$id' is not a toolchain id"))
    else
      val dir = home.resolve(".histos").resolve("toolchains").resolve(id)
      ZIO
        .attemptBlocking {
          if Files.isDirectory(dir) then
            deleteTree(dir)
            Some(dir)
          else None
        }
        .mapError(_ => LocalPublishError.Io(dir.toString))

  private def deleteTree(path: Path): Unit =
    if Files.isDirectory(path) then
      val stream = Files.list(path)
      try stream.iterator.asScala.foreach(deleteTree)
      finally stream.close()
    Files.deleteIfExists(path)
end LocalWithdraw
