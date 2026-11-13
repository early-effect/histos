package histos.publish

import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, ProjectGraph, Publication}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** `histos`, rendered from the ship number the way a workflow file is. The checkout script execs the user shim. The
  * shim chooses the local toolchain or the pin. Histos does not install a JDK.
  */
object LauncherScript:
  def check(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO
      .fromEither(ship(graph))
      .flatMap:
        case None          => ZIO.unit
        case Some(version) =>
          LauncherPin.read(repo).mapError(ScriptError.Pin(_)) *>
            agree(repo.resolve("histos"), posix(version))

  def update(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO
      .fromEither(ship(graph))
      .flatMap:
        case None          => ZIO.unit
        case Some(version) =>
          LauncherPin.read(repo).mapError(ScriptError.Pin(_)) *>
            write(
              repo.resolve("histos"),
              posix(version),
              executable = true,
            )

  def posix(version: String): String =
    Posix.replace("__VERSION__", version)

  private def ship(graph: ProjectGraph): Either[ScriptError, Option[String]] =
    if !launcher(graph) then Right(None)
    else
      versions(graph.catalog) match
        case List(version) => Right(Some(version))
        case _             => Left(ScriptError.Revision)

  /** The next release number. One distinct value. Session coordinates stay out of the script. */
  private def versions(catalog: Catalog): List[String] =
    catalog match
      case Catalog.Empty       => Nil
      case Catalog.Lines(rows) => rows.toList.map(_.number.render).distinct

  private def launcher(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case module: Module =>
        module.publication match
          case Publication.Assembly(_) => true
          case _                       => false
      case _ => false

  private def agree(path: Path, body: String): IO[ScriptError, Unit] =
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(path) then Left(ScriptError.Missing(path))
        else if Files.readString(path) == body then Right(())
        else Left(ScriptError.Drift(path))
      }
      .mapError(_ => ScriptError.Io(IoFailure(path, IoFault.Read)))
      .flatMap(ZIO.fromEither)

  private def write(
      path: Path,
      body: String,
      executable: Boolean,
  ): IO[ScriptError, Unit] =
    ZIO
      .attemptBlocking {
        val same = Files.isRegularFile(path) && Files.readString(path) == body
        if !same then Files.writeString(path, body)
        if executable then path.toFile.setExecutable(true)
      }
      .mapError(_ => ScriptError.Io(IoFailure(path, IoFault.Write)))

  private val Posix: String = """#!/bin/sh
# Generated from the ship number. `histos workflow --update` rewrites this file.
# The checkout script execs the user shim. The shim chooses the local toolchain or the pin.
set -eu
version=__VERSION__
fail() { echo "histos $version: $*" >&2; exit 1; }
os=$(uname -s 2>/dev/null || true)
case $os in
  MINGW*|MSYS*|CYGWIN*|Windows*)
    fail "Histos runs on macOS and Linux. On Windows, use WSL. A native Windows build may come later."
    ;;
esac
home=${HISTOS_HOME:-$HOME/.histos}
shim=$home/bin/histos
exec "$shim" "$@"
"""
end LauncherScript
