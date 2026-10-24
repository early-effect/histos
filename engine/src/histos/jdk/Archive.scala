package histos.jdk

import histos.process.Child
import histos.workspace.IoFailure

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** What a locked archive's first bytes say it is. */
enum ArchiveKind:
  case Zip, TarGz, TarXz

object ArchiveKind:
  def of[E](archive: Path, failed: E): IO[E, Option[ArchiveKind]] =
    ZIO
      .attemptBlocking {
        val input = Files.newInputStream(archive)
        try input.readNBytes(6).toList.map(_ & 0xff)
        finally input.close()
      }
      .orElseFail(failed)
      .map:
        case 0x50 :: 0x4b :: _ => Some(ArchiveKind.Zip)
        case 0x1f :: 0x8b :: _ => Some(ArchiveKind.TarGz)
        case 0xfd :: 0x37 :: 0x7a :: 0x58 :: 0x5a :: 0x00 :: Nil =>
          Some(ArchiveKind.TarXz)
        case _ => None

/** Why the system `tar` did not unpack an archive. */
enum ExtractFailure:
  case NoTar
  case Start
  case Wait
  case Exit(code: Int)
  case Malformed
  case Io(failure: IoFailure)

  def message(archive: Path): String = this match
    case NoTar       => s"no tar on PATH to unpack $archive"
    case Start       => s"tar did not start for $archive"
    case Wait        => s"tar did not finish unpacking $archive"
    case Exit(code)  => s"tar exited $code unpacking $archive"
    case Malformed   => s"$archive is malformed"
    case Io(failure) => failure.message

/** The system `tar` keeps modes and symlinks, and `-xf` picks gzip or xz from
  * the archive itself.
  */
object Tar:
  def extract(
      archive: Path,
      into: Path,
      machine: Machine
  ): IO[ExtractFailure, Unit] =
    for
      tar <- onPath(machine)
      ended <- Child.run(
        start(tar, archive, into, machine),
        ExtractFailure.Wait
      )
      _ <- ZIO.unless(ended.status == 0)(
        ZIO.fail(ExtractFailure.Exit(ended.status))
      )
    yield ()

  private def onPath(machine: Machine): IO[ExtractFailure, Path] =
    val name = machine.os match
      case Os.Windows                   => "tar.exe"
      case Os.Mac | Os.Linux | Os.Other => "tar"
    ZIO
      .attemptBlocking {
        machine.env
          .get("PATH")
          .toList
          .flatMap(_.split(java.io.File.pathSeparator).toList)
          .filter(_.nonEmpty)
          .map(dir => Path.of(dir).resolve(name))
          .find(Files.isExecutable)
      }
      .orElseFail(ExtractFailure.NoTar)
      .someOrFail(ExtractFailure.NoTar)

  /** GNU tar runs `gzip` or `xz` from `PATH`, so a missing one is a non-zero
    * exit. Nothing else from the environment reaches it.
    */
  private def start(
      tar: Path,
      archive: Path,
      into: Path,
      machine: Machine
  ): IO[ExtractFailure, Process] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder(
          tar.toString,
          "-xf",
          archive.toString,
          "-C",
          into.toString
        )
        val env = builder.environment()
        env.clear()
        machine.env.get("PATH").foreach(path => env.put("PATH", path))
        builder
          .redirectOutput(ProcessBuilder.Redirect.DISCARD)
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
      }
      .orElseFail(ExtractFailure.Start)
