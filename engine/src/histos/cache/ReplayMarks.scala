package histos.cache

import histos.schedule.Digest
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.CompileReplay
import histos.{Platform, ProjectName, ScalaVersion}

import java.io.IOException
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** `replay.key` files a compile wrote. `analysis` is main classes. `test-analysis` is test classes. Coverage analysis
  * is the same shape under `.histos/coverage` and is only collected when asked.
  */
private[histos] object ReplayMarks:
  enum Kind:
    case Main
    case Test

  final case class Mark(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      kind: Kind,
      key: Digest,
      marker: Path,
  )

  def collect(
      repo: Path,
      wanted: Option[Set[ProjectName]],
      coverage: Boolean,
  ): IO[IoFailure, List[Mark]] =
    val histos = repo.resolve(".histos")
    val roots  = List(
      histos.resolve("analysis")      -> Kind.Main,
      histos.resolve("test-analysis") -> Kind.Test,
    ) ++ (if coverage then List(histos.resolve("coverage").resolve("analysis") -> Kind.Main) else Nil)
    ZIO
      .foreach(roots)((root, kind) => readRoot(root, kind))
      .map: found =>
        found.flatten.filter(mark => wanted.forall(_.contains(mark.module)))

  private def readRoot(root: Path, kind: Kind): IO[IoFailure, List[Mark]] =
    ZIO.attemptBlocking(scan(root, kind)).mapError(_ => IoFailure(root, IoFault.Walk)).flatMap(ZIO.fromEither)

  private def scan(root: Path, kind: Kind): Either[IoFailure, List[Mark]] =
    if !Files.isDirectory(root) then Right(Nil)
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.foldLeft[Either[IoFailure, List[Mark]]](Right(Nil)):
          case (Left(error), _)   => Left(error)
          case (Right(acc), path) =>
            val name = Option(path.getFileName).map(_.toString)
            if name.contains(CompileReplay.Marker) && Files.isRegularFile(path) then
              read(path, kind).map(found => found.fold(acc)(_ :: acc))
            else Right(acc)
      catch case _: IOException => Left(IoFailure(root, IoFault.Walk))
      finally walk.close()

  private def read(marker: Path, kind: Kind): Either[IoFailure, Option[Mark]] =
    val placed =
      for
        platformDir <- Option(marker.getParent)
        versionDir  <- Option(platformDir.getParent)
        moduleDir   <- Option(versionDir.getParent)
        platform    <- Option(platformDir.getFileName).map(_.toString).flatMap(Platform.from(_).toOption)
        version     <- Option(versionDir.getFileName).map(_.toString).flatMap(ScalaVersion.from(_).toOption)
        module      <- Option(moduleDir.getFileName).map(_.toString).flatMap(ProjectName.from(_).toOption)
      yield (module, version, platform)
    placed match
      case None                              => Right(None)
      case Some((module, version, platform)) =>
        try
          val text = Files.readString(marker).trim
          Right(
            Digest.from(text).map(key => Mark(module, version, platform, kind, key, marker))
          )
        catch case _: IOException => Left(IoFailure(marker, IoFault.Read))
end ReplayMarks
