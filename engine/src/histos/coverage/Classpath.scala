package histos.coverage

import histos.zinc.ScalaSeries
import histos.ScalaVersion

import java.nio.file.Path

enum ClasspathGap:
  case MissingRuntime

object Classpath:
  /** Scala 2.13 gains the runtime jar. Scala 3 returns the classes unchanged, even when a jar is offered. */
  def tests(
      classes: List[Path],
      version: ScalaVersion,
      runtime: Option[Path],
  ): Either[ClasspathGap, List[Path]] =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala2(13)) =>
        runtime match
          case Some(jar) => Right(classes :+ jar)
          case None      => Left(ClasspathGap.MissingRuntime)
      case _ => Right(classes)
