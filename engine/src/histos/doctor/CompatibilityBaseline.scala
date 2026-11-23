package histos.doctor

import histos.central.MavenModule
import histos.version.ShipNumber

import java.nio.file.Path

import zio.{IO, ULayer, ZIO, ZLayer}

final case class BaselineCoordinate(module: MavenModule, revision: ShipNumber)

enum BaselineError:
  case Unavailable(coordinate: BaselineCoordinate, cause: Throwable)
  case MissingJar(coordinate: BaselineCoordinate)

/** Resolves one exact released artifact. Listing releases and judging bump policy belong to the caller. */
trait CompatibilityBaseline:
  def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path]

object CompatibilityBaseline:
  val live: ULayer[CompatibilityBaseline] = ZLayer.succeed(CoursierCompatibilityBaseline())

final class CoursierCompatibilityBaseline(
    repositories: Option[Seq[coursier.Repository]] = None
) extends CompatibilityBaseline:
  def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] =
    ZIO
      .attemptBlockingInterrupt:
        val dependency = coursier
          .Dependency(
            coursier.Module(
              coursier.Organization(coordinate.module.group.value),
              coursier.ModuleName(coordinate.module.artifact.value),
            ),
            coursier.version.VersionConstraint(coordinate.revision.render),
          )
          .copy(transitive = false)
        val requested = coursier.Fetch().addDependencies(dependency)
        repositories.fold(requested)(requested.withRepositories(_)).run().toList
      .mapError(BaselineError.Unavailable(coordinate, _))
      .flatMap: files =>
        val expected = s"${coordinate.module.artifact.value}-${coordinate.revision.render}.jar"
        ZIO
          .fromOption(
            files
              .map(_.toPath)
              .find: path =>
                Option(path.getFileName).exists(_.toString == expected)
          )
          .orElseFail(BaselineError.MissingJar(coordinate))
