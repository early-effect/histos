package histos.doctor

import histos.Platform
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import com.typesafe.tools.mima.core.{MemberProblem, MissingClassProblem, Problem, TemplateProblem}
import com.typesafe.tools.mima.lib.MiMaLib
import zio.{IO, Layer, Scope, ZIO, ZLayer}

final case class CompatibilityInput(backend: Platform, previous: Path, current: Path)

enum CompatibilityProblem:
  case MissingClass(name: String)
  case Template(description: String)
  case Member(description: String)

enum CompatibilityResult:
  case Compatible
  case Incompatible(problems: ::[CompatibilityProblem])

enum CompatibilityError:
  case InitializationFailed(cause: Throwable)
  case UnsupportedBackend(backend: Platform)
  case MissingPrevious(path: Path)
  case InvalidPrevious(path: Path)
  case MissingCurrent(path: Path)
  case InvalidBaselineEntry(path: Path, entry: String)
  case PreparationFailed(input: CompatibilityInput, error: IoFailure)
  case AnalysisFailed(input: CompatibilityInput, cause: Throwable)

/** Analyzes backward binary compatibility, independently of registry access and version-bump policy.
  *
  * The current JVM implementation compares released JVM classfiles with current classfiles using MiMa. It does not
  * prove source, behavioral, TASTy, Scala.js IR, or Native ABI compatibility. Future JS/Native implementations can
  * implement this service with backend-specific evidence; the JVM implementation explicitly rejects those backends.
  */
trait CompatibilityAnalyzer:
  def analyze(input: CompatibilityInput): IO[CompatibilityError, CompatibilityResult]

object CompatibilityAnalyzer:
  val jvm: Layer[CompatibilityError, CompatibilityAnalyzer] = ZLayer.fromZIO(
    ZIO
      .attemptBlocking(new MiMaLib(Nil))
      .mapError(CompatibilityError.InitializationFailed(_))
      .map(JvmCompatibilityAnalyzer(_))
  )

/** The injected MiMa library is used only inside the blocking adapter effect. */
final class JvmCompatibilityAnalyzer(mima: MiMaLib) extends CompatibilityAnalyzer:
  def analyze(input: CompatibilityInput): IO[CompatibilityError, CompatibilityResult] =
    input.backend match
      case Platform.Js | Platform.Native => ZIO.fail(CompatibilityError.UnsupportedBackend(input.backend))
      case Platform.Jvm                  =>
        ZIO.scoped:
          for
            _ <- ZIO
              .attemptBlocking:
                if !Files.exists(input.previous) then Left(CompatibilityError.MissingPrevious(input.previous))
                else if !Files.isDirectory(input.current) then Left(CompatibilityError.MissingCurrent(input.current))
                else Right(())
              .mapError(CompatibilityError.AnalysisFailed(input, _))
              .flatMap(ZIO.fromEither(_))
            previous <- classfiles(input)
            result   <- ZIO
              .attemptBlockingInterrupt:
                mima.collectProblems(previous.toFile, input.current.toFile, Nil).map(problem) match
                  case Nil          => CompatibilityResult.Compatible
                  case head :: tail => CompatibilityResult.Incompatible(::(head, tail))
              .mapError(CompatibilityError.AnalysisFailed(input, _))
          yield result

  // MiMa opens jar filesystems without closing them. Give it a scoped classfile directory instead.
  private def classfiles(input: CompatibilityInput): ZIO[Scope, CompatibilityError, Path] =
    ZIO
      .attemptBlocking(Files.isDirectory(input.previous))
      .mapError(CompatibilityError.AnalysisFailed(input, _))
      .flatMap: directory =>
        if directory then ZIO.succeed(input.previous)
        else if !Option(input.previous.getFileName).exists(_.toString.endsWith(".jar")) then
          ZIO.fail(CompatibilityError.InvalidPrevious(input.previous))
        else
          for
            root <- Workspace.work("histos-mima").mapError(CompatibilityError.PreparationFailed(input, _))
            jar  <- ZIO.acquireRelease(
              ZIO
                .attemptBlocking(new ZipFile(input.previous.toFile))
                .mapError(CompatibilityError.AnalysisFailed(input, _))
            )(jar => ZIO.attemptBlocking(jar.close()).orDie)
            entries <- ZIO
              .attemptBlocking(jar.entries().asScala.map[java.util.zip.ZipEntry](identity).toList)
              .mapError(CompatibilityError.AnalysisFailed(input, _))
            _ <- ZIO.foreachDiscard(entries.filter(entry => !entry.isDirectory && entry.getName.endsWith(".class"))):
              entry =>
                val destination = root.resolve(entry.getName).normalize()
                if !destination.startsWith(root) then
                  ZIO.fail(CompatibilityError.InvalidBaselineEntry(input.previous, entry.getName))
                else
                  ZIO.scoped:
                    for
                      source <- ZIO.acquireRelease(
                        ZIO
                          .attemptBlocking(jar.getInputStream(entry))
                          .mapError(CompatibilityError.AnalysisFailed(input, _))
                      )(source => ZIO.attemptBlocking(source.close()).orDie)
                      _ <- ZIO
                        .attemptBlockingInterrupt:
                          Option(destination.getParent).foreach(Files.createDirectories(_))
                          Files.copy(source, destination)
                        .mapError(CompatibilityError.AnalysisFailed(input, _))
                    yield ()
          yield root

  private def problem(found: Problem): CompatibilityProblem =
    found match
      case missing: MissingClassProblem => CompatibilityProblem.MissingClass(missing.ref.fullName)
      case template: TemplateProblem    => CompatibilityProblem.Template(template.description("current"))
      case member: MemberProblem        => CompatibilityProblem.Member(member.description("current"))
end JvmCompatibilityAnalyzer
