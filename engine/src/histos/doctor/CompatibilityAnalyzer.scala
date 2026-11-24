package histos.doctor

import histos.Platform
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import com.typesafe.tools.mima.core.{MemberProblem, MissingClassProblem, Problem, TemplateProblem}
import com.typesafe.tools.mima.lib.MiMaLib
import org.objectweb.asm.{Attribute, ClassReader, ClassVisitor, ClassWriter, Opcodes}
import zio.{Exit, IO, Layer, Scope, ZIO, ZLayer}

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
  /** Each analysis builds its own library. One shared instance is not safe across fibers. */
  val jvm: Layer[Nothing, CompatibilityAnalyzer] = ZLayer.succeed(JvmCompatibilityAnalyzer())

/** MiMa is constructed inside each analysis. Fibers do not share one instance. */
final class JvmCompatibilityAnalyzer extends CompatibilityAnalyzer:
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
            mima     <- ZIO
              .attemptBlocking(new MiMaLib(Nil))
              .mapError(CompatibilityError.InitializationFailed(_))
            result <- ZIO
              .attemptBlockingInterrupt:
                mima.collectProblems(previous.toFile, input.current.toFile, Nil).map(problem) match
                  case Nil          => CompatibilityResult.Compatible
                  case head :: tail => CompatibilityResult.Incompatible(::(head, tail))
              .mapError(CompatibilityError.AnalysisFailed(input, _))
          yield result

  // MiMa opens jar filesystems without closing them. Scala 3 classfiles also require their TASTy sidecars.
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
            jar  <- ZIO
              .attemptBlocking(new ZipFile(input.previous.toFile))
              .mapError(CompatibilityError.AnalysisFailed(input, _))
            extracted <- explode(input, root, jar).exit
            closed    <- closeOf(input)(jar.close()).exit
            path      <- joined(extracted, closed)
          yield path

  private def explode(input: CompatibilityInput, root: Path, jar: ZipFile): IO[CompatibilityError, Path] =
    for
      entries <- ZIO
        .attemptBlocking(jar.entries().asScala.map[java.util.zip.ZipEntry](identity).toList)
        .mapError(CompatibilityError.AnalysisFailed(input, _))
      _ <- ZIO.foreachDiscard(
        entries.filter(entry =>
          !entry.isDirectory && (entry.getName.endsWith(".class") || entry.getName.endsWith(".tasty"))
        )
      ): entry =>
        ZIO
          .attempt(root.resolve(entry.getName).normalize())
          .mapError(_ => CompatibilityError.InvalidBaselineEntry(input.previous, entry.getName))
          .flatMap: destination =>
            if !destination.startsWith(root) then
              ZIO.fail(CompatibilityError.InvalidBaselineEntry(input.previous, entry.getName))
            else
              for
                source <- ZIO
                  .attemptBlocking(jar.getInputStream(entry))
                  .mapError(CompatibilityError.AnalysisFailed(input, _))
                copied <- ZIO
                  .attemptBlockingInterrupt:
                    Option(destination.getParent).foreach(Files.createDirectories(_))
                    val name   = entry.getName
                    val absent = !Files.isRegularFile(input.current.resolve(name))
                    if name.endsWith(".class") && absent then
                      Files.write(destination, missingClass(source.readAllBytes()))
                    else Files.copy(source, destination)
                    ()
                  .mapError(CompatibilityError.AnalysisFailed(input, _))
                  .exit
                closed <- closeOf(input)(source.close()).exit
                _      <- joined(copied, closed)
              yield ()
    yield root

  /** A close failure is an analysis failure for this input. The body failure wins when both fail. */
  private def closeOf(input: CompatibilityInput)(close: => Unit): IO[CompatibilityError, Unit] =
    ZIO.attemptBlocking(close).mapError(CompatibilityError.AnalysisFailed(input, _))

  private def joined[A](
      body: Exit[CompatibilityError, A],
      closed: Exit[CompatibilityError, Unit],
  ): IO[CompatibilityError, A] =
    (body, closed) match
      case (Exit.Success(value), Exit.Success(_)) => ZIO.succeed(value)
      case (Exit.Success(_), Exit.Failure(cause)) => ZIO.failCause(cause)
      case (Exit.Failure(cause), _)               => ZIO.failCause(cause)

  // Absent classes still produce JVM problems. TASTy can only narrow their visibility, not restore them.
  private def missingClass(bytes: Array[Byte]): Array[Byte] =
    val reader  = new ClassReader(bytes)
    val writer  = new ClassWriter(reader, 0)
    val visitor = new ClassVisitor(Opcodes.ASM9, writer):
      override def visitAttribute(attribute: Attribute): Unit =
        if attribute.`type` != "TASTY" then super.visitAttribute(attribute)
    reader.accept(visitor, 0)
    writer.toByteArray

  private def problem(found: Problem): CompatibilityProblem =
    found match
      case missing: MissingClassProblem => CompatibilityProblem.MissingClass(missing.ref.fullName)
      case template: TemplateProblem    => CompatibilityProblem.Template(template.description("current"))
      case member: MemberProblem        => CompatibilityProblem.Member(member.description("current"))
end JvmCompatibilityAnalyzer
