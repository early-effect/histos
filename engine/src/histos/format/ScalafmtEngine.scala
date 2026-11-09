package histos.format

import org.scalafmt.interfaces.{
  PositionException,
  RepositoryPackage,
  RepositoryPackageDownloader,
  RepositoryPackageDownloaderFactory,
  RepositoryProperties,
  Scalafmt,
  ScalafmtClassLoader,
  ScalafmtReporter,
  ScalafmtSession,
}

import java.io.{File, OutputStream, OutputStreamWriter, PrintWriter}
import java.net.URLClassLoader
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, ZIO}

/** One loaded scalafmt-dynamic. Only `org.scalafmt.interfaces` crosses into its loader. The core jars come from the
  * lock through [[LockedDownloader]], so loading never resolves.
  */
private[format] final class ScalafmtEngine private (scalafmt: Scalafmt, downloader: LockedDownloader):
  def session(repo: Path, config: Path): IO[FormatError, FormatSession] =
    ZIO
      .attemptBlocking(scalafmt.createSession(repo.resolve(config)))
      .mapError: thrown =>
        downloader.drift match
          case Some(asked) => FormatError.CoreDrift(asked, downloader.locked)
          case None        => FormatError.Config(config, ScalafmtEngine.text(thrown))
      .flatMap(found => ZIO.fromOption(Option(found)).orElseFail(FormatError.Config(config, "no session")))
      .map(FormatSession(repo, _))

  /** `clear` closes the core loaders on scalafmt's own pool. Closing this loader as well races that close, so the
    * dynamic loader is left for the collector.
    */
  def release: UIO[Unit] = ZIO.attemptBlocking(scalafmt.clear()).ignore
end ScalafmtEngine

private[format] object ScalafmtEngine:
  def open(jars: FormatterJars): IO[FormatError, ScalafmtEngine] =
    val downloader = LockedDownloader(jars.roots, jars.core.artifacts.map(_.path.toFile))
    ZIO
      .attemptBlocking:
        val urls   = jars.dynamic.artifacts.map(_.path.toUri.toURL).toArray
        val loader = URLClassLoader(urls, ScalafmtClassLoader(classOf[Scalafmt].getClassLoader))
        Scalafmt.create(loader).withReporter(SilentReporter).withRespectProjectFilters(true)
      .mapError(thrown => FormatError.Load(text(thrown)))
      .flatMap: created =>
        val injected = created.withRepositoryPackageDownloader(downloader)
        if injected eq created then ZIO.fail(FormatError.Load("this scalafmt-dynamic does not take a downloader"))
        else ZIO.succeed(ScalafmtEngine(injected, downloader))

  private[format] def text(thrown: Throwable): String =
    Option(thrown.getMessage).filter(_.nonEmpty).getOrElse(thrown.getClass.getName)
end ScalafmtEngine

/** Every error already comes back through `formatOrError` or a failed `createSession`. */
private[format] object SilentReporter extends ScalafmtReporter:
  def error(file: Path, message: String): Unit                  = ()
  def error(file: Path, e: Throwable): Unit                     = ()
  def excluded(file: Path): Unit                                = ()
  def parsedConfig(config: Path, scalafmtVersion: String): Unit = ()
  def downloadWriter(): PrintWriter                             = PrintWriter(OutputStream.nullOutputStream())
  def downloadOutputStreamWriter(): OutputStreamWriter          = OutputStreamWriter(OutputStream.nullOutputStream())

/** Hands scalafmt-dynamic the locked core jars when it asks for exactly the locked packages. Anything else gets no
  * jars, and the ask is kept so the failed session can name the drift. The callback is synchronous Java, so the ask
  * lives in an atomic reference.
  */
private[format] final class LockedDownloader(val locked: List[ScalafmtPackage], files: List[File])
    extends RepositoryPackageDownloaderFactory
    with RepositoryPackageDownloader:
  private val asked = AtomicReference(Option.empty[List[ScalafmtPackage]])

  def create(reporter: ScalafmtReporter, properties: RepositoryProperties): RepositoryPackageDownloader = this

  def download(
      scalaVersion: String,
      scalafmtVersion: String,
      reporter: ScalafmtReporter,
      dependencies: java.lang.Iterable[RepositoryPackage],
  ): java.lang.Iterable[File] =
    val wanted = dependencies.asScala.toList.map(found => ScalafmtPackage(found.group, found.artifact, found.version))
    if wanted.toSet == locked.toSet then files.asJava
    else
      asked.set(Some(wanted))
      List.empty[File].asJava

  def drift: Option[List[ScalafmtPackage]] = asked.get
end LockedDownloader

/** A session over one configuration. Calls are safe from several fibers. */
private[format] final class FormatSession(repo: Path, session: ScalafmtSession):
  def included(file: Path): IO[FormatFault, Boolean] =
    ZIO
      .attemptBlocking(session.matchesProjectFilters(repo.resolve(file)))
      .mapError(thrown => FormatFault.Failed(file, ScalafmtEngine.text(thrown)))

  def gitOnly: UIO[Boolean] = ZIO.attemptBlocking(session.isGitOnly).orElseSucceed(false)

  /** The git-tracked files the configuration's filters keep, relative to the repository. */
  def tracked: IO[FormatError, Set[Path]] =
    ZIO
      .attemptBlocking(session.listFiles(repo).asScala.toList)
      .mapBoth(thrown => FormatError.Tracked(ScalafmtEngine.text(thrown)), _.map(repo.relativize).toSet)

  def format(file: Path, code: String): IO[FormatFault, String] =
    ZIO
      .attemptBlocking(session.formatOrError(repo.resolve(file), code))
      .mapError(thrown => FormatFault.Failed(file, ScalafmtEngine.text(thrown)))
      .flatMap: result =>
        Option(result.value) match
          case Some(text) => ZIO.succeed(text)
          case None       => ZIO.fail(fault(file, Option(result.exception)))

  /** scalafmt-dynamic wraps a parse error, so the position is the error or its cause. Lines are 0-based there. */
  private def fault(file: Path, thrown: Option[Throwable]): FormatFault =
    thrown match
      case None        => FormatFault.Failed(file, "scalafmt returned neither text nor an error")
      case Some(found) =>
        (found :: Option(found.getCause).toList).collectFirst { case position: PositionException => position } match
          case Some(position) =>
            FormatFault.Parse(file, position.startLine + 1, position.startCharacter + 1, position.shortMessage)
          case None => FormatFault.Failed(file, ScalafmtEngine.text(found))
end FormatSession
