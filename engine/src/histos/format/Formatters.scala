package histos.format

import histos.jdk.Sha256
import histos.workspace.Workspace

import java.nio.file.Path

import zio.{IO, Ref, Scope, UIO, ZIO, ZLayer}

/** Formats a selection with the locked scalafmt. One scalafmt-dynamic stays loaded for the jars it was opened with, so
  * a second call in the same server pays no class loading. Resolution happens before this service.
  */
trait Formatters:
  def format(request: FormatRequest): IO[FormatError, FormatReport]

  /** How many times a scalafmt-dynamic was loaded. */
  def openings: UIO[Int]
end Formatters

object Formatters:
  def format(request: FormatRequest): ZIO[Formatters, FormatError, FormatReport] =
    ZIO.serviceWithZIO[Formatters](_.format(request))

  /** The loaded scalafmt is released when `Scope` closes. */
  val make: ZIO[Scope, Nothing, Formatters] =
    for
      held   <- Ref.Synchronized.make(Option.empty[(Sha256, ScalafmtEngine)])
      opened <- Ref.make(0)
      _      <- ZIO.addFinalizer(held.get.flatMap(current => ZIO.foreachDiscard(current)(_._2.release)))
    yield FormattersLive(held, opened)

  val live: ZLayer[Any, Nothing, Formatters] = ZLayer.scoped(make)
end Formatters

private final class FormattersLive(
    held: Ref.Synchronized[Option[(Sha256, ScalafmtEngine)]],
    opened: Ref[Int],
) extends Formatters:
  def openings: UIO[Int] = opened.get

  def format(request: FormatRequest): IO[FormatError, FormatReport] =
    val FormatRequest(root, config, selection, mode, jars) = request
    val repo                                               = root.toAbsolutePath.normalize
    for
      text     <- Workspace.readString(repo.resolve(config)).mapError(FormatError.Io(_))
      engine   <- engineFor(jars)
      session  <- engine.session(repo, config)
      narrowed <- candidates(session, selection)
      filtered <- ZIO.foreach(narrowed)(file => session.included(file).either.map(file -> _))
      kept    = filtered.collect { case (file, Right(true)) => file }
      refused = filtered.collect { case (_, Left(fault)) => fault }
      key     = FormatLedger.key(jars, text)
      ledger   <- FormatLedger.read(repo, key).mapError(FormatError.Io(_))
      width    <- ZIO.succeed(java.lang.Runtime.getRuntime.availableProcessors.max(1))
      outcomes <- ZIO.foreachPar(kept)(file => one(repo, session, ledger, mode, file)).withParallelism(width)
      proved = outcomes.flatMap(_.proved).toMap
      _ <- FormatLedger.write(repo, ledger.after(kept.toSet, proved)).mapError(FormatError.Io(_))
    yield FormatReport(
      mode,
      outcomes.collect { case Outcome.Changed(file) => file },
      outcomes.count { case Outcome.Unchanged(_, _) => true; case _ => false },
      outcomes.count { case Outcome.Skipped(_, _) => true; case _ => false },
      selection.files.length - kept.length - refused.length,
      refused ++ outcomes.collect { case Outcome.Faulted(fault) => fault },
    )

  /** The downloader inside an engine holds the roots, so they are part of its key. */
  private def engineFor(jars: FormatterJars): IO[FormatError, ScalafmtEngine] =
    val key = FormatLedger.digest(
      (jars.dynamic.fingerprint.value :: jars.core.fingerprint.value :: jars.roots.map(_.shown)).mkString("\n")
    )
    held.modifyZIO:
      case Some((current, engine)) if current == key => ZIO.succeed((engine, Some((current, engine))))
      case prior                                     =>
        ZIO.foreachDiscard(prior)(_._2.release) *>
          ScalafmtEngine.open(jars).tap(_ => opened.update(_ + 1)).map(engine => (engine, Some((key, engine))))

  private def candidates(session: FormatSession, selection: FormatSelection): IO[FormatError, List[Path]] =
    selection.git match
      case GitFilter.Named   => ZIO.succeed(selection.files)
      case GitFilter.Tracked =>
        session.gitOnly.flatMap: only =>
          if only then session.tracked.map(tracked => selection.files.filter(tracked.contains))
          else ZIO.succeed(selection.files)

  private def one(
      repo: Path,
      session: FormatSession,
      ledger: FormatLedger,
      mode: FormatMode,
      file: Path,
  ): UIO[Outcome] =
    val formatted =
      for
        code <- Workspace.readString(repo.resolve(file)).mapError(FormatFault.Read(_))
        digest = FormatLedger.digest(code)
        outcome <-
          if ledger.holds(file, digest) then ZIO.succeed(Outcome.Skipped(file, digest))
          else
            session
              .format(file, code)
              .flatMap: out =>
                if out == code then ZIO.succeed(Outcome.Unchanged(file, digest))
                else
                  mode match
                    case FormatMode.Check => ZIO.succeed(Outcome.Changed(file))
                    case FormatMode.Write =>
                      Workspace
                        .writeString(repo.resolve(file), out)
                        .mapBoth(FormatFault.Write(_), _ => Outcome.Changed(file))
      yield outcome
    formatted.fold(Outcome.Faulted(_), identity)
end FormattersLive

/** One file's verdict. `proved` is what the ledger may record as formatted. A file this run wrote is not proved:
  * scalafmt is not idempotent on every input, so only a later run that leaves the bytes alone proves it.
  */
private enum Outcome:
  case Skipped(file: Path, digest: Sha256)
  case Unchanged(file: Path, digest: Sha256)
  case Changed(file: Path)
  case Faulted(fault: FormatFault)

  def proved: Option[(Path, Sha256)] = this match
    case Skipped(file, digest)   => Some(file -> digest)
    case Unchanged(file, digest) => Some(file -> digest)
    case Changed(_)              => None
    case Faulted(_)              => None
end Outcome
