package histos.format

import histos.jdk.{Sha256, Sha256Digest}
import histos.workspace.{IoFailure, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{Chunk, IO}

/** Files known formatted under one key: the scalafmt jars and the configuration bytes. A file is skipped only while its
  * bytes still hash to the recorded digest. A different key forgets every entry.
  */
final case class FormatLedger(key: Sha256, formatted: Map[Path, Sha256]):
  def holds(file: Path, digest: Sha256): Boolean = formatted.get(file).contains(digest)

  /** This run's verdicts replace the earlier ones for the files it saw. A file it saw and did not prove formatted is
    * forgotten.
    */
  def after(seen: Set[Path], proved: Map[Path, Sha256]): FormatLedger =
    FormatLedger(key, formatted.view.filterKeys(file => !seen.contains(file)).toMap ++ proved)

  def render: String =
    (s"key ${key.value}" :: formatted.toList.sortBy(_._1.toString).map((file, digest) => s"${digest.value} $file"))
      .mkString("", "\n", "\n")
end FormatLedger

object FormatLedger:
  def path(repo: Path): Path = repo.resolve(".histos").resolve("format").resolve("ledger")

  def key(jars: FormatterJars, config: String): Sha256 =
    digest(s"${jars.dynamic.fingerprint.value}\n${jars.core.fingerprint.value}\n$config")

  def digest(text: String): Sha256 =
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  /** A missing, unreadable-as-text, or other-key ledger is empty. It is a cache, so a bad line drops only that line. */
  def parse(key: Sha256, text: String): FormatLedger =
    text.linesIterator.toList match
      case head :: rows if head == s"key ${key.value}" =>
        FormatLedger(key, rows.flatMap(row).toMap)
      case _ => FormatLedger(key, Map.empty)

  def read(repo: Path, key: Sha256): IO[IoFailure, FormatLedger] =
    Workspace.readIfPresent(path(repo)).map(found => found.fold(FormatLedger(key, Map.empty))(parse(key, _)))

  def write(repo: Path, ledger: FormatLedger): IO[IoFailure, Unit] =
    Workspace.createDirectories(path(repo).getParent) *> Workspace.writeString(path(repo), ledger.render)

  private def row(line: String): Option[(Path, Sha256)] =
    line.split(" ", 2) match
      case Array(hex, file) =>
        for
          digest <- Sha256.from(hex).toOption
          path   <- scala.util.Try(Path.of(file)).toOption
        yield path -> digest
      case _ => None
end FormatLedger
