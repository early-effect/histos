package histos.zinc

import histos.jdk.{Sha256, Sha256Digest}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.OptionConverters.*

import sbt.internal.inc.FileAnalysisStore

import zio.Chunk

/** One upstream class and the public signature Zinc recorded for it. The binary name is the name a caller links. */
opaque type BinaryName = String

object BinaryName:
  def apply(raw: String): BinaryName = raw

  extension (name: BinaryName)
    def value: String = name

    /** Every name this class is stored under. A top-level object is `Lib` in one relation and `Lib$` in another. */
    def forms: Set[BinaryName] =
      val plain = if name.endsWith("$") then BinaryName(name.dropRight(1)) else name
      Set(name, plain, BinaryName(plain.value + "$"))
  end extension
end BinaryName

/** Zinc's `apiHash`. A method body does not change it. A parameter list does. */
opaque type ApiHash = Int

object ApiHash:
  def apply(raw: Int): ApiHash = raw

  extension (hash: ApiHash) def value: Int = hash
end ApiHash

final case class ClassSignature(name: BinaryName, hash: ApiHash)

/** What changed between the signature table this cell compiled against and the one it sees now. */
enum UpstreamChange:
  /** The tables name the same classes with the same hashes. */
  case Same

  /** Every changed class is one Zinc's external invalidation will see, and every dropped class is one this cell does
    * not reference.
    */
  case Precise

  /** A changed class this cell never recorded, an upstream analysis the lookup cannot read, or a cell with no stored
    * table. The whole cell compiles again.
    */
  case Unproven
end UpstreamChange

/** The public signature of every class on this compile's upstream classpath. The digest is a compile input, so a
  * changed signature cannot restore a stored result.
  */
object UpstreamApis:
  /** Binary names the analysis recorded a use of. These are source class names: `Lib`, not the `Lib$` file. */
  def referenced(dir: Path): Option[Set[BinaryName]] =
    load(dir).map(analysis => analysis.relations.allExternalDeps.iterator.map(BinaryName(_)).toSet)

  /** Classes defined by the analysis at `dir`, or nothing when that analysis is absent. */
  def readAnalysis(dir: Path): Option[List[ClassSignature]] =
    load(dir).map(analysis => of(List(analysis)))

  private def load(dir: Path): Option[sbt.internal.inc.Analysis] =
    val zip = dir.resolve("inc.zip")
    if !Files.isRegularFile(zip) then None
    else
      FileAnalysisStore
        .binary(zip.toFile)
        .get()
        .toScala
        .flatMap: contents =>
          contents.getAnalysis match
            case analysis: sbt.internal.inc.Analysis => Some(analysis)
            case _                                   => None

  /** Classes defined by `analyses`, in binary-name order. A binary name defined twice keeps the first. */
  def of(analyses: List[sbt.internal.inc.Analysis]): List[ClassSignature] =
    analyses
      .flatMap(signatures)
      .groupBy(_.name)
      .toList
      .sortBy(_._1.value)
      .flatMap(_._2.headOption)

  val fileName: String = "upstream.tsv"

  /** The table a previous compile wrote. `None` is a missing file, an unreadable file, or a row this writer would not
    * produce. A cell compiled before this table existed is [[UpstreamChange.Unproven]].
    */
  def read(dir: Path): Option[List[ClassSignature]] =
    val path = dir.resolve(fileName)
    try Some(parse(Files.readString(path, StandardCharsets.UTF_8))).filter(_.isDefined).flatten
    catch case _: IOException => None

  /** Writes the table the next compile compares. An empty table is still a table: it records that this compile saw no
    * upstream classes.
    */
  def store(dir: Path, table: List[ClassSignature]): Either[CompileError, Unit] =
    val path = dir.resolve(fileName)
    try
      Files.createDirectories(dir)
      Files.writeString(path, render(table), StandardCharsets.UTF_8)
      Right(())
    catch case _: IOException => Left(CompileError.Io(histos.workspace.IoFailure(path, histos.workspace.IoFault.Write)))

  def digest(table: List[ClassSignature]): Sha256 =
    val text = table.map(row => s"${row.name.value} ${row.hash.value}").mkString("\n")
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  /** Binary names whose hash differs. An absent stored table has no names to name. */
  def changedNames(stored: List[ClassSignature], now: List[ClassSignature]): Set[BinaryName] =
    val prior = stored.map(row => row.name -> row.hash).toMap
    now
      .collect:
        case ClassSignature(name, hash) if prior.get(name).exists(_ != hash) => name
      .toSet

  /** `stored` is the table the previous compile wrote. `now` is what the classpath says today. `external` answers
    * whether the previous analysis recorded a use of a binary name: a member reference, an inheritance, or a macro
    * expansion. `present` is every binary name an upstream analysis still defines. A stored table is required: its
    * absence is [[UpstreamChange.Unproven]].
    */
  def decide(
      stored: Option[List[ClassSignature]],
      now: List[ClassSignature],
      external: BinaryName => Boolean,
      present: Set[BinaryName],
  ): UpstreamChange =
    stored match
      case None          => UpstreamChange.Unproven
      case Some(earlier) =>
        val prior   = earlier.map(row => row.name -> row.hash).toMap
        val current = now.map(row => row.name -> row.hash).toMap
        val changed = current.collect:
          case (name, hash) if prior.get(name).exists(_ != hash) => name
        val added    = current.keySet -- prior.keySet
        val dropped  = prior.keySet -- current.keySet
        val recorded = (name: BinaryName) => name.forms.exists(external)
        if changed.isEmpty && added.isEmpty && dropped.isEmpty then UpstreamChange.Same
        else if changed.forall(name => present.contains(name) && recorded(name)) && dropped.forall(name =>
            !recorded(name)
          )
        then UpstreamChange.Precise
        else UpstreamChange.Unproven

  private def render(table: List[ClassSignature]): String =
    table.map(row => s"${row.name.value}\t${row.hash.value}").mkString("\n") + "\n"

  private def parse(text: String): Option[List[ClassSignature]] =
    val rows = text.linesIterator.filter(_.nonEmpty).toList
    rows.foldLeft(Option(List.empty[ClassSignature])):
      case (None, _)         => None
      case (Some(acc), line) =>
        line.split('\t') match
          case Array(name, raw) if name.nonEmpty && !name.contains(' ') =>
            raw.toIntOption.map(hash => acc :+ ClassSignature(BinaryName(name), ApiHash(hash)))
          case _ => None

  /** `productClassName` points a source class at the binary names its compilation wrote. The hash belongs to the source
    * class, and every binary name it produced carries that hash.
    */
  private def signatures(analysis: sbt.internal.inc.Analysis): List[ClassSignature] =
    val product = analysis.relations.productClassName
    product._1s.iterator.flatMap { source =>
      val hash = analysis.apis.internalAPI(source).apiHash()
      product.forward(source).iterator.map(binary => ClassSignature(BinaryName(binary), ApiHash(hash)))
    }.toList

end UpstreamApis
