package histos.zinc

import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import sbt.internal.inc.{FileAnalysisStore, MappedFileConverter, Stamp, Stamper}
import xsbti.VirtualFileRef

import zio.{UIO, ZIO}

/** Which analysis directory a cell writes. Main and test compiles do not share one.
  */
enum AnalysisKind:
  case Main, Test

/** What the last `inc.zip` says about this cell, without compiling.
  *
  * [[FileStaleness.Initial]] is a missing analysis: every listed source would be compiled. [[FileStaleness.Known]] was
  * read. An empty `stale` list is a fresh cell. [[FileStaleness.Unknown]] is an analysis that is present and could not
  * be read. A missing file is not unknown.
  */
enum FileStaleness:
  case Initial(sources: List[Path])
  case Known(stale: List[Path])
  case Unknown

/** Read source stamps and source dependencies out of an existing analysis. This does not call the compiler and does
  * not write class files or a new analysis.
  */
object SourceStale:
  def directory(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      kind: AnalysisKind,
  ): Path =
    val name = kind match
      case AnalysisKind.Main => "analysis"
      case AnalysisKind.Test => "test-analysis"
    repo
      .resolve(".histos")
      .resolve(name)
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  /** `sources` are this cell's current inputs, in the order a compile would see them. */
  def read(dir: Path, sources: List[Path]): UIO[FileStaleness] =
    ZIO.attemptBlocking(answer(dir, sources)).orDie

  private def answer(dir: Path, sources: List[Path]): FileStaleness =
    val zip = dir.resolve("inc.zip")
    if !Files.exists(zip) then FileStaleness.Initial(sources)
    else
      load(zip) match
        case None           => FileStaleness.Unknown
        case Some(analysis) => FileStaleness.Known(stale(analysis, sources))

  /** `None` is a file that exists and could not be read, or an analysis that is not Zinc's. */
  private def load(zip: Path): Option[sbt.internal.inc.Analysis] =
    FileAnalysisStore.binary(zip.toFile).get().toScala.flatMap: contents =>
      contents.getAnalysis match
        case analysis: sbt.internal.inc.Analysis => Some(analysis)
        case _                                   => None

  private def stale(analysis: sbt.internal.inc.Analysis, sources: List[Path]): List[Path] =
    val converter = MappedFileConverter.empty
    val byId      = analysis.stamps.sources.iterator.map { (ref, stamp) =>
      ref.id -> (ref, stamp)
    }.toMap
    val rows = sources.map: path =>
      val file    = path.toAbsolutePath.normalize
      val virtual = converter.toVirtualFile(file)
      byId.get(virtual.id) match
        case None =>
          Row(file, None, changed = true)
        case Some((ref, stamp)) =>
          val now = Stamper.forContentHash(virtual)
          Row(file, Some(ref), changed = !Stamp.equivStamp.equiv(stamp, now))
    val current = rows.flatMap(_.ref).map(_.id).toSet
    val removed = byId.iterator.collect:
      case (id, (ref, _)) if !current.contains(id) => ref
    val changed = rows.collect:
      case Row(_, Some(ref), true) => ref
    val defined =
      (changed.toSet ++ removed.toSet).flatMap(ref => analysis.relations.classNames(ref))
    val users   = directUsers(analysis.relations, defined)
    val userIds =
      users.flatMap(name => analysis.relations.definesClass(name)).map(_.id).toSet
    val wanted = rows.collect:
      case Row(file, _, true)                                       => file
      case Row(file, Some(ref), false) if userIds.contains(ref.id) => file
    val keep = wanted.toSet
    sources.filter(path => keep.contains(path.toAbsolutePath.normalize))

  /** Direct member-ref users of `defined`, not `defined` itself. That relation is not transitive. */
  private def directUsers(
      relations: sbt.internal.inc.Relations,
      defined: Set[String],
  ): Set[String] =
    defined.flatMap(name => relations.usesInternalClass(name)) -- defined

  private final case class Row(file: Path, ref: Option[VirtualFileRef], changed: Boolean)
end SourceStale
