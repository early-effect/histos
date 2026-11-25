package histos.zinc

import histos.cli.Lines
import histos.{Platform, ProjectName, ScalaVersion}

/** Why a content-store blob was not restored. */
enum CacheLookup:
  case NoBlob
  case Former
  case Stamps
  case Orphans
  case Nir

  def token: String = this match
    case NoBlob  => "no-blob"
    case Former  => "former"
    case Stamps  => "stamps"
    case Orphans => "orphans"
    case Nir     => "nir"
end CacheLookup

/** Whether the compile that followed a miss was written back. */
enum CacheSave:
  case Stored
  case Unsaved(reason: CacheSkip)

  def token: String = this match
    case Stored          => "stored"
    case Unsaved(reason) => s"unsaved ${reason.token}"
end CacheSave

/** Why `cached` refused to store the result. */
enum CacheSkip:
  case Unstable
  case Stamps
  case Unproved

  def token: String = this match
    case Unstable => "unstable"
    case Stamps   => "stamps"
    case Unproved => "unproved"
end CacheSkip

/** What supplied one cell. Printed for every cell, including a hit. */
enum CacheNote:
  case Disk
  case Store
  case Vacant
  case Miss(lookup: CacheLookup, inputs: KeyMiss, saved: CacheSave)

object CacheNote:
  def line(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      note: CacheNote,
      column: Int = 0,
  ): String =
    val id = Lines.begin(module, version, platform, column)
    s"$id ${text(note)}"

  private def text(note: CacheNote): String =
    note match
      case CacheNote.Disk                        => "cache hit disk"
      case CacheNote.Store                       => "cache hit store"
      case CacheNote.Vacant                      => "cache vacant"
      case CacheNote.Miss(lookup, inputs, saved) =>
        s"cache miss ${lookup.token} ${changed(inputs)} ${saved.token}"

  private def changed(inputs: KeyMiss): String =
    inputs match
      case KeyMiss.NoPrior        => "no-prior"
      case KeyMiss.Changed(Nil)   => "inputs"
      case KeyMiss.Changed(names) => s"inputs ${names.map(_.token).mkString(" ")}"
end CacheNote
