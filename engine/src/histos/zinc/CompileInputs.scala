package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.schedule.Digest
import histos.{Platform, ProjectName, ScalaVersion}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.annotation.tailrec
import scala.util.control.NonFatal

import zio.{Chunk, IO}

/** One named compile input. The token is what a miss prints. */
enum CompileInput:
  case Scala, Platform, Jdk, Runtime, Flags, Sources, Artifacts, Apis, Tools, Compiler

  def token: String = this match
    case Scala     => "scala"
    case Platform  => "platform"
    case Jdk       => "jdk"
    case Runtime   => "runtime"
    case Flags     => "flags"
    case Sources   => "sources"
    case Artifacts => "artifacts"
    case Apis      => "apis"
    case Tools     => "tools"
    case Compiler  => "compiler"
end CompileInput

object CompileInput:
  def from(raw: String): Option[CompileInput] =
    raw match
      case "scala"     => Some(CompileInput.Scala)
      case "platform"  => Some(CompileInput.Platform)
      case "jdk"       => Some(CompileInput.Jdk)
      case "runtime"   => Some(CompileInput.Runtime)
      case "flags"     => Some(CompileInput.Flags)
      case "sources"   => Some(CompileInput.Sources)
      case "artifacts" => Some(CompileInput.Artifacts)
      case "apis"      => Some(CompileInput.Apis)
      case "tools"     => Some(CompileInput.Tools)
      case "compiler"  => Some(CompileInput.Compiler)
      case _           => None

/** Why this compile key was not stored. */
enum KeyMiss:
  case NoPrior
  case Changed(names: List[CompileInput])

object KeyMiss:
  def line(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      change: KeyMiss,
  ): String =
    val id = s"${module.value} ${version.value} ${platform.token}"
    change match
      case KeyMiss.NoPrior        => s"$id no prior key"
      case KeyMiss.Changed(Nil)   => id
      case KeyMiss.Changed(names) => s"$id ${names.map(_.token).mkString(" ")}"

final case class CompileRow(input: CompileInput, value: String)

/** The named inputs of a compile key, stored under the analysis path. The path survives `clean`. The key does not: it
  * changes on a miss, so it cannot address the previous map.
  */
object CompileInputs:
  def declared(request: CompileRequest, sources: Sha256): List[CompileRow] =
    val tool = request.toolchain
    List(
      CompileRow(CompileInput.Scala, tool.version.value),
      CompileRow(CompileInput.Platform, tool.platform.token),
      CompileRow(CompileInput.Jdk, tool.jdk.version.value),
      CompileRow(CompileInput.Runtime, histos.toolchain.Runtimes.identityToken(tool.jdk.identity)),
      CompileRow(CompileInput.Flags, tool.flags.canonical),
      CompileRow(CompileInput.Sources, sources.value),
      CompileRow(CompileInput.Artifacts, join(request.artifactHashes)),
      CompileRow(CompileInput.Apis, join(request.upstreamApis)),
      CompileRow(CompileInput.Tools, join(request.compilationTools.map(_.fingerprint))),
    )

  /** Compiler jars are not in [[declared]]. [[CompileKey.withTools]] adds the same fingerprints to the key. */
  def withCompiler(rows: List[CompileRow], hashes: List[Sha256]): List[CompileRow] =
    rows.filter(_.input != CompileInput.Compiler) :+ CompileRow(CompileInput.Compiler, join(hashes))

  def diff(previous: List[CompileRow], current: List[CompileRow]): List[CompileInput] =
    val prior   = previous.map(row => row.input -> row.value).toMap
    val now     = current.map(row => row.input -> row.value).toMap
    val changed = current.collect:
      case CompileRow(input, value) if prior.get(input).exists(_ != value) => input
    val added   = current.map(_.input).filterNot(prior.contains)
    val removed = previous.map(_.input).filterNot(now.contains)
    changed ++ added ++ removed

  def explain(
      store: ContentStore,
      analysis: Path,
      rows: List[CompileRow],
  ): IO[CompileError, KeyMiss] =
    store
      .read(slot(analysis))
      .mapError(CompileError.Stored(_))
      .map:
        case None        => KeyMiss.NoPrior
        case Some(bytes) =>
          decode(bytes) match
            case None           => KeyMiss.NoPrior
            case Some(previous) => KeyMiss.Changed(diff(previous, rows))

  def remember(
      store: ContentStore,
      analysis: Path,
      rows: List[CompileRow],
  ): IO[CompileError, Unit] =
    for
      // File mtime. TestClock.instant arms a warning unless the test moves the clock.
      now <- zio.Clock.ClockLive.instant
      _   <- store.put(slot(analysis), encode(rows), now).mapError(CompileError.Stored(_))
    yield ()

  /** Analysis directory, not the compile key. */
  def slot(analysis: Path): Digest =
    val text = "compile-inputs\n" + analysis.toAbsolutePath.normalize.toString
    Digest.ofBytes(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  private[histos] def encode(rows: List[CompileRow]): Chunk[Byte] =
    val out  = new ByteArrayOutputStream
    val data = new DataOutputStream(out)
    data.writeInt(rows.size)
    rows.foreach: row =>
      val name = row.input.token.getBytes(StandardCharsets.UTF_8)
      val body = row.value.getBytes(StandardCharsets.UTF_8)
      data.writeInt(name.length)
      data.write(name)
      data.writeInt(body.length)
      data.write(body)
    data.flush()
    Chunk.fromArray(out.toByteArray)

  /** A short or unknown map is absent. The next successful compile replaces it. */
  private[histos] def decode(bytes: Chunk[Byte]): Option[List[CompileRow]] =
    val in = new DataInputStream(new ByteArrayInputStream(bytes.toArray))
    try
      val count = in.readInt()
      if count < 0 then None
      else readRows(in, count, Nil).filter(unique)
    catch case NonFatal(_) => None

  private def join(hashes: List[Sha256]): String =
    hashes.map(_.value).mkString("\n")

  private def unique(rows: List[CompileRow]): Boolean =
    rows.map(_.input).distinct.size == rows.size

  @tailrec
  private def readRows(
      in: DataInputStream,
      left: Int,
      acc: List[CompileRow],
  ): Option[List[CompileRow]] =
    if left == 0 then if in.available() == 0 then Some(acc.reverse) else None
    else
      readRow(in) match
        case None      => None
        case Some(row) => readRows(in, left - 1, row :: acc)

  private def readRow(in: DataInputStream): Option[CompileRow] =
    val nameLength = in.readInt()
    if nameLength < 0 || nameLength > in.available() then None
    else
      val raw = new Array[Byte](nameLength)
      in.readFully(raw)
      val valueLength = in.readInt()
      if valueLength < 0 || valueLength > in.available() then None
      else
        val body = new Array[Byte](valueLength)
        in.readFully(body)
        CompileInput
          .from(new String(raw, StandardCharsets.UTF_8))
          .map(input => CompileRow(input, new String(body, StandardCharsets.UTF_8)))
end CompileInputs
