package histos.step

import histos.plugin.PluginTaskId
import histos.{Platform, ProjectName, ScalaVersion}

import zio.Chunk

enum Phase:
  case Generate, Compile, TestCompile, Link, Test, Package, Publish, Run

/** Stable identity of a step in one graph. */
enum StepId:
  case Cell(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      phase: Phase
  )
  case Plugin(task: PluginTaskId)

  /** A key that is not a cell or a plugin task. The scheduler uses the action
    * id.
    */
  case Work(key: String)

opaque type InputName = String

object InputName:
  def apply(raw: String): InputName = raw

  extension (name: InputName) def value: String = name

enum InputValue:
  case Text(value: String)
  case Digest(hex: String)

final case class Input(name: InputName, value: InputValue)

/** A resource class. The executor bounds parallelism per need. */
enum Need:
  case Cpu, Io, Process

/** What identifies the work. The digest is these inputs plus the content hash
  * of each upstream value.
  */
final case class StepInfo(id: StepId, inputs: Chunk[Input], needs: Set[Need])

final case class OutputFile(relative: String)

enum StoredError:
  case Malformed

/** How a value is kept in the action cache, hashed into a downstream key, and
  * which files a hit must find.
  */
@scala.annotation.implicitNotFound(
  "${A} has no Stored codec, so it cannot be cached or feed a hermetic step"
)
trait Stored[A]:
  def encode(value: A): Chunk[Byte]
  def decode(bytes: Chunk[Byte]): Either[StoredError, A]
  def outputs(value: A): Chunk[OutputFile]
