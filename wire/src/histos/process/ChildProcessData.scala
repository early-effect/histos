package histos.process

import histos.Platform
import histos.workspace.{IoFailure}
import java.nio.file.{Path}

/** Text a process wrote. It is text because the process speaks text. */
opaque type ProcessOutput = String

object ProcessOutput:
  def from(raw: String): ProcessOutput = raw

  extension (output: ProcessOutput) def text: String = output
end ProcessOutput

enum GroupOp:
  case Lookup, Call, Set, Pid

enum ChildError:
  case UnsupportedPlatform(platform: Platform)
  case Java(path: Path)
  case Classes(path: Path)
  case Artifact(path: Path)
  case Work(path: Path)
  case Group(op: GroupOp)
  case Bootstrap(output: ProcessOutput)
  case NoProgram
  case Start(program: String)
  case Property(name: String)
  case Status
  case Console
  case Io(failure: IoFailure)

  def message: String = this match
    case UnsupportedPlatform(platform) =>
      s"a jvm child is required, not ${platform.token}"
    case Java(path)        => s"no java executable at $path"
    case Classes(path)     => s"no class directory at $path"
    case Artifact(path)    => s"no classpath artifact at $path"
    case Work(path)        => s"work directory is not an empty directory: $path"
    case Group(op)         => s"process group failed while ${op.toString.toLowerCase}"
    case Bootstrap(output) => s"child bootstrap failed: ${output.text}"
    case NoProgram         => "a child needs a program"
    case Start(program)    => s"could not start $program"
    case Property(name)    => s"could not read system property $name"
    case Status            => "the child did not report a status"
    case Console           => "could not read the child's output"
    case Io(failure)       => failure.message
end ChildError
