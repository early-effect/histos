package histos.workspace

import java.nio.file.Path

/** Why a filesystem call failed. The path stays on `IoFailure`. */
enum IoFault:
  case Read, Write, Stat, Delete, Create, Walk, Temp

  def word: String = this match
    case Read   => "read"
    case Write  => "write"
    case Stat   => "stat"
    case Delete => "delete"
    case Create => "create"
    case Walk   => "walk"
    case Temp   => "create"
end IoFault

final case class IoFailure(path: Path, fault: IoFault):
  def message: String = s"could not ${fault.word} $path"
