package histos.pack

import java.nio.file.{Path}

enum JarIo:
  case Read(path: Path)
  case Write(path: Path)
  case Move(path: Path)

  def word: String = this match
    case Read(_)  => "read"
    case Write(_) => "write"
    case Move(_)  => "move"

  def location: Path = this match
    case Read(path)  => path
    case Write(path) => path
    case Move(path)  => path
end JarIo

enum JarError:
  case NotADirectory(path: Path)
  case Empty(path: Path)
  case Escaped(name: String)
  case Duplicate(name: String)
  case DestinationInside(path: Path)
  case Io(op: JarIo)

  def message: String = this match
    case NotADirectory(path)     => s"$path is not a directory of classes"
    case Empty(path)             => s"$path has nothing to jar"
    case Escaped(name)           => s"'$name' leaves the jar"
    case Duplicate(name)         => s"'$name' is already a jar entry"
    case DestinationInside(path) => s"$path is inside the classes directory"
    case Io(op)                  => s"jar failed while ${op.word} ${op.location}"
end JarError
