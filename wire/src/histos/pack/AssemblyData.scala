package histos.pack

import java.nio.file.Path

enum AssemblyError:
  case NotADirectory(path: Path)
  case Empty(path: Path)
  case Escaped(name: String)
  case DestinationInside(path: Path)
  case Overlap(path: Path)
  case NotAJar(path: Path)
  case MissingMain(main: MainClass)
  case NoClasses
  case Duplicate(name: String)
  case NotAFile(path: Path)
  case LazyVal(error: LazyValError)
  case Io(op: JarIo)

  def message: String = this match
    case NotADirectory(path)     => s"$path is not a directory of classes"
    case Empty(path)             => s"$path has no class files"
    case Escaped(name)           => s"'$name' leaves the assembly"
    case DestinationInside(path) => s"$path is inside the classes directory"
    case Overlap(path)           => s"$path is the assembly destination"
    case NotAJar(path)           => s"$path is not a jar"
    case MissingMain(main)       => s"${main.binary} is not in the assembly"
    case NoClasses               => "an assembly needs a classes directory"
    case Duplicate(name)         => s"'$name' is already in the assembly"
    case NotAFile(path)          => s"$path is not a file"
    case LazyVal(error)          => error.message
    case Io(op)                  => s"assembly failed while ${op.toString.toLowerCase}"
end AssemblyError
