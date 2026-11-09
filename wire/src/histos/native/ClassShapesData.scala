package histos.native

import java.nio.file.Path

enum ClassShapeError:
  case Failed(kind: String)
  case Read(path: Path)
  case ClassFile(label: String)

  def message: String = this match
    case Failed(kind)     => s"could not read class files ($kind)"
    case Read(path)       => s"could not read class files under $path"
    case ClassFile(label) => s"could not read the class file $label"
end ClassShapeError
