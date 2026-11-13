package histos.assembly

import histos.pack.{AssemblyError, JarError}
import histos.resolve.ResolveError
import histos.zinc.GraphCompileError
import histos.{ProjectName, ScalaVersion}
import java.nio.file.Path

enum AssembleError:
  case Toolchain(error: histos.zinc.CompileError)
  case Compile(error: GraphCompileError)
  case Resolve(error: ResolveError)
  case Jar(error: JarError)
  case Pack(error: AssemblyError)
  case NoModule(name: String)
  case NotAssembly(module: ProjectName)
  case NoClasses(module: ProjectName, version: ScalaVersion)
  case Io(path: Path)

  def message: String = this match
    case Toolchain(error)           => error.message
    case Compile(error)             => error.message
    case Resolve(error)             => error.message
    case Jar(error)                 => error.message
    case Pack(error)                => error.message
    case NoModule(name)             => s"'$name' is not a module"
    case NotAssembly(module)        => s"'${module.value}' does not publish an assembly"
    case NoClasses(module, version) =>
      s"'${module.value}' has no ${version.value} classes"
    case Io(path) => s"could not write $path"
end AssembleError
