package histos.load

import histos.plugin.*
import histos.jdk.Sha256
import histos.schedule.{CacheError, Digest}
import histos.workspace.IoFailure
import java.nio.file.Path

/** One invocation owns a generation, never the parent of a File output. */
final case class ProductRequest(repo: Path, task: DeclaredTask, invocation: InvocationId)

enum ProductEntry:
  case RootDirectory
  case RootFile(hash: Sha256)
  case Directory(path: ProductEntryPath)
  case File(path: ProductEntryPath, hash: Sha256)

final case class NamedProduct(output: DeclaredOutput, entries: List[ProductEntry])
final case class OutputManifest(text: String, products: List[NamedProduct]):
  def contentDigests: Set[Digest] = products
    .flatMap(_.entries.flatMap:
      case ProductEntry.RootDirectory | ProductEntry.Directory(_) => None
      case ProductEntry.RootFile(hash)                            => Some(Digest.trusted(hash.value))
      case ProductEntry.File(_, hash)                             => Some(Digest.trusted(hash.value)))
    .toSet

enum ProductError:
  case Io(error: IoFailure)
  case Cache(error: CacheError)
  case Report(error: PluginRunError)
  case InvalidManifest
  case InvalidEntry(path: String)
  case UnsafePath(path: Path)
  case Link(path: Path)
  case Missing(path: Path)
  case Kind(path: Path, expected: OutputKind)
  case Unmanaged(path: Path)
  case ActiveReaders
  case Changed

  def message: String = this match
    case Io(error)            => error.message
    case Cache(error)         => error.message
    case Report(error)        => error.message
    case InvalidManifest      => "invalid plugin product manifest"
    case InvalidEntry(path)   => s"invalid plugin product entry '$path'"
    case UnsafePath(path)     => s"unsafe plugin product path $path"
    case Link(path)           => s"symbolic link in plugin product $path"
    case Missing(path)        => s"missing plugin product $path"
    case Kind(path, expected) => s"plugin product $path is not $expected"
    case Unmanaged(path) => s"existing unmanaged output $path must be explicitly cleaned before managed publication"
    case ActiveReaders   => "plugin product generations still have active readers"
    case Changed         => "staged plugin products changed after sealing"
end ProductError
