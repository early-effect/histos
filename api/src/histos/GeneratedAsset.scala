package histos

import histos.plugin.RepoPath

import java.nio.file.Path

/** A file the build writes into the main class directory. It replaces an unmanaged resource of the same name.
  */
enum GeneratedAsset:
  case File(name: ResourceName, path: RepoPath)
  case Text(name: ResourceName, text: String)

  def resource: ResourceName = this match
    case File(name, _) => name
    case Text(name, _) => name
end GeneratedAsset

/** What fails while copying resources into a class directory. */
enum AssetError:
  case Escaped(raw: String)
  case Duplicate(name: ResourceName)
  case Collides(name: ResourceName)
  case Missing(path: Path)
  case Unreadable(path: Path)

  def message: String = this match
    case Escaped(raw)     => s"'$raw' is not a resource name"
    case Duplicate(name)  => s"resource '${name.value}' is generated twice"
    case Collides(name)   => s"resource '${name.value}' is already a class file"
    case Missing(path)    => s"$path is not a file"
    case Unreadable(path) => s"could not read $path"
end AssetError
