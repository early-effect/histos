package histos.scaffold

/** `Full` writes `.gitignore`, the wrapper, CI, and a lock. `Bare` writes the build file and sources. */
enum ScaffoldKind:
  case Full
  case Bare
end ScaffoldKind

/** `histos init --from`. `sbt` reads `build.sbt`. `zipx` reads `project/ZipxVersions.scala`. */
enum ImportSource:
  case Sbt
  case Zipx
end ImportSource

object ImportSource:
  def from(raw: String): Option[ImportSource] =
    raw match
      case "sbt"  => Some(ImportSource.Sbt)
      case "zipx" => Some(ImportSource.Zipx)
      case _      => None
end ImportSource
