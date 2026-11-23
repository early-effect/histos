package histos.splice

/** Failures from the splice program. Values until the sbt plugin boundary. */
enum SpliceError derives CanEqual:
  case Unresolved(specifier: String, referringFile: String)
  case MissingFile(specifier: String, path: String)
  case LeftoverSpecifier(specifier: String, outputFile: String)
  case DuplicateLib(specifier: String)
  case MissingSha256(specifier: String)
  case ChecksumMismatch(specifier: String, expected: String, actual: String)
  case NoResolver(specifier: String, kind: String)
  case NotFound(specifier: String, detail: String)
  case MissingJarPath(specifier: String, jar: String, path: String)
  case MissingArchivePath(specifier: String, archive: String, path: String)

  /** esbuild could not bundle; `file` is the one it pointed at, when it named one. */
  case Bundle(file: Option[String], detail: String)

  /** The bundle path saw more than one linker file. Empty `spliceLibs` still concatenates. */
  case SeveralModules(files: List[String])
  case Closure(detail: String)
  case Minify(detail: String)
  case Io(detail: String)

  def message: String = this match
    case Unresolved(specifier, referringFile) =>
      s"""sbt-splice: unresolved specifier "$specifier" in $referringFile"""
    case MissingFile(specifier, path) =>
      s"""sbt-splice: mapped file for "$specifier" is missing: $path"""
    case LeftoverSpecifier(specifier, outputFile) =>
      s"""sbt-splice: leftover specifier "$specifier" in $outputFile"""
    case DuplicateLib(specifier) =>
      s"""sbt-splice: duplicate spliceLibs entry for "$specifier""""
    case MissingSha256(specifier) =>
      s"""sbt-splice: coordinate "$specifier" is missing sha256"""
    case ChecksumMismatch(specifier, expected, actual) =>
      s"""sbt-splice: sha256 mismatch for "$specifier": expected $expected, got $actual"""
    case NoResolver(specifier, kind) =>
      s"""sbt-splice: no $kind resolver for "$specifier""""
    case NotFound(specifier, detail) =>
      s"""sbt-splice: could not fetch "$specifier": $detail"""
    case MissingJarPath(specifier, jar, path) =>
      s"""sbt-splice: jar for "$specifier" ($jar) has no $path"""
    case MissingArchivePath(specifier, archive, path) =>
      s"""sbt-splice: archive for "$specifier" ($archive) has no $path"""
    case Bundle(Some(file), detail) =>
      s"sbt-splice: esbuild could not bundle $file: $detail"
    case Bundle(None, detail) =>
      s"sbt-splice: esbuild could not bundle the mapped libraries:\n$detail"
    case SeveralModules(files) =>
      s"sbt-splice: expected one linker file to bundle, found ${files.mkString(", ")}"
    case Closure(detail) =>
      s"sbt-splice: Closure compiler failed:\n$detail"
    case Minify(detail) =>
      s"sbt-splice: minify failed:\n$detail"
    case Io(detail) =>
      s"sbt-splice: $detail"
end SpliceError
