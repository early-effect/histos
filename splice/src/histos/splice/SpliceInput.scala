package histos.splice

import coursier.cache.FileCache

import java.nio.file.Path

/** One JS file from the Scala.js linker output directory. `origin` is that file on disk, when the caller has it, so
  * esbuild can bundle it in place.
  */
final case class LinkerFile(
    label: String,
    contents: String,
    sourceMap: Option[Path] = None,
    origin: Option[Path] = None,
)

/** Inputs to a splice run. `libs` is specifier → vendored file. `cache` is where a minified build records what it was
  * built from, so an unchanged build is not minified again.
  */
final case class SpliceInput(
    linker: List[LinkerFile],
    libs: Map[String, Path],
    output: Path,
    minify: Minify = Minify.None,
    sourceMaps: Boolean = false,
    cacheDir: Path = FileCache().location.toPath,
    localOnly: Boolean = false,
    cache: Option[Path] = None,
)
