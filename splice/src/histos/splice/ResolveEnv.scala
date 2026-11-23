package histos.splice

import java.nio.file.Path

/** How to turn `spliceLibs` coordinates into files. `webjars` is specifier → jar from sbt `update`. */
final case class ResolveEnv(
    resolvers: Seq[SpliceResolver],
    cacheDir: Path,
    localOnly: Boolean,
    webjars: Map[String, Path],
    extractDir: Path,
)
