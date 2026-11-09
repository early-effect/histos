package histos.format

import histos.ProjectName

import java.nio.file.Path

import zio.NonEmptyChunk
import zio.json.*
import histos.report.WireJson.given JsonCodec[ProjectName]
import histos.report.WireJson.given JsonCodec[Path]

/** `Write` rewrites a file whose formatting changed. `Check` writes nothing and names those files. */
enum FormatMode derives JsonCodec:
  case Write, Check

/** What `histos fmt` formats. `Build` is every module's `src` and `test` trees plus the build file. `Projects` is those
  * modules' trees. `Files` is exactly those repository-relative files.
  */
enum FormatTargets derives JsonCodec:
  case Build
  case Projects(names: NonEmptyChunk[ProjectName])
  case Files(paths: NonEmptyChunk[Path])
