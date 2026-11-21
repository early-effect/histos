package histos.zinc

import java.nio.file.{Files, Path}

import sbt.internal.inc.{HashUtil, MappedFileConverter}

/** The virtual file a compile stamps. A Scala source inside the checkout is named by its checkout-relative path, which
  * is the id Zinc stores. Java, and a source outside the checkout, keep the absolute path `MappedFileConverter` uses.
  */
object SourceRef:
  def file(checkout: Path, path: Path): xsbti.VirtualFile =
    val root     = checkout.toAbsolutePath.normalize
    val absolute = path.toAbsolutePath.normalize
    if scalaSource(absolute) && absolute.startsWith(root) then
      val relative = root.relativize(absolute).toString.replace('\\', '/')
      Relative(relative, absolute)
    else MappedFileConverter.empty.toVirtualFile(absolute)

  private def scalaSource(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.endsWith(".scala"))

  /** Not path-based, so the bridge reads `rel` as the source path. `contentHash` is the farm hash Zinc stamps. */
  private final class Relative(rel: String, absolute: Path)
      extends xsbti.BasicVirtualFileRef(rel)
      with xsbti.VirtualFile:
    override lazy val contentHash: Long =
      HashUtil.farmHash(absolute)

    override def sizeBytes(): Long =
      Files.size(absolute)

    override lazy val contentHashStr: String =
      HashUtil.sha256HashStr(input())

    def input(): java.io.InputStream =
      Files.newInputStream(absolute)
  end Relative
end SourceRef
