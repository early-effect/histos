package histos.zinc

import java.nio.file.{Files, Path}

import sbt.internal.inc.{HashUtil, MappedFileConverter}

/** Explicit inputs stamp logical identities and read current physical files. Legacy Scala inputs use checkout-relative
  * names; legacy Java and external inputs retain `MappedFileConverter` paths.
  */
object SourceRef:
  def file(checkout: Path, path: Path): xsbti.VirtualFile =
    val root     = checkout.toAbsolutePath.normalize
    val absolute = path.toAbsolutePath.normalize
    if scalaSource(absolute) && absolute.startsWith(root) then
      val relative = root.relativize(absolute).toString.replace('\\', '/')
      Relative(relative, absolute)
    else MappedFileConverter.empty.toVirtualFile(absolute)

  def file(source: CompileSource): xsbti.VirtualFile =
    val absolute = source.path.toAbsolutePath.normalize
    if source.identity.id.endsWith(".scala") then Relative(source.identity.id, absolute)
    else Physical(source.identity.id, absolute)

  def files(request: CompileRequest): List[xsbti.VirtualFile] =
    if request.sourceInputs.isEmpty then request.sources.map(file(request.checkout, _))
    else request.sourceInputs.map(file)

  private def scalaSource(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.endsWith(".scala"))

  /** Not path-based, so the bridge reads `rel` as the source path. `contentHash` is the farm hash Zinc stamps. */
  private class Relative(rel: String, absolute: Path) extends xsbti.BasicVirtualFileRef(rel) with xsbti.VirtualFile:
    override lazy val contentHash: Long =
      HashUtil.farmHash(absolute)

    override def sizeBytes(): Long =
      Files.size(absolute)

    override lazy val contentHashStr: String =
      HashUtil.sha256HashStr(input())

    def input(): java.io.InputStream =
      Files.newInputStream(absolute)
  end Relative

  private final class Physical(id: String, absolute: Path) extends Relative(id, absolute) with xsbti.PathBasedFile:
    def toPath: Path = absolute
end SourceRef
