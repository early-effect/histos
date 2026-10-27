package histos.zinc

import java.nio.file.{Files, Path}

import sbt.internal.inc.{Analysis, FileAnalysisStore, MappedFileConverter, Relations, Stamper, Stamps}
import xsbti.{T2, VirtualFileRef}
import xsbti.api.{DependencyContext, ExternalDependency, InternalDependency}
import xsbti.compile.{CompileOrder, FileHash, MiniOptions, MiniSetup}
import xsbti.compile.analysis.Stamp as XStamp

/** A synthetic `inc.zip` for tests. Preview does not call this.
  */
object StoredAnalysis:
  def put(
      zip: Path,
      classes: Path,
      sources: List[(Path, String)],
      depends: List[(String, String)],
  ): Unit =
    val converter = MappedFileConverter.empty
    val stamped = sources.map: (path, name) =>
      val virtual = converter.toVirtualFile(path.toAbsolutePath.normalize)
      Stamped(virtual, name, Stamper.forContentHash(virtual))
    val stamps = stamped.foldLeft(Stamps.empty): (acc, row) =>
      acc.markSource(row.virtual, row.stamp)
    val relations = stamped.foldLeft(Relations.empty): (acc, row) =>
      val deps = depends.collect:
        case (source, target) if source == row.name =>
          InternalDependency.of(source, target, DependencyContext.DependencyByMemberRef)
      acc.addSource(
        row.virtual,
        List.empty[VirtualFileRef],
        List(row.name -> row.name),
        deps,
        List.empty[ExternalDependency],
        List.empty[(VirtualFileRef, String, XStamp)],
      )
    val analysis = Analysis.empty.copy(stamps = stamps, relations = relations)
    val setup = MiniSetup.of(
      sbt.internal.inc.CompileOutput(classes),
      MiniOptions.of(Array.empty[FileHash], Array.empty[String], Array.empty[String]),
      "3.10.0",
      CompileOrder.Mixed,
      false,
      Array.empty[T2[String, String]],
    )
    Option(zip.getParent).foreach(dir => Files.createDirectories(dir))
    FileAnalysisStore.binary(zip.toFile).set(xsbti.compile.AnalysisContents.create(analysis, setup))

  private final case class Stamped(virtual: VirtualFileRef, name: String, stamp: XStamp)
end StoredAnalysis
