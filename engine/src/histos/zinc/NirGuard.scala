package histos.zinc

import histos.Platform

import java.nio.file.{Files, Path}

import xsbti.VirtualFileRef

/** A Native source that emitted class files must emit NIR beside those classes. JVM and Scala.js are not this check.
  */
private[zinc] object NirGuard:
  def missing(request: CompileRequest, analysis: sbt.internal.inc.Analysis): Option[Path] =
    if request.toolchain.platform != Platform.Native then None
    else
      request.sources
        .filter(scalaSource)
        .sortBy(path => path.toAbsolutePath.normalize.toString)
        .collectFirst:
          case source if bare(analysis, source) => source.toAbsolutePath.normalize

  private def scalaSource(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.endsWith(".scala"))

  private def bare(analysis: sbt.internal.inc.Analysis, source: Path): Boolean =
    val want = source.toAbsolutePath.normalize.toString
    analysis.relations.allSources.iterator.find(ref => same(ref, want)) match
      case None      => false
      case Some(ref) =>
        val classes = analysis.relations.products(ref).iterator.filter(_.id.endsWith(".class")).toList
        classes.nonEmpty && !classes.exists(product => Files.isRegularFile(nirBeside(product)))

  private def same(ref: VirtualFileRef, want: String): Boolean =
    ref.id == want || normalized(ref.id).contains(want)

  private def normalized(id: String): Option[String] =
    try Some(Path.of(id).toAbsolutePath.normalize.toString)
    catch case _: java.nio.file.InvalidPathException => None

  private def nirBeside(product: VirtualFileRef): Path =
    val path = Path.of(product.id)
    val file = Option(path.getFileName).map(_.toString).getOrElse("")
    val name = if file.endsWith(".class") then file.stripSuffix(".class") + ".nir" else file + ".nir"
    Option(path.getParent).fold(Path.of(name))(_.resolve(name))
end NirGuard
