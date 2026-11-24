package histos.doc

import histos.build.StageProduct
import histos.js.ScalaJs
import histos.resolve.LockedTools
import histos.zinc.{CompileError, JvmCompile}
import histos.{Module, Platform}

import java.nio.file.Path

import zio.ZIO

/** Scaladoc reads the Main product's compile classpath and its platform library tool cell, never Test dependencies. JVM
  * library copies stay off this list. Scala.js keeps its backend standard library, including UnitOps metadata.
  */
object DocClasspath:
  def of(module: Module, product: StageProduct): ZIO[LockedTools, CompileError, List[Path]] =
    val cell      = product.compiled
    val libraries = cell.platform match
      case Platform.Jvm | Platform.Native => ZIO.succeed(Nil)
      case Platform.Js                    => ScalaJs.libraries(cell.version, module.jsVersion)
    libraries.map(paths => (product.classpath ++ paths).distinct.filterNot(scalaLibrary))

  private def scalaLibrary(path: Path): Boolean =
    JvmCompile.isStandardLibrary(path) ||
      Option(path.getFileName)
        .map(_.toString)
        .exists: name =>
          name.startsWith("scala3lib_") || name.startsWith("scalalib_")
end DocClasspath
