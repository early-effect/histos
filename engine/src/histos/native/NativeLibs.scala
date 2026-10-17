package histos.native

import histos.jdk.Sha256
import histos.resolve.ArtifactName
import histos.zinc.JvmCompile
import histos.{Dep, DepKind, Platform}

import java.nio.file.Path

import zio.{IO, ZIO}

/** One jar of the Scala Native standard library or the compiler plugin. The names go through [[ArtifactName]]. */
enum NativeComponent:
  case Nativelib, CLib, PosixLib, WindowsLib, JavaLib, AuxLib, ScalaLib, Scala3Lib, Plugin

  def artifact: String = this match
    case Plugin =>
      ArtifactName.of(Dep.trusted("org.scala-native", "nscplugin", NativePins.revision, DepKind.Full), JvmCompile.Scala390, Platform.Jvm)
    case other =>
      ArtifactName.of(Dep.trusted("org.scala-native", other.base, other.revision, DepKind.Binary), JvmCompile.Scala390, Platform.Native)

  def revision: String = this match
    case ScalaLib | Scala3Lib => NativePins.scala3Lib
    case _                     => NativePins.revision

  private def base: String = this match
    case Nativelib  => "nativelib"
    case CLib       => "clib"
    case PosixLib   => "posixlib"
    case WindowsLib => "windowslib"
    case JavaLib    => "javalib"
    case AuxLib     => "auxlib"
    case ScalaLib   => "scalalib"
    case Scala3Lib  => "scala3lib"
    case Plugin     => "nscplugin"

object NativePins:
  /** The local Scala Native publish. Not a Maven Central release. */
  val revision: String = "0.5.13-SNAPSHOT"

  /** `scala3lib` and `scalalib` are published against the Scala version, not the toolchain revision alone. */
  val scala3Lib: String = s"${JvmCompile.Scala390.value}+$revision"

enum NativeFetch:
  case Unavailable
  case Missing(component: NativeComponent)
  case Duplicated(component: NativeComponent)

  def message: String = this match
    case Unavailable         => "the Scala Native toolchain could not be fetched from ivy2 local"
    case Missing(component)  => s"ivy2 local has no ${component.artifact} ${component.revision}"
    case Duplicated(component) => s"ivy2 local has more than one ${component.artifact}"

/** Plugin jar, then the native libraries a program may see. `tools` is not here. */
final class NativeClasspath private (
    val plugin: Path,
    val libraries: List[Path],
):
  def entries: List[Path] = libraries

object NativeClasspath:
  private val Order: List[NativeComponent] = List(
    NativeComponent.Nativelib,
    NativeComponent.CLib,
    NativeComponent.PosixLib,
    NativeComponent.WindowsLib,
    NativeComponent.JavaLib,
    NativeComponent.AuxLib,
    NativeComponent.ScalaLib,
    NativeComponent.Scala3Lib,
  )

  def fetch: IO[NativeFetch, NativeClasspath] =
    ZIO.attemptBlockingInterrupt(load()).mapError(_ => NativeFetch.Unavailable).flatMap(ZIO.fromEither)

  private def load(): Either[NativeFetch, NativeClasspath] =
    val files = coursier.cache.FileCache()
    val wanted = NativeComponent.Plugin :: Order
    val got = coursier.Fetch(files)(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .withRepositories(Seq(coursier.LocalRepositories.ivy2Local))
      .addDependencies(wanted.map(dependency)*)
      .eitherResult()
    got match
      case Left(_) => Left(NativeFetch.Unavailable)
      case Right(result) =>
        val found = result.detailedArtifacts0.toList.flatMap: (dep, publication, _, file) =>
          val jar = file.getName.endsWith(".jar")
          if !jar || classified(publication) then Nil
          else
            val name = dep.module.name.value
            wanted.find(_.artifact == name).toList.map(component => component -> file.toPath)
        assemble(found)

  private def classified(publication: Either[coursier.core.VariantPublication, coursier.core.Publication]): Boolean =
    publication match
      case Right(published) => published.classifier.nonEmpty
      case Left(variant)    => variant.classifier.nonEmpty

  private def assemble(found: List[(NativeComponent, Path)]): Either[NativeFetch, NativeClasspath] =
    def one(component: NativeComponent): Either[NativeFetch, Path] =
      found.collect { case (`component`, path) => path }.distinct match
        case path :: Nil => Right(path)
        case Nil         => Left(NativeFetch.Missing(component))
        case _           => Left(NativeFetch.Duplicated(component))
    for
      plugin <- one(NativeComponent.Plugin)
      libs   <- Order.foldLeft[Either[NativeFetch, List[Path]]](Right(Nil)): (acc, component) =>
        acc.flatMap(soFar => one(component).map(soFar :+ _))
    yield new NativeClasspath(plugin, libs)

  private def dependency(component: NativeComponent): coursier.core.Dependency =
    coursier.Dependency(
      coursier.Module(coursier.Organization("org.scala-native"), coursier.ModuleName(component.artifact)),
      coursier.version.VersionConstraint(component.revision),
    ).copy(transitive = false)

  def hashes(classpath: NativeClasspath): IO[NativeFetch, List[Sha256]] =
    ZIO.foreach(classpath.libraries)(hash).mapError(_ => NativeFetch.Unavailable)

  private def hash(path: Path): IO[NativeFetch, Sha256] =
    ZIO.attemptBlocking(Sha256.of(zio.Chunk.fromArray(java.nio.file.Files.readAllBytes(path)))).mapError(_ => NativeFetch.Unavailable)
