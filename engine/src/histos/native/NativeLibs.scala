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

  def artifact(toolchain: NativeToolchain): String = this match
    case Plugin =>
      ArtifactName.of(
        Dep.trusted("org.scala-native", "nscplugin", toolchain.revision.value, DepKind.Full),
        JvmCompile.Scala390,
        Platform.Jvm,
      )
    case other =>
      ArtifactName.of(
        Dep.trusted("org.scala-native", other.base, revision(toolchain), DepKind.Binary),
        JvmCompile.Scala390,
        Platform.Native,
      )

  def revision(toolchain: NativeToolchain): String = this match
    case ScalaLib | Scala3Lib => s"${JvmCompile.Scala390.value}+${toolchain.revision.value}"
    case _                     => toolchain.revision.value

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

enum NativeFetch:
  case Unavailable(source: NativeSource)
  case Missing(artifact: String, revision: String)
  case Duplicated(artifact: String)

  def message: String = this match
    case Unavailable(source)       => s"the Scala Native toolchain could not be fetched from ${source.label}"
    case Missing(artifact, revision) => s"the Scala Native toolchain has no $artifact $revision"
    case Duplicated(artifact)      => s"the Scala Native toolchain has more than one $artifact"

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

  def fetch(toolchain: NativeToolchain): IO[NativeFetch, NativeClasspath] =
    ZIO.attemptBlockingInterrupt(load(toolchain)).mapError(_ => NativeFetch.Unavailable(toolchain.source)).flatMap(ZIO.fromEither)

  private def load(toolchain: NativeToolchain): Either[NativeFetch, NativeClasspath] =
    val files = coursier.cache.FileCache()
    val wanted = NativeComponent.Plugin :: Order
    val got = coursier.Fetch(files)(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .withRepositories(Seq(repository(toolchain.source)))
      .addDependencies(wanted.map(component => dependency(component, toolchain))*)
      .eitherResult()
    got match
      case Left(_) => Left(NativeFetch.Unavailable(toolchain.source))
      case Right(result) =>
        val found = result.detailedArtifacts0.toList.flatMap: (dep, publication, _, file) =>
          val jar = file.getName.endsWith(".jar")
          if !jar || classified(publication) then Nil
          else
            val name = dep.module.name.value
            wanted.find(_.artifact(toolchain) == name).toList.map(component => component -> file.toPath)
        assemble(found, toolchain)

  private def classified(publication: Either[coursier.core.VariantPublication, coursier.core.Publication]): Boolean =
    publication match
      case Right(published) => published.classifier.nonEmpty
      case Left(variant)    => variant.classifier.nonEmpty

  private def repository(source: NativeSource): coursier.core.Repository =
    source match
      case NativeSource.Central     => coursier.Repositories.central
      case NativeSource.IvyLocal    => coursier.LocalRepositories.ivy2Local
      case NativeSource.Maven(url)  => coursier.MavenRepository(url.value)

  private def assemble(found: List[(NativeComponent, Path)], toolchain: NativeToolchain): Either[NativeFetch, NativeClasspath] =
    def one(component: NativeComponent): Either[NativeFetch, Path] =
      found.collect { case (`component`, path) => path }.distinct match
        case path :: Nil => Right(path)
        case Nil         => Left(NativeFetch.Missing(component.artifact(toolchain), component.revision(toolchain)))
        case _           => Left(NativeFetch.Duplicated(component.artifact(toolchain)))
    for
      plugin <- one(NativeComponent.Plugin)
      libs   <- Order.foldLeft[Either[NativeFetch, List[Path]]](Right(Nil)): (acc, component) =>
        acc.flatMap(soFar => one(component).map(soFar :+ _))
    yield new NativeClasspath(plugin, libs)

  private def dependency(component: NativeComponent, toolchain: NativeToolchain): coursier.core.Dependency =
    coursier.Dependency(
      coursier.Module(coursier.Organization("org.scala-native"), coursier.ModuleName(component.artifact(toolchain))),
      coursier.version.VersionConstraint(component.revision(toolchain)),
    ).copy(transitive = false)

  def hashes(classpath: NativeClasspath, source: NativeSource): IO[NativeFetch, List[Sha256]] =
    ZIO.foreach(classpath.libraries)(path => hash(path, source))

  private def hash(path: Path, source: NativeSource): IO[NativeFetch, Sha256] =
    ZIO.attemptBlocking(Sha256.of(zio.Chunk.fromArray(java.nio.file.Files.readAllBytes(path)))).mapError(_ => NativeFetch.Unavailable(source))
