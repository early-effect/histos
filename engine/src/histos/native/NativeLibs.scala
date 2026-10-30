package histos.native

import histos.resolve.{
  ArtifactName,
  LockedTools,
  ResolveError,
  ToolRoot,
  ToolRequest,
  ToolId,
  ToolRole,
  ToolVariant,
  Repositories,
  Transitivity,
  Repository,
  RepoUrl,
}
import histos.zinc.JvmCompile
import histos.{Dep, DepKind, Platform, ScalaVersion, ToolCatalog}

import java.nio.file.Path

import zio.{IO, ZIO}

/** One jar of the Scala Native standard library or the compiler plugin. The names go through [[ArtifactName]].
  */
enum NativeComponent:
  case Nativelib, CLib, PosixLib, WindowsLib, JavaLib, AuxLib, ScalaLib,
    Scala3Lib, Plugin

  def artifact(
      toolchain: NativeToolchain,
      version: ScalaVersion = ToolCatalog.default.native.bridgeScala,
  ): String = this match
    case Plugin =>
      ArtifactName.of(
        Dep.trusted(
          "org.scala-native",
          "nscplugin",
          toolchain.revision.value,
          DepKind.Full,
        ),
        version,
        Platform.Jvm,
      )
    case other =>
      ArtifactName.of(
        Dep.trusted(
          "org.scala-native",
          other.base,
          revision(toolchain, version),
          DepKind.Binary,
        ),
        version,
        Platform.Native,
      )

  def revision(
      toolchain: NativeToolchain,
      version: ScalaVersion = ToolCatalog.default.native.bridgeScala,
  ): String = this match
    case ScalaLib | Scala3Lib => s"${version.value}+${toolchain.revision.value}"
    case _                    => toolchain.revision.value

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
end NativeComponent

enum NativeFetch:
  case Compiler(error: histos.zinc.CompileError)
  case Resolved(error: ResolveError)
  case Missing(artifact: String, revision: String)
  case Duplicated(artifact: String)

  def message: String = this match
    case Compiler(error)             => error.message
    case Resolved(error)             => error.message
    case Missing(artifact, revision) =>
      s"the Scala Native toolchain has no $artifact $revision"
    case Duplicated(artifact) =>
      s"the Scala Native toolchain has more than one $artifact"
end NativeFetch

/** Plugin jar, then the native libraries a program may see. `tools` is not here.
  */
final class NativeClasspath private (
    val plugin: Path,
    val libraries: List[Path],
    val input: histos.resolve.ToolInput,
):
  def entries: List[Path] = libraries

object NativeComponent:
  private val order: List[NativeComponent] = List(
    NativeComponent.Nativelib,
    NativeComponent.CLib,
    NativeComponent.PosixLib,
    NativeComponent.WindowsLib,
    NativeComponent.JavaLib,
    NativeComponent.AuxLib,
    NativeComponent.ScalaLib,
    NativeComponent.Scala3Lib,
  )

  def libraries(version: ScalaVersion): List[NativeComponent] =
    order.filter(component => component != NativeComponent.Scala3Lib || version.value.startsWith("3."))
end NativeComponent

object NativeClasspath:

  def bridgeJars(
      toolchain: NativeToolchain
  ): ZIO[LockedTools, NativeFetch, List[Path]] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      for
        fetched <- tools
          .materialize(
            histos.native.NativeRequests
              .linker(toolchain, tools.catalog, tools.repositories)
          )
          .mapError(NativeFetch.Resolved(_))
        runtime <- JvmCompile
          .runtime(tools.catalog.native.bridgeScala)
          .mapError(error => NativeFetch.Compiler(error))
      yield fetched.artifacts.map(_.path) ++ runtime

  def fetch(
      toolchain: NativeToolchain,
      named: Option[ScalaVersion] = None,
  ): ZIO[LockedTools, NativeFetch, NativeClasspath] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      val version = named.getOrElse(tools.catalog.native.bridgeScala)
      val request =
        histos.native.NativeRequests.libraries(
          version,
          toolchain,
          tools.repositories,
        )
      tools
        .materialize(request)
        .mapError(NativeFetch.Resolved(_))
        .flatMap: classpath =>
          def one(component: NativeComponent): IO[NativeFetch, Path] =
            val name     = component.artifact(toolchain, version)
            val revision = component.revision(toolchain, version)
            classpath.artifacts
              .filter(artifact =>
                artifact.organization == "org.scala-native" && artifact.name == name && artifact.revision == revision && artifact.classifier.isEmpty
              )
              .map(_.path) match
              case path :: Nil => ZIO.succeed(path)
              case Nil         => ZIO.fail(NativeFetch.Missing(name, revision))
              case _           => ZIO.fail(NativeFetch.Duplicated(name))
          for
            plugin    <- one(NativeComponent.Plugin)
            libraries <- ZIO.foreach(
              NativeComponent.libraries(version)
            )(one)
          yield new NativeClasspath(
            plugin,
            libraries,
            histos.resolve.ToolInput(request.id, classpath.artifacts),
          )
end NativeClasspath

object NativeRequests:
  private def nativeRepository(source: NativeSource): Repository = source match
    case NativeSource.Central    => Repository.Central
    case NativeSource.IvyLocal   => Repository.IvyLocal
    case NativeSource.Maven(url) => Repository.Maven(RepoUrl.trusted(url.value))

  private def nativeOrigin(source: NativeSource): Option[Repository] =
    source match
      case NativeSource.Central => None
      case other                => Some(nativeRepository(other))

  def libraries(
      version: ScalaVersion,
      toolchain: NativeToolchain,
      repos: Repositories,
  ): ToolRequest =
    val source = nativeOrigin(toolchain.source)
    val roots  =
      (NativeComponent.Plugin :: NativeComponent.libraries(version)).map(component =>
        ToolRoot(
          "org.scala-native",
          component.artifact(toolchain, version),
          component.revision(toolchain, version),
          Transitivity.Single,
          source,
        )
      )
    ToolRequest(
      ToolId(
        ToolRole.NativeLibraries,
        version,
        ToolVariant.Native(toolchain.revision),
      ),
      roots,
      repos,
    )

  def linker(
      toolchain: NativeToolchain,
      catalog: ToolCatalog,
      repos: Repositories,
  ): ToolRequest =
    val version = catalog.native.bridgeScala
    ToolRequest(
      ToolId(
        ToolRole.NativeLinker,
        version,
        ToolVariant.Native(toolchain.revision),
      ),
      List("tools", "nir", "util").map(name =>
        ToolRoot.of(
          Dep.trusted(
            "org.scala-native",
            name,
            toolchain.revision.value,
            DepKind.Binary,
          ),
          version,
          Platform.Jvm,
          Transitivity.Single,
          nativeOrigin(toolchain.source),
        )
      ),
      repos,
    )

  def suite(
      toolchain: NativeToolchain,
      catalog: ToolCatalog,
      repos: Repositories,
  ): ToolRequest =
    val version = catalog.native.bridgeScala
    ToolRequest(
      ToolId(
        ToolRole.NativeSuite,
        version,
        ToolVariant.Native(toolchain.revision),
      ),
      List(
        ToolRoot.of(
          Dep.trusted(
            "org.scala-native",
            "test-runner",
            toolchain.revision.value,
            DepKind.Binary,
          ),
          version,
          Platform.Jvm,
          Transitivity.Closure,
          nativeOrigin(toolchain.source),
        )
      ),
      repos,
    )

  /** `test-interface` and `test-interface-sbt-defs`, each alone. Their closure pulls another `scala3lib`. */
  def testInterface(
      toolchain: NativeToolchain,
      catalog: ToolCatalog,
      repos: Repositories,
  ): ToolRequest =
    val version                      = catalog.native.bridgeScala
    val origin                       = nativeOrigin(toolchain.source)
    def root(name: String): ToolRoot =
      ToolRoot.of(
        Dep.trusted(
          "org.scala-native",
          name,
          toolchain.revision.value,
          DepKind.Binary,
        ),
        version,
        Platform.Native,
        Transitivity.Single,
        origin,
      )
    ToolRequest(
      ToolId(
        ToolRole.NativeTestInterface,
        version,
        ToolVariant.Native(toolchain.revision),
      ),
      List(root("test-interface"), root("test-interface-sbt-defs")),
      repos,
    )
end NativeRequests
