package histos.native

import histos.resolve.{RepoUrl, Repositories, Repository, ToolRole, ToolVariant, Transitivity}
import histos.{NativeVersion, ScalaVersion, ToolCatalog}

import java.nio.file.Path

import zio.test.*

/** Native library coordinates are the published names, and a Central snapshot adds the snapshot repository once.
  */
object NativeRequestLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion.trusted("3.10.0")
  private val scala2  = ScalaVersion.trusted("2.13.18")
  private val release = NativeVersion("0.5.9")
  private val snap    = NativeVersion("0.5.9-SNAPSHOT")
  private val central = NativeToolchain(NativeSource.Central, release)
  private val ivy     = NativeToolchain(NativeSource.IvyLocal, release)
  private val repos   = Repositories.central
  private val bridge  = ToolCatalog.default.native.bridgeScala

  def spec = suite("native requests")(
    test("a library name carries the native suffix, Scala 3 keeps scala3lib, and the scala libs use the bridge"):
      val names3 = NativeComponent.libraries(scala3).map(_.artifact(central, scala3))
      val names2 = NativeComponent.libraries(scala2).map(_.artifact(central, scala2))
      assertTrue(
        names3 == List(
          "nativelib_native0.5_3",
          "clib_native0.5_3",
          "posixlib_native0.5_3",
          "windowslib_native0.5_3",
          "javalib_native0.5_3",
          "auxlib_native0.5_3",
          "scalalib_native0.5_3",
          "scala3lib_native0.5_3",
        ),
        names2 == List(
          "nativelib_native0.5_2.13",
          "clib_native0.5_2.13",
          "posixlib_native0.5_2.13",
          "windowslib_native0.5_2.13",
          "javalib_native0.5_2.13",
          "auxlib_native0.5_2.13",
          "scalalib_native0.5_2.13",
        ),
        NativeComponent.ScalaLib.revision(central, scala3) == "3.10.0+0.5.9",
        NativeComponent.Nativelib.revision(central, scala3) == "0.5.9",
        NativeComponent.Plugin.artifact(central, bridge) == s"nscplugin_${bridge.value}",
        NativeComponent.Plugin.revision(central, bridge) == "0.5.9",
      )
    ,
    test("central has no origin, ivy and a file maven do, and a snapshot repository is added once"):
      fileOrigin match
        case None        => assertTrue(false)
        case Some(maven) =>
          val asked    = NativeRequests.libraries(scala3, central, repos)
          val fromIvy  = NativeRequests.libraries(scala3, ivy, repos)
          val fromFile = NativeRequests.libraries(scala3, maven, repos)
          val snapshot = NativeRequests.libraries(scala3, NativeToolchain(NativeSource.Central, snap), repos)
          val again    =
            NativeRequests.libraries(scala3, NativeToolchain(NativeSource.Central, snap), snapshot.repositories)
          val url = "https://central.sonatype.com/repository/maven-snapshots/"
          assertTrue(
            asked.roots.forall(_.origin.isEmpty),
            asked.repositories == repos,
            asked.id.role == ToolRole.NativeLibraries,
            asked.id.variant == ToolVariant.Native(release),
            fromIvy.roots.forall(_.origin == Some(Repository.IvyLocal)),
            fromFile.roots.forall(_.origin == Some(Repository.Maven(RepoUrl.trusted("file:/tmp/m2")))),
            snapshot.repositories.all.head == Repository.Maven(RepoUrl.trusted(url)),
            snapshot.repositories.all.contains(Repository.Central),
            again.repositories.all.count(_ == Repository.Maven(RepoUrl.trusted(url))) == 1,
          )
    ,
    test("the linker, the test interface, and the suite are those roots"):
      val linker = NativeRequests.linker(central, ToolCatalog.default, repos)
      val tests  = NativeRequests.testInterface(scala3, central, repos)
      val suite  = NativeRequests.suite(central, ToolCatalog.default, repos)
      assertTrue(
        linker.id.role == ToolRole.NativeLinker,
        linker.roots
          .map(_.name) == List(s"tools_${binary(bridge)}", s"nir_${binary(bridge)}", s"util_${binary(bridge)}"),
        linker.roots.forall(_.transitivity == Transitivity.Single),
        tests.id.role == ToolRole.NativeTestInterface,
        tests.roots.map(_.name) == List("test-interface_native0.5_3"),
        tests.roots.forall(_.transitivity == Transitivity.Single),
        suite.id.role == ToolRole.NativeSuite,
        suite.roots.map(_.name) == List(s"test-runner_${binary(bridge)}", "scala-library"),
        suite.roots.map(_.revision) == List(release.value, "2.13.18"),
        suite.roots.map(_.transitivity) == List(Transitivity.Closure, Transitivity.Single),
        suite.roots.drop(1).forall(_.origin.isEmpty),
      )
    ,
    test("the bridge source names the module, the main, and the compilers, and escapes a quote"):
      val clang   = Path.of("/opt/clang")
      val clangpp = Path.of("/opt/clang++")
      val text    = NativeBridge.source("a\"b\\c\nd", "Main", clang, clangpp, List(Path.of("/lib/app.jar")))
      assertTrue(
        NativeBridge.MainName == "subprocess.NativeBridge",
        text.contains("""withModuleName("a\"b\\c\nd")"""),
        text.contains("""withMainClass(Some("Main"))"""),
        text.contains("""Path.of("/opt/clang")"""),
        text.contains("""Path.of("/opt/clang++")"""),
        text.contains("""Path.of("/lib/app.jar")"""),
        text.contains("object NativeBridge"),
      ),
  )

  private def binary(version: ScalaVersion): String =
    version.value.split("\\.", 3) match
      case Array("3", _, _)       => "3"
      case Array(major, minor, _) => s"$major.$minor"
      case _                      => version.value

  private def fileOrigin: Option[NativeToolchain] =
    NativeToolchain.parse("file:/tmp/m2") match
      case Right(source) => Some(NativeToolchain(source, release))
      case Left(_)       => None
end NativeRequestLawSpec
