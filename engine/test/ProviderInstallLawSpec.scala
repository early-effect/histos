package histos.testrun

import histos.jdk.Machine
import histos.resolve.LockedTools
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{CompilerClosure, Jvm, ProjectName, Projects, module}

import java.nio.file.{Files, Path}

import zio.{NonEmptyChunk, Scope, UIO, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** One install compiles the provider once. A new install compiles it again. An empty classpath does not.
  */
object ProviderInstallLawSpec extends ZIOSpecDefault:
  private val name = ProjectName.trusted("providelaw")

  def spec = suite("provider install laws")(
    test("one install keeps the compiled provider, and a new install writes it again"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-provider")
          .flatMap: dir =>
            for
              graph    <- ZIO.fromEither(Projects(module("providelaw", Jvm)(scala3)))
              machine  <- Machine.load(dir)
              jdk      <- GraphCompile.jdk(machine)
              tools    <- LockedTools.make(closure.lock, graph)
              libs     <- libraries
              install  <- ProviderInstall.make
              first    <- install.classes(dir, name, scala3, jdk, libs.toList).provideSomeLayer(ZLayer.succeed(tools))
              wrote    <- classFile(first)
              _        <- ZIO.attemptBlocking(Files.delete(providerClass(first))).orDie
              second   <- install.classes(dir, name, scala3, jdk, libs.toList).provideSomeLayer(ZLayer.succeed(tools))
              kept     <- classFile(second)
              fresh    <- ProviderInstall.make
              third    <- fresh.classes(dir, name, scala3, jdk, libs.toList).provideSomeLayer(ZLayer.succeed(tools))
              restored <- classFile(third)
              widened  <- install
                .classes(dir, name, scala3, jdk, (libs :+ libs.head).toList)
                .provideSomeLayer(ZLayer.succeed(tools))
              failed <- install.classes(dir, name, scala3, jdk, Nil).provideSomeLayer(ZLayer.succeed(tools)).either
            yield assertTrue(
              wrote,
              second == first,
              !kept,
              third == first,
              restored,
              widened != first,
              failed match
                case Left(ModuleTestError.Sources(module, version, _)) => module == name && version == scala3
                case _                                                 => false,
            )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def providerClass(classes: Path): Path =
    classes.resolve("subprocess").resolve("Provider.class")

  private def classFile(classes: Path): UIO[Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(providerClass(classes))).orDie

  /** The jars of the test interface and the Scala library this process runs on. */
  private def libraries: UIO[NonEmptyChunk[Path]] =
    ZIO
      .fromOption(
        NonEmptyChunk.fromIterableOption(
          List(classOf[sbt.testing.Framework], classOf[scala.collection.immutable.Nil.type]).flatMap(codeSource)
        )
      )
      .orElse(ZIO.dieMessage("the provider classpath is not on this process"))

  private def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
      .flatMap(location => scala.util.Try(Path.of(location.toURI)).toOption)
end ProviderInstallLawSpec
