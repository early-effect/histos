package histos.testrun

import histos.jdk.Machine
import histos.resolve.{DependencyLock, LockedTools, Resolve}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{Jvm, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** One install compiles the provider once. A new install compiles it again. An empty classpath does not.
  */
object ProviderInstallLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val name   = ProjectName.trusted("providelaw")

  def spec = suite("provider install laws")(
    test("one install keeps the compiled provider, and a new install writes it again"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-provider")
          .flatMap: dir =>
            val program =
              for
                graph <- ZIO
                  .fromEither(Projects(module("providelaw", Jvm)(scala3)))
                  .catchAll(error => ZIO.dieMessage(error.message))
                machine  <- Machine.load(dir).catchAll(error => ZIO.dieMessage(error.message))
                jdk      <- GraphCompile.jdk(machine).catchAll(error => ZIO.dieMessage(error.message))
                tools    <- LockedTools.make(lock, graph)
                libs     <- libraries
                install  <- ProviderInstall.make
                first    <- install.classes(dir, name, scala3, jdk, libs).provideSomeLayer(ZLayer.succeed(tools))
                wrote    <- classFile(first)
                _        <- ZIO.attemptBlocking(Files.delete(providerClass(first))).orDie
                second   <- install.classes(dir, name, scala3, jdk, libs).provideSomeLayer(ZLayer.succeed(tools))
                kept     <- classFile(second)
                fresh    <- ProviderInstall.make
                third    <- fresh.classes(dir, name, scala3, jdk, libs).provideSomeLayer(ZLayer.succeed(tools))
                restored <- classFile(third)
                widened  <-
                  install
                    .classes(dir, name, scala3, jdk, libs :+ libs.head)
                    .provideSomeLayer(ZLayer.succeed(tools))
                failed <-
                  install.classes(dir, name, scala3, jdk, Nil).provideSomeLayer(ZLayer.succeed(tools)).either
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
            program
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def providerClass(classes: Path): Path =
    classes.resolve("subprocess").resolve("Provider.class")

  private def classFile(classes: Path) =
    ZIO.attempt(Files.isRegularFile(providerClass(classes))).orDie

  private def libraries =
    val found = List(classOf[sbt.testing.Framework], classOf[scala.collection.immutable.Nil.type]).flatMap(codeSource)
    if found.isEmpty then ZIO.dieMessage("the provider classpath is not on this process")
    else ZIO.succeed(found)

  private def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
      .flatMap(location => scala.util.Try(Path.of(location.toURI)).toOption)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end ProviderInstallLawSpec
