package histos.doctor

import histos.Platform
import histos.cache.ContentStore
import histos.load.{PluginCompileError, PluginCompileJob, PluginCompileSupport, PluginCompiler}
import histos.suite.HistosSpec
import histos.toolchain.HostCompilerSession
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, ZIO, durationInt}
import zio.test.*

object Scala3CompatibilitySpec extends HistosSpec:
  private val proof = suite("Scala 3 baseline preparation")(
    test("released TASTy supports both compatible and removed-member analysis") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("compiler"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session  <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            old      <- compile(root.resolve("old"), session, "def kept: Int = 1")
            now      <- compile(root.resolve("now"), session, "def kept: Int = 2")
            broken   <- compile(root.resolve("broken"), session, "def replacement: Int = 2")
            jar      <- baselineJar(root.resolve("baseline.jar"), old)
            sidecars <- ZIO
              .attemptBlocking(
                Files.isRegularFile(old.resolve("fixture/Api.tasty")) &&
                  Files.isRegularFile(now.resolve("fixture/Api.tasty"))
              )
              .mapError(_ => PluginCompileError.Io(IoFailure(old, IoFault.Stat)))
            results <- ZIO
              .serviceWithZIO[CompatibilityAnalyzer] { analyzer =>
                for
                  compatible   <- analyzer.analyze(CompatibilityInput(Platform.Jvm, jar, now))
                  incompatible <- analyzer.analyze(CompatibilityInput(Platform.Jvm, jar, broken))
                yield (compatible, incompatible)
              }
              .provideLayer(CompatibilityAnalyzer.jvm)
              .either
          yield (sidecars, results)
        }
        .map { ran =>
          val (sidecars, results) = ran.value
          val removed             = results.exists { (_, result) =>
            result match
              case CompatibilityResult.Incompatible(problems) =>
                problems.exists {
                  case CompatibilityProblem.Member(description) => description.contains("kept")
                  case CompatibilityProblem.MissingClass(_) | CompatibilityProblem.Template(_) => false
                }
              case CompatibilityResult.Compatible => false
          }
          assertTrue(sidecars, results.exists(_._1 == CompatibilityResult.Compatible), removed)
        }
    },
    test("unreadable TASTy cannot hide an absent bundled class or bypass matched-class analysis") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("compiler"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            old     <- compile(root.resolve("old"), session, "def kept: Int = 1")
            _       <- PluginCompileSupport.write(old.resolve("fixture/Api.tasty"), "unreadable metadata")
            jar     <- baselineJar(root.resolve("bundled.jar"), old)
            absent  <- ZIO
              .attemptBlocking(Files.createDirectories(root.resolve("absent")))
              .mapError(_ => PluginCompileError.Io(IoFailure(root, IoFault.Write)))
            results <- ZIO
              .serviceWithZIO[CompatibilityAnalyzer] { analyzer =>
                for
                  missing <- analyzer.analyze(CompatibilityInput(Platform.Jvm, jar, absent)).either
                  matched <- analyzer.analyze(CompatibilityInput(Platform.Jvm, jar, old)).either
                yield (missing, matched)
              }
              .provideLayer(CompatibilityAnalyzer.jvm)
          yield results
        }
        .map { ran =>
          val (missingResult, matchedResult) = ran.value
          val missing                        = missingResult match
            case Right(CompatibilityResult.Incompatible(problems)) =>
              problems.contains(CompatibilityProblem.MissingClass("fixture.Api"))
            case _ => false
          val matched = matchedResult match
            case Left(CompatibilityError.AnalysisFailed(_, _)) => true
            case _                                             => false
          assertTrue(missing, matched)
        }
    },
  )

  def spec =
    (proof @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)).provideShared(histos.zinc.Compilers.owned)

  private def compile(
      root: Path,
      session: HostCompilerSession,
      member: String,
  ): ZIO[PluginCompileSupport.Env & Scope, PluginCompileError, Path] =
    for
      source <- PluginCompileSupport.write(
        root.resolve("src/fixture/Api.scala"),
        s"package fixture\nclass Api:\n  $member\n",
      )
      _ <- PluginCompiler.compile(
        PluginCompileJob(
          PluginCompileSupport.App,
          session,
          root,
          root,
          List(source),
          classes = root.resolve("classes"),
          analysis = root.resolve("analysis"),
        )
      )
    yield root.resolve("classes")

  private def baselineJar(path: Path, classes: Path): IO[PluginCompileError, Path] =
    ZIO.scoped:
      for
        jar <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(new JarOutputStream(Files.newOutputStream(path)))
            .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Write)))
        )(jar => ZIO.attemptBlocking(jar.close()).orDie)
        walk <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(Files.walk(classes))
            .mapError(_ => PluginCompileError.Io(IoFailure(classes, IoFault.Read)))
        )(walk => ZIO.attemptBlocking(walk.close()).orDie)
        _ <- ZIO
          .attemptBlocking:
            walk.iterator.asScala.filter(Files.isRegularFile(_)).foreach { source =>
              jar.putNextEntry(new JarEntry(classes.relativize(source).toString.replace('\\', '/')))
              jar.write(Files.readAllBytes(source))
              jar.closeEntry()
            }
          .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Write)))
      yield path
end Scala3CompatibilitySpec
