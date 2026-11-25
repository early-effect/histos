package histos.load

import histos.cache.ContentStore
import histos.plugin.PluginCoordinate
import histos.suite.HistosSpec
import histos.toolchain.HostCompilerSession
import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.CompileError
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  ScalacFlag,
  ScalacFlags,
  SourcePos,
  Splits,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.test.*
import zio.{durationInt, Scope, ZIO}

object BuildFlagsSpec extends HistosSpec:
  private val Strict   = ScalacFlags.of(ScalacFlag.Deprecation, ScalacFlag.FatalWarnings)
  private val Warnings = ScalacFlags.of(ScalacFlag.Deprecation)
  private val Warning  =
    """|object Warning:
       |  @deprecated("use text", "1") def old: String = "compiled"
       |  def text: String = old
       |""".stripMargin

  private val suiteValue: Spec[histos.zinc.Compilers, PluginCompileError | LoadError] =
    suite("build compiler flags")(
      test("direct cache applies fatal warnings, invalidates flags and extra identity, and restores classes") {
        PluginCompileSupport
          .run { (root, starts) =>
            for
              session <- prepared(root)
              source  <- PluginCompileSupport.write(root.resolve("src/Warning.scala"), Warning)
              out     <- ZIO
                .attemptBlocking(Files.createDirectories(root.resolve("classes")))
                .mapError(_ => PluginCompileError.Io(IoFailure(root, IoFault.Write)))
              runtime = PluginCompileSupport.runtimeJars(session)
              first <- BuildCache.compile(List(source), runtime, out, "resources").either
              _     <- ZIO
                .attemptBlocking(Files.deleteIfExists(out.resolve("Warning$.class")))
                .mapError(_ => PluginCompileError.Io(IoFailure(out, IoFault.Write)))
              again    <- BuildCache.compile(List(source), runtime, out, "resources", BuildScalac.bootstrapFlags).either
              warm     <- starts.get
              text     <- PluginCompileSupport.invoke(out, runtime, "Warning$", "text")
              warnings <- BuildCache.compile(List(source), runtime, out, "resources", Warnings).either
              warningsAgain <- BuildCache.compile(List(source), runtime, out, "resources", Warnings).either
              changed       <- starts.get
              failed        <- BuildCache.compile(List(source), runtime, out, "resources", Strict).either
              failedAgain   <- BuildCache.compile(List(source), runtime, out, "resources", Strict).either
              extra <- BuildCache.compile(List(source), runtime, out, "changed-resources", ScalacFlags.empty).either
              extraAgain <- BuildCache
                .compile(List(source), runtime, out, "changed-resources", ScalacFlags.empty)
                .either
              restored     <- BuildCache.compile(List(source), runtime, out, "resources").either
              restoredText <- PluginCompileSupport.invoke(out, runtime, "Warning$", "text")
              labeled      <- BuildFile.compileLabeled(List(source -> "histos.scala"), runtime, out, Strict).either
            yield (
              List(first, again, warnings, warningsAgain, extra, extraAgain, restored).forall(_.contains(())),
              warm,
              changed,
              text,
              restoredText,
              loadRejected(failed) && loadRejected(failedAgain) && loadRejected(labeled),
            )
          }
          .map { ran =>
            val (compiled, warm, changed, text, restored, rejected) = ran.value
            assertTrue(
              compiled,
              warm == 1,
              changed == 2,
              text == "compiled",
              restored == "compiled",
              rejected,
              ran.starts == 5,
            )
          }
      },
      hosted(Mode.Hosted),
      hosted(Mode.Worker),
      modules(Bootstrap.Direct),
      modules(Bootstrap.Hosted),
    )

  private enum Mode:
    case Hosted, Worker

  private enum Bootstrap:
    case Direct, Hosted

  private def prepared(root: Path): ZIO[PluginCompileSupport.Env & Scope, PluginCompileError, HostCompilerSession] =
    for
      store   <- ZIO.service[ContentStore]
      payload <- PluginCompileSupport
        .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
        .mapError(PluginCompileError.Host(_))
      session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
    yield session

  private def hosted(mode: Mode): Spec[histos.zinc.Compilers, PluginCompileError] =
    test(s"$mode applies flags, invalidates keys, and reuses successful classes") {
      PluginCompileSupport
        .run { (root, starts) =>
          for
            session <- prepared(root)
            source  <- PluginCompileSupport.write(root.resolve("src/Warning.scala"), Warning)
            stages  <- mode match
              case Mode.Hosted => ZIO.succeed(Nil)
              case Mode.Worker =>
                ZIO
                  .fromEither(
                    PluginCompileSupport.frozen("src/Warning.scala", Warning.getBytes(StandardCharsets.UTF_8))
                  )
                  .map(file => List(PluginCompileSupport.declared("Warning", file, "FLAG_PROBE", "fixed")))
            job = PluginCompileJob(
              PluginCompileSupport.App,
              session,
              root,
              root,
              List(source),
              classes = root.resolve("classes"),
              analysis = root.resolve("analysis"),
              stages = stages,
              flags = BuildScalac.bootstrapFlags,
            )
            first <- PluginCompiler.compile(job)
            _     <- ZIO
              .attemptBlocking(Files.deleteIfExists(job.classes.resolve("Warning$.class")))
              .mapError(_ => PluginCompileError.Io(IoFailure(job.classes, IoFault.Write)))
            again         <- PluginCompiler.compile(job)
            warm          <- starts.get
            warnings      <- PluginCompiler.compile(job.copy(flags = Warnings))
            warningsAgain <- PluginCompiler.compile(job.copy(flags = Warnings))
            changed       <- starts.get
            text          <- PluginCompileSupport.invoke(
              job.classes,
              PluginCompileSupport.runtimeJars(session),
              "Warning$",
              "text",
            )
            failed       <- PluginCompiler.compile(job.copy(flags = Strict)).either
            failedAgain  <- PluginCompiler.compile(job.copy(flags = Strict)).either
            restored     <- PluginCompiler.compile(job)
            restoredText <- PluginCompileSupport.invoke(
              job.classes,
              PluginCompileSupport.runtimeJars(session),
              "Warning$",
              "text",
            )
          yield (
            first.key == again.key && first.key == restored.key,
            first.key != warnings.key && warnings.key == warningsAgain.key,
            warm,
            changed,
            text,
            restoredText,
            failed,
            failedAgain,
          )
        }
        .map { ran =>
          val (same, changedKey, warm, changed, text, restored, failed, failedAgain) = ran.value
          assertTrue(
            same,
            changedKey,
            warm == 1,
            changed == 2,
            text == "compiled",
            restored == "compiled",
            rejected(mode, failed),
            rejected(mode, failedAgain),
            ran.starts == 4,
          ).label(s"first strict compile: $failed; repeated strict compile: $failedAgain")
        }
    }

  private def modules(bootstrap: Bootstrap): Spec[histos.zinc.Compilers, PluginCompileError | LoadError] =
    test(s"$bootstrap plugin modules apply flags and retain resource identity") {
      PluginCompileSupport
        .run { (root, starts) =>
          val effect = for
            session <- prepared(root)
            _       <- PluginCompileSupport.write(root.resolve("helper/src/main/scala/Warning.scala"), Warning)
            note    <- PluginCompileSupport.write(root.resolve("helper/src/main/resources/note.txt"), "alpha")
            hosted = bootstrap match
              case Bootstrap.Direct => None
              case Bootstrap.Hosted => Some(session)
            runtime = PluginCompileSupport.runtimeJars(session)
            first   <- PluginModules.compileCached(root, graph(ScalacFlags.empty), runtime, None, hosted)
            again   <- PluginModules.compileCached(root, graph(ScalacFlags.empty), runtime, None, hosted)
            warm    <- starts.get
            _       <- PluginCompileSupport.write(note, "beta")
            changed <- PluginModules.compileCached(root, graph(ScalacFlags.empty), runtime, None, hosted)
            classes <- ZIO
              .fromOption(changed.lastOption)
              .orElseFail(PluginCompileError.Io(IoFailure(root, IoFault.Stat)))
            resource <- ZIO
              .attemptBlocking(Files.readString(classes.resolve("note.txt")))
              .mapError(_ => PluginCompileError.Io(IoFailure(classes, IoFault.Read)))
            failed <- PluginModules.compileCached(root, graph(Strict), runtime, None, hosted).either
            reused <- ZIO.fromOption(again.lastOption).orElseFail(PluginCompileError.Io(IoFailure(root, IoFault.Stat)))
            text   <- PluginCompileSupport.invoke(reused, runtime, "Warning$", "text")
          yield (first.nonEmpty, warm, resource, loadRejected(failed), text)
          effect.either
        }
        .flatMap { ran =>
          ZIO.fromEither(ran.value).map { (compiled, warm, resource, failed, text) =>
            assertTrue(compiled, warm == 1, resource == "beta", failed, text == "compiled", ran.starts == 3)
          }
        }
    }

  private def graph(flags: ScalacFlags): ProjectGraph =
    val name   = ProjectName("helper")
    val module = Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion.trusted("3.10.1"), Nil),
      Nil,
      Nil,
      SourcePos("histos.plugins.scala", 1, 1),
      flags = flags,
    )
    ProjectGraph.trusted(
      List(module),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      coordinates = List(PluginCoordinate.Module(name)),
    )

  private def loadRejected[A](result: Either[LoadError, A]): Boolean =
    result match
      case Left(LoadError.CompileFailed(messages)) => messages.exists(_.text.contains("warnings"))
      case _                                       => false

  private def rejected(mode: Mode, result: Either[PluginCompileError, PluginCompileResult]): Boolean =
    (mode, result) match
      case (Mode.Hosted, Left(PluginCompileError.Compile(CompileError.Failed(diagnostics)))) =>
        diagnostics.exists(_.text.contains("warnings"))
      case (Mode.Worker, Left(PluginCompileError.Worker(code, detail))) => code != 0 && detail.contains("warnings")
      case _                                                            => false

  def spec =
    (suiteValue @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)).provideShared(
      histos.zinc.Compilers.owned
    )
end BuildFlagsSpec
