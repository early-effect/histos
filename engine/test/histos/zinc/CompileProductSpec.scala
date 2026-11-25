package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkRuntime, Sha256Digest}
import histos.load.{PluginCompileError, PluginCompileSupport}
import histos.suite.HistosSpec
import histos.toolchain.{HostCompilerError, HostCompilerSession, HostToolRole, MaterializedTool, RuntimeIdentity}
import histos.{Platform, ScalacFlags}

import java.nio.file.Path

import zio.test.*
import zio.{Chunk, durationInt, ZIO}

object CompileProductSpec extends HistosSpec:
  def spec = suite("a compile product")(
    test("orphaned consumer TASTy cannot shadow a changed upstream constructor") {
      PluginCompileSupport
        .run { (root, _) =>
          val base    = root.resolve("shape")
          val library = cell(base, "library")
          val caller  = cell(base, "caller")
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            tools   <- ZIO.fromEither(split(session))
            shape   <- PluginCompileSupport
              .write(base.resolve("src/Shape.scala"), "final case class Shape(value: String)\n")
            _ <- compile(base, library, analysisOf(library), List(shape), session, tools)
            _ <- ZIO
              .attemptBlocking {
                java.nio.file.Files.createDirectories(caller)
                java.nio.file.Files.copy(library.resolve("Shape.tasty"), caller.resolve("Shape.tasty"))
              }
              .mapError(_ => PluginCompileError.Io(histos.workspace.IoFailure(caller, histos.workspace.IoFault.Write)))
            _   <- PluginCompileSupport.write(shape, "final case class Shape(value: String, marker: String)\n")
            _   <- compile(base, library, analysisOf(library), List(shape), session, tools)
            use <- PluginCompileSupport.write(
              base.resolve("src/Use.scala"),
              "object Use:\n  def text: String = Shape(\"value\", \"new\").marker\n",
            )
            compiled <- compile(base, caller, analysisOf(caller), List(use), session, tools, List(library))
            text     <- PluginCompileSupport.invoke(caller, library :: tools.runtime, "Use$", "text")
            warm     <- compile(base, caller, analysisOf(caller), List(use), session, tools, List(library))
            orphan   <- ZIO
              .attemptBlocking(java.nio.file.Files.exists(caller.resolve("Shape.tasty")))
              .mapError(_ => PluginCompileError.Io(histos.workspace.IoFailure(caller, histos.workspace.IoFault.Stat)))
          yield (compiled.classesWritten, text, warm.changed, orphan)
        }
        .map { ran =>
          val (written, text, changed, orphan) = ran.value
          assertTrue(written.exists(_ > 0), text == "new", !changed, !orphan)
        }
    },
    test("a failed incremental request retires its analysis before the corrected retry") {
      PluginCompileSupport
        .run { (root, _) =>
          val base     = root.resolve("retry")
          val classes  = base.resolve("classes")
          val analysis = base.resolve("analysis")
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session  <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            tools    <- ZIO.fromEither(split(session))
            source   <- PluginCompileSupport.write(base.resolve("src/Mark.scala"), markSource("one"))
            _        <- compile(base, classes, analysis, List(source), session, tools)
            _        <- PluginCompileSupport.write(source, "object Mark:\n  def text: String = 1\n")
            failed   <- compile(base, classes, analysis, List(source), session, tools).either
            retained <- ZIO
              .attemptBlocking(java.nio.file.Files.exists(analysis.resolve("inc.zip")))
              .mapError(_ => PluginCompileError.Io(histos.workspace.IoFailure(analysis, histos.workspace.IoFault.Stat)))
            _         <- PluginCompileSupport.write(source, markSource("fixed"))
            corrected <- compile(base, classes, analysis, List(source), session, tools)
            text      <- PluginCompileSupport.invoke(classes, tools.runtime, "Mark$", "text")
            warm      <- compile(base, classes, analysis, List(source), session, tools)
          yield (failed, retained, corrected.classesWritten, text, warm.changed)
        }
        .map { ran =>
          val (failed, retained, written, text, changed) = ran.value
          assertTrue(failed.isLeft, !retained, written.exists(_ > 0), text == "fixed", !changed)
        }
    },
    test("a changed source is stored as the new class file") {
      PluginCompileSupport
        .run { (root, _) =>
          val base     = root.resolve("edit")
          val classes  = base.resolve("classes")
          val analysis = base.resolve("analysis")
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            tools   <- ZIO.fromEither(split(session))
            mark    <- PluginCompileSupport.write(base.resolve("src/Mark.scala"), markSource("one"))
            other   <- PluginCompileSupport.write(base.resolve("src/Other.scala"), otherSource)
            _       <- compile(base, classes, analysis, List(mark, other), session, tools)
            first   <- PluginCompileSupport.invoke(classes, tools.runtime, "Mark$", "text")
            _       <- PluginCompileSupport.write(mark, markSource("two"))
            second  <- compile(base, classes, analysis, List(mark, other), session, tools)
            edited  <- PluginCompileSupport.invoke(classes, tools.runtime, "Mark$", "text")
            third   <- compile(base, classes, analysis, List(mark, other), session, tools)
            stored  <- PluginCompileSupport.invoke(classes, tools.runtime, "Mark$", "text")
          yield (first, edited, stored, second.classesWritten, third.changed)
        }
        .map { ran =>
          val (first, edited, stored, wrote, recompiled) = ran.value
          assertTrue(
            first == "one",
            edited == "two",
            stored == "two",
            wrote.exists(_ > 0),
            !recompiled,
          )
        }
    },
    test("a signature change in the same module recompiles the unchanged test") {
      PluginCompileSupport
        .run { (root, _) =>
          val base = root.resolve(".histos")
          val main = cell(base, "main")
          val test = cell(base, "test")
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            tools   <- ZIO.fromEither(split(session))
            lib     <- PluginCompileSupport.write(base.resolve("src/Lib.scala"), libSource("n + 1"))
            use     <- PluginCompileSupport.write(base.resolve("src/Use.scala"), useSource)
            idle    <- PluginCompileSupport.write(base.resolve("src/Idle.scala"), idleSource)
            _       <- compile(base, main, analysisOf(main), List(lib), session, tools)
            _       <- compile(base, test, analysisOf(test), List(use, idle), session, tools, List(main))
            _       <- PluginCompileSupport.write(lib, libSource("n + m", ", m: Int = 0"))
            _       <- compile(base, main, analysisOf(main), List(lib), session, tools)
            grown   <- compile(base, test, analysisOf(test), List(use, idle), session, tools, List(main))
            linked  <- PluginCompileSupport.invoke(test, main :: tools.runtime, "Use$", "text")
          yield (grown.sources, linked)
        }
        .map { ran =>
          val (grown, linked) = ran.value
          assertTrue(grown.exists(count => count.compiled == 1 && count.skipped == 1), linked == "1")
        }
    },
    test("a signature change recompiles the unchanged caller") {
      PluginCompileSupport
        .run { (root, _) =>
          val base    = root.resolve(".histos")
          val library = cell(base, "library")
          val caller  = cell(base, "caller")
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session  <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            tools    <- ZIO.fromEither(split(session))
            lib      <- PluginCompileSupport.write(base.resolve("src/Lib.scala"), libSource("n + 1"))
            use      <- PluginCompileSupport.write(base.resolve("src/Use.scala"), useSource)
            idle     <- PluginCompileSupport.write(base.resolve("src/Idle.scala"), idleSource)
            _        <- compile(base, library, analysisOf(library), List(lib), session, tools)
            _        <- compile(base, caller, analysisOf(caller), List(use, idle), session, tools, List(library))
            recorded <- ZIO
              .fromOption(UpstreamApis.referenced(analysisOf(caller)))
              .orElseFail(
                PluginCompileError.Io(histos.workspace.IoFailure(analysisOf(caller), histos.workspace.IoFault.Read))
              )
            _      <- PluginCompileSupport.write(lib, libSource("n + m", ", m: Int = 0"))
            _      <- compile(base, library, analysisOf(library), List(lib), session, tools)
            grown  <- compile(base, caller, analysisOf(caller), List(use, idle), session, tools, List(library))
            linked <- PluginCompileSupport.invoke(caller, library :: tools.runtime, "Use$", "text")
            _      <- PluginCompileSupport.write(lib, libSource("n + m + 1", ", m: Int = 0"))
            _      <- compile(base, library, analysisOf(library), List(lib), session, tools)
            body   <- compile(base, caller, analysisOf(caller), List(use, idle), session, tools, List(library))
          yield (recorded, grown.sources, linked, body.sources)
        }
        .map { ran =>
          val (recorded, grown, linked, body) = ran.value
          assertTrue(
            recorded.contains(BinaryName("Lib$")),
            grown.exists(count => count.compiled == 1 && count.skipped == 1),
            linked == "1",
            body.exists(count => count.compiled == 0 && count.skipped == 2),
          )
        }
    },
    test("older classes stored under the edited source's key are compiled again") {
      PluginCompileSupport
        .run { (root, _) =>
          val base     = root.resolve("stale")
          val classes  = base.resolve("classes")
          val analysis = base.resolve("analysis")
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            tools   <- ZIO.fromEither(split(session))
            mark    <- PluginCompileSupport.write(base.resolve("src/Mark.scala"), markSource("one"))
            other   <- PluginCompileSupport.write(base.resolve("src/Other.scala"), otherSource)
            sources = List(mark, other)
            _ <- compile(base, classes, analysis, sources, session, tools)
            _ <- PluginCompileSupport.write(mark, markSource("two"))
            request = requestOf(base, classes, analysis, sources, session, Nil)
            digest <- JvmCompile.sourceDigest(base, sources).mapError(PluginCompileError.Compile(_))
            _      <- CompileReplay
              .save(
                store,
                CompileKey.of(request, digest),
                request,
                tools.runtime,
                tools.compiler,
                tools.bridge,
              )
              .mapError(PluginCompileError.Compile(_))
            again <- compile(base, classes, analysis, sources, session, tools)
            text  <- PluginCompileSupport.invoke(classes, tools.runtime, "Mark$", "text")
            third <- compile(base, classes, analysis, sources, session, tools)
          yield (text, again.miss.isDefined, third.changed)
        }
        .map { ran =>
          val (text, missed, hit) = ran.value
          assertTrue(text == "two", missed, !hit)
        }
    },
  ).provideShared(Compilers.owned) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private def markSource(text: String): String =
    s"""|object Mark:
        |  def text: String = "$text"
        |""".stripMargin

  private def otherSource: String =
    """|object Other:
       |  def name: String = "other"
       |""".stripMargin

  /** The analysis directory of a `.histos/classes` cell. */
  private def analysisOf(classes: Path): Path =
    GraphCompile.analysisDirOf(classes).getOrElse(classes.resolveSibling("missing-analysis"))

  /** One JVM cell under `.histos`, the layout `classpathAnalysis` reads. */
  private def cell(root: Path, name: String): Path =
    root.resolve("classes").resolve(name).resolve("3.10.1").resolve("jvm")

  /** `extra` is the parameter list after `n: Int`, including its leading comma. The body reads `n` and, when present,
    * `m`.
    */
  private def libSource(body: String, extra: String = ""): String =
    s"""|object Lib:
        |  def f(n: Int$extra): Int = $body
        |""".stripMargin

  private def idleSource: String =
    """|object Idle:
       |  def name: String = "idle"
       |""".stripMargin

  private def useSource: String =
    """|object Use:
       |  def text: String = Lib.f(1).toString
       |""".stripMargin

  private def requestOf(
      base: Path,
      classes: Path,
      analysis: Path,
      sources: List[Path],
      session: HostCompilerSession,
      classpath: List[Path],
  ): CompileRequest =
    CompileRequest(
      CompileToolchain(session.manifest.scalaVersion, Platform.Jvm, jdk(session), ScalacFlags.empty),
      sources,
      classpath,
      classes,
      analysis,
      Nil,
      Nil,
      checkout = base,
      upstream = signatures(classpath.map(analysisOf)),
    )

  private def compile(
      base: Path,
      classes: Path,
      analysis: Path,
      sources: List[Path],
      session: HostCompilerSession,
      tools: Tools,
      classpath: List[Path] = Nil,
  ): ZIO[PluginCompileSupport.Env, PluginCompileError, CompileOutput] =
    val request = requestOf(base, classes, analysis, sources, session, classpath)
    for
      digest <- JvmCompile.sourceDigest(base, sources).mapError(PluginCompileError.Compile(_))
      output <- JvmCompile
        .compileSupplied(
          request,
          tools.runtime,
          tools.compiler,
          tools.bridge,
          tools.support,
          CompileInputs.declared(request, digest),
          CompileKey.of(request, digest),
        )
        .mapError(PluginCompileError.Compile(_))
    yield output

  /** Signatures recorded under each analysis directory. A directory with no analysis contributes nothing. */
  private def signatures(analyses: List[Path]): List[histos.zinc.ClassSignature] =
    analyses.flatMap(dir => histos.zinc.UpstreamApis.readAnalysis(dir).getOrElse(Nil))

  private def jdk(session: HostCompilerSession): JdkRuntime =
    JdkRuntime(
      session.jdkHome,
      session.jdk,
      RuntimeIdentity.Local(session.jdkHome, Sha256Digest.of(Chunk.fromArray(session.jdk.value.getBytes))),
    )

  private final case class Tools(compiler: List[Path], bridge: Path, support: List[Path], runtime: List[Path])

  private def split(session: HostCompilerSession): Either[PluginCompileError, Tools] =
    def jars(role: HostToolRole): List[MaterializedTool] =
      session.tools.filter(_.entry.role == role)
    jars(HostToolRole.Bridge) match
      case tool :: Nil =>
        val library = jars(HostToolRole.Support).filter(_.entry.coordinate.name.startsWith("scala3-library"))
        val support = jars(HostToolRole.Support).filterNot(_.entry.coordinate.name.startsWith("scala3-library"))
        Right(
          Tools(
            jars(HostToolRole.Compiler).map(_.path),
            tool.path,
            support.map(_.path),
            jars(HostToolRole.Runtime).map(_.path) ++ library.map(_.path),
          )
        )
      case _ => Left(PluginCompileError.Host(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))
end CompileProductSpec
