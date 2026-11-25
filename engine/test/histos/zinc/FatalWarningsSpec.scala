package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkRuntime, Sha256Digest}
import histos.load.{PluginCompileError, PluginCompileSupport}
import histos.suite.HistosSpec
import histos.toolchain.{HostCompilerError, HostCompilerSession, HostToolRole, MaterializedTool, RuntimeIdentity}
import histos.{Platform, ScalacFlag, ScalacFlags}

import java.nio.file.Path

import zio.test.*
import zio.{Chunk, Exit, durationInt, ZIO}

object FatalWarningsSpec extends HistosSpec:
  def spec = suite("fatal warnings")(
    test("-Werror is the fatal-warnings flag and no other case here borrows it") {
      val fatal = ScalacFlags.of(ScalacFlag.FatalWarnings)
      val other = ScalacFlags.of(ScalacFlag.Deprecation)
      assertTrue(
        fatal.argv.contains("-Werror"),
        !other.argv.contains("-Werror"),
        fatal.canonical == "fatal-warnings",
      )
    },
    test("an unused local fails only when warnings are fatal") {
      PluginCompileSupport
        .run { (root, _) =>
          val base = root.resolve("unused")
          for
            opened <- open(root)
            source <- PluginCompileSupport.write(base.resolve("src/Probe.scala"), unusedSource)
            fatal  <- compile(
              base,
              base.resolve("fatal/classes"),
              base.resolve("fatal/analysis"),
              List(source),
              opened,
              ScalacFlags.of(ScalacFlag.UnusedAll, ScalacFlag.FatalWarnings),
            ).exit
            plain <- compile(
              base,
              base.resolve("plain/classes"),
              base.resolve("plain/analysis"),
              List(source),
              opened,
              ScalacFlags.of(ScalacFlag.UnusedAll),
            )
          yield (fatal, plain)
        }
        .map { ran =>
          val (fatal, plain) = ran.value
          assertTrue(refusal(fatal) == "failed", plain.classFiles > 0)
        }
    },
  ).provideShared(Compilers.owned) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private def unusedSource: String =
    """|object Probe:
       |  def answer: Int =
       |    val unused = 1
       |    2
       |""".stripMargin

  private def refusal(exit: Exit[PluginCompileError, CompileOutput]): String =
    exit match
      case Exit.Failure(cause) =>
        cause.failureOption match
          case Some(PluginCompileError.Compile(CompileError.Failed(_))) => "failed"
          case Some(error)                                              => error.message
          case None                                                     => "died"
      case Exit.Success(_) => "compiled"

  private final case class Opened(session: HostCompilerSession, tools: Tools)

  private def open(root: Path): ZIO[PluginCompileSupport.Env & zio.Scope, PluginCompileError, Opened] =
    for
      store   <- ZIO.service[ContentStore]
      payload <- PluginCompileSupport
        .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
        .mapError(PluginCompileError.Host(_))
      session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
      tools   <- ZIO.fromEither(split(session))
    yield Opened(session, tools)

  private def compile(
      base: Path,
      classes: Path,
      analysis: Path,
      sources: List[Path],
      opened: Opened,
      flags: ScalacFlags,
  ): ZIO[PluginCompileSupport.Env, PluginCompileError, CompileOutput] =
    val request = CompileRequest(
      CompileToolchain(opened.session.manifest.scalaVersion, Platform.Jvm, jdk(opened.session), flags),
      sources,
      Nil,
      classes,
      analysis,
      Nil,
      Nil,
      checkout = base,
    )
    for
      digest <- JvmCompile.sourceDigest(base, sources).mapError(PluginCompileError.Compile(_))
      output <- JvmCompile
        .compileSupplied(
          request,
          opened.tools.runtime,
          opened.tools.compiler,
          opened.tools.bridge,
          opened.tools.support,
          CompileInputs.declared(request, digest),
          CompileKey.of(request, digest),
        )
        .mapError(PluginCompileError.Compile(_))
    yield output

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
end FatalWarningsSpec
