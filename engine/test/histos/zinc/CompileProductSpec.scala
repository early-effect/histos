package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkRuntime, Sha256Digest}
import histos.load.{PluginCompileError, PluginCompileSupport}
import histos.toolchain.{HostCompilerError, HostCompilerSession, HostToolRole, MaterializedTool, RuntimeIdentity}
import histos.{Platform, ScalacFlags}

import java.nio.file.Path

import zio.test.*
import zio.{Chunk, durationInt, ZIO}

object CompileProductSpec extends ZIOSpecDefault:
  def spec = suite("a compile product")(
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
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private def markSource(text: String): String =
    s"""|object Mark:
        |  def text: String = "$text"
        |""".stripMargin

  private def otherSource: String =
    """|object Other:
       |  def name: String = "other"
       |""".stripMargin

  private def compile(
      base: Path,
      classes: Path,
      analysis: Path,
      sources: List[Path],
      session: HostCompilerSession,
      tools: Tools,
  ): ZIO[PluginCompileSupport.Env, PluginCompileError, CompileOutput] =
    val request = CompileRequest(
      CompileToolchain(session.manifest.scalaVersion, Platform.Jvm, jdk(session), ScalacFlags.empty),
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
          tools.runtime,
          tools.compiler,
          tools.bridge,
          tools.support,
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
end CompileProductSpec
