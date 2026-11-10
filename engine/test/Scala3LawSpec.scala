package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{
  Classpath,
  DependencyLock,
  LockedTools,
  Repositories,
  ResolutionService,
  Resolve,
  ResolveError,
  ResolvedArtifact,
  Repository,
  ToolRequest,
  Transitivity,
}
import histos.workspace.Workspace
import histos.{ScalaVersion, ToolCatalog}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import slayer.{any, stub, stubbed}

import zio.{Ref, Scope, ZIO, ZLayer}
import zio.test.*

object Scala3LawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.7.0")
  private val repos  = Repositories.central

  def spec = suite("scala 3 toolchain laws")(
    test("a text file is refused, an unknown zip is support, and the runtime is the merged or split library"):
      val request = CompilerRequests.compiler(scala3, repos)
      val program =
        for
          held    <- Ref.make[Either[ResolveError, Classpath]](Right(Classpath(Nil, Nil)))
          _       <- stub[ResolutionService](_.tool(any[ToolRequest]))(held.get.flatMap(ZIO.fromEither))
          service <- ZIO.service[ResolutionService]
          resolve <- ZIO.service[Resolve]
          root    <- Workspace.work("histos-scala3")
          tools    = LockedTools(DependencyLock(Nil), ToolCatalog.default, repos, resolve, Some(service))
          bridge   = zip(root, "bridge.jar", List(clazz("dotty/tools/xsbt/CompilerBridge.class")))
          bridge2  = zip(root, "bridge2.jar", List(clazz("dotty/tools/xsbt/CompilerBridge.class")))
          compiler = zip(root, "compiler.jar", List(clazz("dotty/tools/dotc/Driver.class")))
          mixed    = zip(
            root,
            "mixed.jar",
            List(
              clazz("xsbti/AnalysisCallback.class"),
              clazz("scala/collection/immutable/List.class"),
              properties(scala3.value),
              clazz("scala/runtime/Scala3RunTime.class"),
            ),
          )
          util   = zip(root, "util.jar", List(clazz("com/example/Util.class")))
          empty  = zip(root, "empty.jar", Nil)
          util2  = zip(root, "util2.jar", List(clazz("com/example/Extra.class")))
          stdlib = zip(
            root,
            "stdlib.jar",
            List(
              clazz("scala/collection/immutable/List.class"),
              clazz("scala/runtime/Scala3RunTime.class"),
              properties(scala3.value),
            ),
          )
          legacy = zip(root, "legacy.jar", List(clazz("scala/collection/immutable/List.class"), properties("2.13.16")))
          scala3lib  = zip(root, "scala3lib.jar", List(clazz("scala/runtime/Scala3RunTime.class")))
          scala3lib2 = zip(root, "scala3lib2.jar", List(clazz("scala/runtime/Scala3RunTime.class")))
          wrong      = zip(
            root,
            "wrong.jar",
            List(
              clazz("scala/collection/immutable/List.class"),
              clazz("scala/runtime/Scala3RunTime.class"),
              properties("3.8.0"),
            ),
          )
          stdlib2 = zip(
            root,
            "stdlib2.jar",
            List(
              clazz("scala/collection/immutable/List.class"),
              clazz("scala/runtime/Scala3RunTime.class"),
              properties(scala3.value),
            ),
          )
          unmarked = zip(
            root,
            "unmarked.jar",
            List(clazz("scala/collection/immutable/List.class"), clazz("scala/runtime/Scala3RunTime.class")),
          )
          text <- ZIO.attemptBlocking:
            val path = root.resolve("notes.txt")
            Files.writeString(path, "not a jar")
            path
          base = List(bridge, compiler, mixed, util, empty, stdlib, util2)
          refused    <- pull(held, tools, Left(ResolveError.NoTool(request.id)))
          ok         <- pull(held, tools, Right(base))
          noted      <- pull(held, tools, Right(text :: base))
          onlyText   <- pull(held, tools, Right(List(text)))
          split      <- pull(held, tools, Right(List(bridge, compiler, legacy, scala3lib)))
          extra      <- pull(held, tools, Right(List(bridge, compiler, stdlib, scala3lib)))
          absent     <- pull(held, tools, Right(List(bridge, compiler, legacy)))
          doubled    <- pull(held, tools, Right(List(bridge, compiler, legacy, scala3lib, scala3lib2)))
          mismatch   <- pull(held, tools, Right(List(bridge, compiler, wrong)))
          twoLibs    <- pull(held, tools, Right(List(bridge, compiler, stdlib, stdlib2)))
          noLib      <- pull(held, tools, Right(List(bridge, compiler, util)))
          noBridge   <- pull(held, tools, Right(List(stdlib)))
          bridges    <- pull(held, tools, Right(List(bridge, bridge2, compiler, stdlib)))
          properties <- pull(held, tools, Right(List(bridge, compiler, unmarked)))
        yield assertTrue(
          refused == Left(CompileError.Resolved(ResolveError.NoTool(request.id))),
          view(ok) == Some(
            Seen(
              List(stdlib),
              List(compiler, util, empty, util2),
              List(compiler, util, empty, util2, stdlib),
              bridge,
              LoaderBoundary.Engine,
            )
          ),
          noted == Left(CompileError.Toolchain(ToolchainMiss.Unexpected(scala3, List(text)))),
          onlyText == Left(CompileError.Toolchain(ToolchainMiss.Unexpected(scala3, List(text)))),
          view(split) == Some(
            Seen(
              List(legacy, scala3lib),
              List(compiler),
              List(compiler, legacy, scala3lib),
              bridge,
              LoaderBoundary.Engine,
            )
          ),
          extra == Left(CompileError.Toolchain(ToolchainMiss.Duplicated(scala3, CompilerPart.Scala3Library))),
          absent == Left(CompileError.Toolchain(ToolchainMiss.Missing(scala3, CompilerPart.Scala3Library))),
          doubled == Left(CompileError.Toolchain(ToolchainMiss.Duplicated(scala3, CompilerPart.Scala3Library))),
          mismatch == Left(CompileError.Toolchain(ToolchainMiss.Library(scala3, ScalaVersion("3.8.0")))),
          twoLibs == Left(CompileError.Toolchain(ToolchainMiss.Duplicated(scala3, CompilerPart.Library))),
          noLib == Left(CompileError.Toolchain(ToolchainMiss.Missing(scala3, CompilerPart.Library))),
          noBridge == Left(CompileError.Toolchain(ToolchainMiss.Missing(scala3, CompilerPart.Scala3Bridge))),
          bridges == Left(CompileError.Toolchain(ToolchainMiss.Duplicated(scala3, CompilerPart.Scala3Bridge))),
          properties == Left(CompileError.Toolchain(ToolchainMiss.Missing(scala3, CompilerPart.Library))),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[ResolutionService] ++ stubbed[Resolve])
    ,
    test("a Scala 3 compiler pins jline-reader and a Scala 2 compiler does not"):
      val current = CompilerRequests.compiler(scala3, repos)
      val scala2  = CompilerRequests.compiler(ScalaVersion("2.13.18"), repos)
      assertTrue(
        current.roots.map(root => (root.organization, root.name, root.revision)) == List(
          ("org.scala-lang", "scala3-compiler_3", scala3.value),
          ("org.scala-lang", "scala3-sbt-bridge", scala3.value),
          ("org.jline", "jline-reader", "4.3.1"),
          ("org.jline", "jline-terminal-jni", "4.3.1"),
        ),
        current.roots.forall(_.transitivity == Transitivity.Closure),
        scala2.roots.forall(root => root.organization != "org.jline"),
      ),
  )

  private final case class Seen(
      runtime: List[Path],
      compiler: List[Path],
      loader: List[Path],
      bridge: Path,
      boundary: LoaderBoundary,
  )

  private def pull(
      held: Ref[Either[ResolveError, Classpath]],
      tools: LockedTools,
      answer: Either[ResolveError, List[Path]],
  ) =
    val classpath = answer.map(paths => Classpath(paths.map(artifact), Nil))
    held.set(classpath) *> Scala3.jars(scala3).provide(ZLayer.succeed(tools)).either

  private def view(result: Either[CompileError, Scala3.Toolchain]): Option[Seen] =
    result match
      case Right(tool) =>
        Some(Seen(tool.runtime, tool.compilerClasspath, tool.loaderJars, tool.bridgeJar, tool.boundary))
      case Left(_) => None

  private def artifact(path: Path): ResolvedArtifact =
    ResolvedArtifact("org.scala-lang", "tool", scala3.value, path, Sha256.trusted("aa"), Repository.Central)

  private def properties(version: String): (String, String) =
    "library.properties" -> s"version.number=$version\n"

  private def clazz(name: String): (String, String) = name -> ""

  private def zip(dir: Path, name: String, entries: List[(String, String)]): Path =
    val path = dir.resolve(name)
    val out  = new ZipOutputStream(Files.newOutputStream(path))
    try
      entries.foreach: (entry, text) =>
        out.putNextEntry(new ZipEntry(entry))
        out.write(text.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
    finally out.close()
    path
end Scala3LawSpec
