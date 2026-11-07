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
}
import histos.workspace.Workspace
import histos.{ScalaVersion, ToolCatalog}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import slayer.{any, stub, stubbed}

import zio.{Ref, Scope, ZIO, ZLayer}
import zio.test.*

object Scala213LawSpec extends ZIOSpecDefault:
  private val scala213 = ScalaVersion("2.13.16")
  private val repos    = Repositories.central

  def spec = suite("scala 2.13 toolchain laws")(
    test("jars keep bridge ahead of the compiler, ignore a text file, and refuse a foreign zip or a fetch"):
      val request = CompilerRequests.compiler(scala213, repos)
      val program =
        for
          held    <- Ref.make[Either[ResolveError, Classpath]](Right(Classpath(Nil, Nil)))
          _       <- stub[ResolutionService](_.tool(any[ToolRequest]))(held.get.flatMap(ZIO.fromEither))
          service <- ZIO.service[ResolutionService]
          resolve <- ZIO.service[Resolve]
          root    <- Workspace.work("histos-scala213")
          tools    = LockedTools(DependencyLock(Nil), ToolCatalog.default, repos, resolve, Some(service))
          library  = zip(root, "library.jar", List(properties(scala213.value), clazz("org/jline/Inside.class")))
          compiler = zip(root, "compiler.jar", List(clazz("scala/tools/nsc/Global.class")))
          reflect  = zip(root, "reflect.jar", List(clazz("scala/reflect/runtime/JavaUniverse.class")))
          bridge   = zip(
            root,
            "bridge.jar",
            List(clazz("scala/tools/xsbt/CompilerBridge.class"), clazz("scala/tools/nsc/Global.class")),
          )
          diff      = zip(root, "diff.jar", List(clazz("com/github/difflib/DiffUtils.class")))
          jline     = zip(root, "jline.jar", List(clazz("org/jline/reader/LineReader.class")))
          jline2    = zip(root, "jline2.jar", List(clazz("org/jline/reader/Parser.class")))
          wrong     = zip(root, "wrong.jar", List(properties("2.13.15")))
          library2  = zip(root, "library2.jar", List(properties(scala213.value)))
          compiler2 = zip(root, "compiler2.jar", List(clazz("scala/tools/nsc/Global.class")))
          empty     = zip(root, "empty.jar", Nil)
          text <- ZIO.attemptBlocking:
            val path = root.resolve("notes.txt")
            Files.writeString(path, "not a jar")
            path
          refused    <- pull(held, tools, Left(ResolveError.NoTool(request.id)))
          ok         <- pull(held, tools, Right(List(library, compiler, reflect, bridge, diff, jline, text, jline2)))
          foreign    <- pull(held, tools, Right(List(library, compiler, reflect, bridge, diff, jline, text, empty)))
          unexpected <- pull(held, tools, Right(List(wrong, compiler, reflect, bridge)))
          duplicated <- pull(held, tools, Right(List(library, library2, compiler, reflect, bridge)))
          compilers  <- pull(held, tools, Right(List(library, compiler, compiler2, reflect, bridge)))
          missing    <- pull(held, tools, Right(List(library, compiler, bridge)))
          onlyText   <- pull(held, tools, Right(List(text)))
        yield assertTrue(
          refused == Left(CompileError.Resolved(ResolveError.NoTool(request.id))),
          view(ok) == Some(
            Seen(
              List(library),
              List(compiler, reflect, diff, jline, jline2),
              List(compiler, reflect, library, diff, jline, jline2),
              bridge,
              LoaderBoundary.Platform,
            )
          ),
          foreign == Left(CompileError.Toolchain(ToolchainMiss.Unexpected(scala213, List(empty)))),
          unexpected == Left(CompileError.Toolchain(ToolchainMiss.Unexpected(scala213, List(wrong)))),
          duplicated == Left(CompileError.Toolchain(ToolchainMiss.Duplicated(scala213, CompilerPart.Library))),
          compilers == Left(CompileError.Toolchain(ToolchainMiss.Duplicated(scala213, CompilerPart.Compiler))),
          missing == Left(CompileError.Toolchain(ToolchainMiss.Missing(scala213, CompilerPart.Reflect))),
          onlyText == Left(CompileError.Toolchain(ToolchainMiss.Missing(scala213, CompilerPart.Library))),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[ResolutionService] ++ stubbed[Resolve])
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
    held.set(classpath) *> Scala213.jars(scala213).provide(ZLayer.succeed(tools)).either

  private def view(result: Either[CompileError, Scala213.Toolchain]): Option[Seen] =
    result match
      case Right(tool) =>
        Some(Seen(tool.runtime, tool.compilerClasspath, tool.loaderJars, tool.bridgeJar, tool.boundary))
      case Left(_) => None

  private def artifact(path: Path): ResolvedArtifact =
    ResolvedArtifact("org.scala-lang", "tool", scala213.value, path, Sha256.trusted("aa"), Repository.Central)

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
end Scala213LawSpec
