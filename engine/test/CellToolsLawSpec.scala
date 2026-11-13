package histos.zinc

import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.js.JsRequests
import histos.native.{NativeOrigin, NativeRequests, NativeToolchain}
import histos.resolve.{
  DependencyLock,
  LockedArtifact,
  LockedTool,
  LockedTools,
  Repositories,
  Repository,
  ResolvedArtifact,
  Resolve,
  ToolInput,
  ToolRequest,
}
import histos.toolchain.RuntimeIdentity
import histos.{Platform, ScalaVersion, ScalacFlags, ToolCatalog}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import slayer.{Stubbed, any, stub, stubbed}

import zio.*
import zio.test.*

/** Recall's key against the compile's key for the same cell. The lock pins every tool. A fetch returns each pinned
  * artifact at another path, so the path is not the identity.
  */
object CellToolsLawSpec extends ZIOSpecDefault:
  private val catalog  = ToolCatalog.default
  private val repos    = Repositories.central
  private val scala3   = ScalaVersion.trusted("3.10.0")
  private val older    = ScalaVersion.trusted("3.9.0")
  private val scala213 = ScalaVersion.trusted("2.13.18")
  private val jdk      = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab" * 32)),
  )

  def spec = suite("cell key laws")(
    test("recall's key and rows are the compile's for every series, platform, and set of key inputs"):
      for
        toolchain <- nativeToolchain
        tools     <- locked(toolchain)
        result    <- check(Gen.elements(cells(toolchain)*), genBase): (cell, inputs) =>
          val (version, platform, placed) = cell
          val base                        = inputs.request(version, platform)
          for
            recalled <- CellTools.recall(tools, placed, base, inputs.sources)
            compiled <- CellTools.compile(placed, base, inputs.sources).provideLayer(ZLayer.succeed(tools))
            (_, stored) = compiled
          yield assertTrue(
            recalled.map(_.key) == Some(stored.key),
            recalled.map(_.rows) == Some(stored.rows),
          )
      yield result
    ,
    test("each series and platform names its own tools in the key"):
      for
        toolchain <- nativeToolchain
        tools     <- locked(toolchain)
        jvm       <- stored(tools, scala3, Platform.Jvm, CellPlatform.Jvm)
        jvm213    <- stored(tools, scala213, Platform.Jvm, CellPlatform.Jvm)
        js        <- stored(tools, scala3, Platform.Js, CellPlatform.Js(catalog.js.revision))
        native    <- stored(tools, scala3, Platform.Native, CellPlatform.Native(toolchain))
        native213 <- stored(tools, scala213, Platform.Native, CellPlatform.Native(toolchain))
      yield
        val compiler3  = print(tools, CompilerRequests.compiler(scala3, repos))
        val compiler2  = print(tools, CompilerRequests.compiler(scala213, repos))
        val semanticdb = print(tools, CompilerRequests.semanticdb(scala213, catalog, repos))
        val jsLibs     = print(tools, JsRequests.libraries(scala3, catalog.js.revision, repos, catalog))
        val nativeLibs = print(tools, NativeRequests.libraries(scala3, toolchain, repos))
        val native2    = print(tools, NativeRequests.libraries(scala213, toolchain, repos))
        val plugin     = pluginPrint(tools, NativeRequests.libraries(scala3, toolchain, repos))
        val plugin2    = pluginPrint(tools, NativeRequests.libraries(scala213, toolchain, repos))
        assertTrue(
          row(jvm, CompileInput.Compiler) == lines(compiler3),
          row(jvm, CompileInput.Tools) == lines(),
          row(jvm213, CompileInput.Compiler) == lines(compiler2, semanticdb),
          row(jvm213, CompileInput.Tools) == lines(),
          row(js, CompileInput.Compiler) == lines(compiler3),
          row(js, CompileInput.Tools) == lines(jsLibs),
          row(native, CompileInput.Compiler) == lines(compiler3, plugin),
          row(native, CompileInput.Tools) == lines(nativeLibs),
          row(native213, CompileInput.Compiler) == lines(compiler2, semanticdb, plugin2),
          row(native213, CompileInput.Tools) == lines(native2),
        ),
  ).provideSomeLayer[TestEnvironment](stubbed[Resolve])

  /** The rows a cell's key reads. The tool inputs come from the lock. */
  private final case class Inputs(
      sources: Sha256,
      artifacts: List[Sha256],
      apis: List[Sha256],
      coverage: Option[String],
  ):
    def request(version: ScalaVersion, platform: Platform): CompileRequest =
      CompileRequest(
        CompileToolchain(version, platform, jdk, ScalacFlags.empty),
        List(Path.of("/repo/core/src/A.scala")),
        Nil,
        Path.of("/repo/.histos/classes/core"),
        Path.of("/repo/.histos/analysis/core"),
        artifacts,
        apis,
        coverage = coverage,
        checkout = Path.of("/repo"),
      )

  private val genSha: Gen[Any, Sha256] = Gen.alphaNumericStringBounded(0, 12).map(digest)

  private val genBase: Gen[Any, Inputs] =
    for
      sources   <- genSha
      artifacts <- Gen.listOfBounded(0, 3)(genSha)
      apis      <- Gen.listOfBounded(0, 3)(genSha)
      coverage  <- Gen.option(Gen.alphaNumericStringBounded(1, 8))
    yield Inputs(sources, artifacts, apis, coverage)

  /** Every series and platform a compile accepts. Scala.js has no Scala 2.13 compile. */
  private def cells(toolchain: NativeToolchain): List[(ScalaVersion, Platform, CellPlatform)] =
    List(
      (scala3, Platform.Jvm, CellPlatform.Jvm),
      (older, Platform.Jvm, CellPlatform.Jvm),
      (scala213, Platform.Jvm, CellPlatform.Jvm),
      (scala3, Platform.Js, CellPlatform.Js(catalog.js.revision)),
      (scala3, Platform.Native, CellPlatform.Native(toolchain)),
      (scala213, Platform.Native, CellPlatform.Native(toolchain)),
    )

  private val nativeToolchain: IO[histos.native.NativeToolchainError, NativeToolchain] =
    ZIO.fromEither(NativeToolchain.of(catalog.native.revision, NativeOrigin.Central))

  /** A lock with a row for every tool those cells ask for, and a fetch that answers each pinned artifact. */
  private def locked(toolchain: NativeToolchain): ZIO[Resolve & Stubbed[Resolve], Nothing, LockedTools] =
    val requests =
      List(scala3, older, scala213).map(CompilerRequests.compiler(_, repos)) ++
        List(
          CompilerRequests.semanticdb(scala213, catalog, repos),
          JsRequests.libraries(scala3, catalog.js.revision, repos, catalog),
          NativeRequests.libraries(scala3, toolchain, repos),
          NativeRequests.libraries(scala213, toolchain, repos),
        )
    for
      _ <- stub[Resolve](_.fetch(any[LockedArtifact]))((artifact: LockedArtifact) =>
        ZIO.succeed(
          ResolvedArtifact(
            artifact.organization,
            artifact.name,
            artifact.revision,
            Path.of("/fetched").resolve(s"${artifact.name}.jar"),
            artifact.sha256,
            artifact.repository,
            artifact.classifier,
          )
        )
      )
      resolve <- ZIO.service[Resolve]
    yield LockedTools(DependencyLock(Nil, tools = requests.map(pinned)), catalog, repos, resolve)

  private def pinned(request: ToolRequest): LockedTool =
    LockedTool(
      request.id,
      request.roots,
      Repositories.ordered(request.repositories),
      request.roots.map(root =>
        LockedArtifact(
          root.organization,
          root.name,
          root.revision,
          digest(s"${root.organization}:${root.name}:${root.revision}"),
          Repository.Central,
        )
      ),
      Nil,
    )

  /** The key a compile of an empty-input cell stores. */
  private def stored(
      tools: LockedTools,
      version: ScalaVersion,
      platform: Platform,
      placed: CellPlatform,
  ): IO[CompileError, KeyedRequest] =
    val sources = digest("sources")
    CellTools
      .compile(placed, Inputs(sources, Nil, Nil, None).request(version, platform), sources)
      .map((_, keyed) => keyed)
      .provideLayer(ZLayer.succeed(tools))

  private def print(tools: LockedTools, request: ToolRequest): Option[Sha256] =
    tools.pinned(request).map(_.fingerprint)

  /** The plugin's row of the libraries input, named by the artifact the lock pins for `nscplugin`. */
  private def pluginPrint(tools: LockedTools, request: ToolRequest): Option[Sha256] =
    tools
      .pinned(request)
      .map(input => ToolInput(input.id, input.artifacts.filter(_.name.startsWith("nscplugin_"))).fingerprint)

  private def row(keyed: KeyedRequest, input: CompileInput): Option[String] =
    keyed.rows.collectFirst { case CompileRow(`input`, value) => value }

  /** The text a row holds for these fingerprints. A missing pin makes the expected row absent. */
  private def lines(prints: Option[Sha256]*): Option[String] =
    prints.toList
      .foldLeft(Option(List.empty[Sha256]))((acc, next) => acc.flatMap(have => next.map(have :+ _)))
      .map(_.map(_.value).mkString("\n"))

  private def digest(text: String): Sha256 =
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))
end CellToolsLawSpec
