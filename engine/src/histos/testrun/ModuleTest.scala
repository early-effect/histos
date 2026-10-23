package histos.testrun

import histos.jdk.Sha256
import histos.js.{JsLink, JsLinkError, JsLinkRequest, JsSuite, JsSuiteRequest, ScalaJs}
import histos.native.{NativeCompile, NativeLink, NativeLinkRequest, NativeOrigin, NativeSuite, NativeSuiteRequest, NativeToolchain}
import histos.node.{NodeBinary, NodeMiss}
import histos.toolchain.Runtimes
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  Repositories,
  Resolve,
  ResolveCell,
  ResolveError
}
import histos.workflow.CiError
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileProblem,
  CompileRequest,
  CompileToolchain,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,
  ResolvedJdk,
  ScalacFlags,
  SemanticDb,
  Sources
}
import histos.pack.{MainClass, MainClassError}
import histos.{
  ClangPin,
  Dep,
  Module,
  Platform,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  TestMain,
  TestProvider
}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Ref, UIO, ZIO}
import zio.stream.ZStream

object ModuleTest:
  /** Compile each requested module, then run its test provider once per Scala
    * version and platform. `TestProvider.Zio` is the provider when the module
    * names neither a provider nor a main. Upstream modules are compiled and not
    * run. The engine does not load the provider.
    */
  def run(asked: TestRun): ZIO[Resolve, ModuleTestError, List[TestReport]] =
    console(asked)
      .runFold(Reports(Transcript.empty, Chunk.empty))(report)
      .map(_.done.toList)

  private final case class Reports(heard: Transcript, done: Chunk[TestReport])

  /** Cells do not interleave, so the lines since the last cell belong to the next `Done`. */
  private def report(have: Reports, feed: TestFeed): Reports =
    feed match
      case TestFeed.Begin(_, _, _)     => have.copy(heard = Transcript.empty)
      case TestFeed.Out(text)          => have.copy(heard = have.heard.withOut(text))
      case TestFeed.Err(text)          => have.copy(heard = have.heard.withErr(text))
      case TestFeed.Done(cell)         => Reports(Transcript.empty, have.done :+ TestReport(cell, have.heard))
      case TestFeed.Cached(_, _, _, _) => have

  /** Test compiles stay parallel. The console is `concatAll`: one suite's
    * lines, then the next, in module name order. Merging the trees would make
    * them unreadable.
    */
  def console(asked: TestRun): ZStream[Resolve, ModuleTestError, TestFeed] =
    ZStream
      .fromZIO(
        GraphCompile
          .run(asked.repo, asked.graph, asked.names, asked.jdk, asked.runtimes.native)
          .mapError(ModuleTestError.Compile(_))
      )
      .flatMap: compiled =>
        suites(asked, compiled)

  /** Test suites for cells that are already compiled. One suite's lines, then
    * the next, in module name order.
    */
  def suites(asked: TestRun, compiled: List[CompiledCell]): ZStream[Resolve, ModuleTestError, TestFeed] =
    val TestRun(repo, graph, names, jdk, runtimes, patterns, quick, onTestCompile) = asked
    val repositories = BuildResolvers(graph.resolvers)
    ZStream.unwrap {
      for
        wanted <- ZIO.fromEither(
          requested(graph, names).left.map(ModuleTestError.Target(_))
        )
        lock <- TestCompile.readLock(repo)
        matched <- Ref.make(false)
        cells = compiled
          .filter(cell => wanted.contains(cell.module))
          .sortBy: cell =>
            (cell.module.value, cell.version.value, platformRank(cell.platform))
        streams <- ZIO.foreachPar(cells)(cell =>
          prepare(
            repo,
            graph,
            cell,
            jdk,
            runtimes,
            lock,
            repositories,
            patterns,
            matched,
            quick,
            onTestCompile
          )
        )
      yield
        val ran = ZStream.concatAll(Chunk.fromIterable(streams))
        val gate =
          if patterns.isEmpty then ZStream.empty
          else
            ZStream.unwrap(matched.get.map { hit =>
              if hit then ZStream.empty
              else ZStream.fail(ModuleTestError.NoneMatched(patterns))
            })
        ran ++ gate
    }

  private def requested(
      graph: ProjectGraph,
      names: List[String]
  ): Either[CiError, Set[ProjectName]] =
    histos.select.Selection
      .select(graph, names)
      .map(selected => selected.modules.map(_.name).toSet)

  private def prepare(
      repo: Path,
      graph: ProjectGraph,
      cell: CompiledCell,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      matched: Ref[Boolean],
      quick: Boolean,
      onTestCompile: (ProjectName, ScalaVersion, Platform) => UIO[Unit]
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val modules = graph.projects.collect { case module: Module => module }
    graph.get(cell.module) match
      case Some(module: Module) =>
        TestCompile.testSources(repo, module, modules, cell.version, cell.platform).flatMap:
          sources =>
            val chosen = (sources, module.test, module.provider) match
              case (_, Some(_), Some(_)) =>
                ZIO.fail(ModuleTestError.Mixed(cell.module))
              case (Nil, None, None) =>
                if patterns.isEmpty then
                  ZIO.succeed(SuiteLaunch.finished(cell, TestStatus.NoTests))
                else ZIO.succeed(ZStream.empty)
              case (Nil, Some(main), None) =>
                ZIO.fail(ModuleTestError.MissingSources(cell.module, main))
              case (Nil, None, Some(provider)) =>
                ZIO.fail(ModuleTestError.MissingProvider(cell.module, provider))
              case (_, Some(main), None) =>
                if !SuitePattern.allows(patterns, main.value) then
                  ZIO.succeed(ZStream.empty)
                else
                  onTestCompile(cell.module, cell.version, cell.platform) *>
                    (cell.platform match
                      case Platform.Jvm =>
                        SuiteLaunch.execute(
                          repo,
                          modules,
                          module,
                          cell,
                          sources,
                          main,
                          jdk,
                          runtimes,
                          lock,
                          repositories,
                          quick
                        )
                      case Platform.Js =>
                        SuiteLaunch.runJs(
                          repo,
                          modules,
                          module,
                          cell,
                          sources,
                          main,
                          jdk,
                          runtimes,
                          lock,
                          repositories,
                          quick
                        )
                      case Platform.Native =>
                        SuiteLaunch.runNative(
                          repo,
                          modules,
                          module,
                          cell,
                          sources,
                          main,
                          jdk,
                          runtimes,
                          lock,
                          repositories,
                          quick
                        ))
              case (_, None, provider) =>
                val framework = provider.getOrElse(TestProvider.Zio)
                onTestCompile(cell.module, cell.version, cell.platform) *>
                  (cell.platform match
                    case Platform.Jvm =>
                      SuiteLaunch.provide(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        framework,
                        jdk,
                        runtimes,
                        lock,
                        repositories,
                        patterns,
                        quick
                      )
                    case Platform.Js =>
                      SuiteLaunch.runJsProvider(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        framework,
                        jdk,
                        runtimes,
                        lock,
                        repositories,
                        patterns,
                        quick
                      )
                    case Platform.Native =>
                      SuiteLaunch.runNativeProvider(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        framework,
                        jdk,
                        runtimes,
                        lock,
                        repositories,
                        patterns,
                        quick
                      ))
            chosen.map(watch(matched, _))
      case _ =>
        ZIO.fail(ModuleTestError.Absent(cell.module))

  /** A passed or failed suite is a match. `unmatched` and `no tests` are not.
    */
  private def watch(
      matched: Ref[Boolean],
      stream: ZStream[Any, ModuleTestError, TestFeed]
  ): ZStream[Any, ModuleTestError, TestFeed] =
    stream.tap:
      case TestFeed.Cached(_, _, _, _) =>
        matched.set(true)
      case TestFeed.Done(
            TestedCell(
              _,
              _,
              _,
              TestStatus.Passed | TestStatus.Failed(_)
            )
          ) =>
        matched.set(true)
      case _ =>
        ZIO.unit

  private def platformRank(platform: Platform): Int =
    platform match
      case Platform.Jvm    => 0
      case Platform.Js     => 1
      case Platform.Native => 2
