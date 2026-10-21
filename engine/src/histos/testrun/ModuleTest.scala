package histos.testrun

import histos.jdk.Sha256
import histos.js.{JsLink, JsLinkError, JsSuite, ScalaJs}
import histos.native.{NativeCompile, NativeLink, NativeSuite, NativeToolchain}
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

private def platformToken(platform: Platform): String =
  platform match
    case Platform.Jvm    => "jvm"
    case Platform.Js     => "js"
    case Platform.Native => "native"

final case class TestedCell(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    status: TestStatus
)

/** What one cell did. `NoTests` means there was nothing to run. It is not a
  * passing suite.
  */
enum TestStatus:
  case NoTests
  case Unmatched
  case Passed(out: String, err: String)
  case Failed(exit: Int, out: String, err: String)

  def label: String = this match
    case NoTests            => "no tests"
    case Unmatched          => "unmatched"
    case Passed(_, _)       => "passed"
    case Failed(exit, _, _) => s"failed $exit"

/** What the test console is doing. Suites are not merged: one tree finishes,
  * then the next starts.
  */
enum TestFeed:
  case Begin(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Out(text: String)
  case Err(text: String)
  case Done(cell: TestedCell)
  case Cached(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String
  )

enum ModuleTestError:
  case Target(error: CiError)
  case Compile(error: GraphCompileError)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case Sources(module: ProjectName, version: ScalaVersion, error: CompileError)
  case MissingSources(module: ProjectName, main: TestMain)
  case MissingProvider(module: ProjectName, provider: TestProvider)
  case ProviderDependency(module: ProjectName, provider: TestProvider)
  case NoneDiscovered(module: ProjectName, provider: TestProvider)
  case NoneMatched(patterns: List[SuitePattern])
  case Mixed(module: ProjectName)
  case Process(module: ProjectName, version: ScalaVersion, error: TestError)
  case Main(error: MainClassError)
  case Absent(module: ProjectName)
  case MissingUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion
  )
  case Work(path: Path)
  case Root(path: Path)
  case Runner(path: Path)
  case LinkFailed(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      detail: String
  )

  def message: String = this match
    case Target(error)                   => error.message
    case Compile(error)                  => error.message
    case Lock(error)                     => error.message
    case LockRead(path)                  => s"could not read $path"
    case Sources(module, version, error) =>
      s"'${module.value}' ${version.value} failed to compile tests\n${error.message}"
    case MissingSources(module, main) =>
      s"'${module.value}' names ${main.value} and has no test sources"
    case MissingProvider(module, provider) =>
      s"'${module.value}' runs ${provider.framework.value} and has no test sources"
    case ProviderDependency(module, provider) =>
      val library = provider.library
      s"'${module.value}' runs ${provider.framework.value} and testDependencies do not include ${library.organization}::${library.name}"
    case NoneDiscovered(module, provider) =>
      s"'${module.value}' ran ${provider.framework.value} and it found no tests"
    case NoneMatched(patterns) =>
      s"no tests matched ${patterns.map(_.value).mkString(" ")}"
    case Mixed(module) =>
      s"'${module.value}' names a test main and a test provider"
    case Process(module, version, error) =>
      s"'${module.value}' ${version.value} did not start\n${error.message}"
    case Main(error)    => error.message
    case Absent(module) =>
      s"'${module.value}' was compiled and is not a module in the graph"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Work(path)   => s"could not prepare the test work directory $path"
    case Root(path)   => s"could not read test roots under $path"
    case Runner(path) => s"could not write the test provider runner at $path"
    case LinkFailed(module, version, platform, detail) =>
      s"'${module.value}' ${version.value} ${platformToken(platform)} failed to link\n$detail"

object ModuleTest:
  /** Compile each requested module, then run its test provider once per Scala
    * version and platform. `TestProvider.Zio` is the provider when the module
    * names neither a provider nor a main. Upstream modules are compiled and not
    * run. The engine does not load the provider.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      patterns: List[SuitePattern] = Nil,
      quick: Boolean = false,
      onTestCompile: (ProjectName, ScalaVersion, Platform) => UIO[Unit] =
        (_, _, _) => ZIO.unit
  ): ZIO[Resolve, ModuleTestError, List[TestedCell]] =
    console(repo, graph, names, jdk, patterns, quick, onTestCompile)
      .collect { case TestFeed.Done(cell) => cell }
      .runCollect
      .map(_.toList)

  /** Test compiles stay parallel. The console is `concatAll`: one suite's
    * lines, then the next, in module name order. Merging the trees would make
    * them unreadable.
    */
  def console(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      patterns: List[SuitePattern] = Nil,
      quick: Boolean = false,
      onTestCompile: (ProjectName, ScalaVersion, Platform) => UIO[Unit] =
        (_, _, _) => ZIO.unit
  ): ZStream[Resolve, ModuleTestError, TestFeed] =
    ZStream
      .fromZIO(
        GraphCompile
          .run(repo, graph, names, jdk)
          .mapError(ModuleTestError.Compile(_))
      )
      .flatMap: compiled =>
        suites(
          repo,
          graph,
          names,
          jdk,
          compiled,
          patterns,
          quick,
          onTestCompile
        )

  /** Test suites for cells that are already compiled. One suite's lines, then
    * the next, in module name order.
    */
  def suites(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      compiled: List[CompiledCell],
      patterns: List[SuitePattern] = Nil,
      quick: Boolean = false,
      onTestCompile: (ProjectName, ScalaVersion, Platform) => UIO[Unit] =
        (_, _, _) => ZIO.unit
  ): ZStream[Resolve, ModuleTestError, TestFeed] =
    val repositories = BuildResolvers(graph.resolvers)
    ZStream.unwrap {
      for
        wanted <- ZIO.fromEither(
          requested(graph, names).left.map(ModuleTestError.Target(_))
        )
        lock <- readLock(repo)
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
        testSources(repo, module, modules, cell.version, cell.platform).flatMap:
          sources =>
            val chosen = (sources, module.test, module.provider) match
              case (_, Some(_), Some(_)) =>
                ZIO.fail(ModuleTestError.Mixed(cell.module))
              case (Nil, None, None) =>
                if patterns.isEmpty then
                  ZIO.succeed(finished(cell, TestStatus.NoTests))
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
                        execute(
                          repo,
                          modules,
                          module,
                          cell,
                          sources,
                          main,
                          jdk,
                          lock,
                          repositories,
                          quick
                        )
                      case Platform.Js =>
                        runJs(
                          repo,
                          modules,
                          module,
                          cell,
                          sources,
                          main,
                          jdk,
                          lock,
                          repositories,
                          quick
                        )
                      case Platform.Native =>
                        runNative(
                          repo,
                          modules,
                          module,
                          cell,
                          sources,
                          main,
                          jdk,
                          lock,
                          repositories,
                          quick
                        ))
              case (_, None, provider) =>
                val framework = provider.getOrElse(TestProvider.Zio)
                onTestCompile(cell.module, cell.version, cell.platform) *>
                  (cell.platform match
                    case Platform.Jvm =>
                      provide(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        framework,
                        jdk,
                        lock,
                        repositories,
                        patterns,
                        quick
                      )
                    case Platform.Js =>
                      runJsProvider(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        framework,
                        jdk,
                        lock,
                        repositories,
                        patterns,
                        quick
                      )
                    case Platform.Native =>
                      runNativeProvider(
                        repo,
                        modules,
                        module,
                        cell,
                        sources,
                        framework,
                        jdk,
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
              TestStatus.Passed(_, _) | TestStatus.Failed(_, _, _)
            )
          ) =>
        matched.set(true)
      case _ =>
        ZIO.unit

  private def runJs(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      quick: Boolean
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val linkDir = linkWork(repo, module.name, cell.version, cell.platform)
    for
      binary <- ZIO.fromEither(
        MainClass.from(main.value).left.map(ModuleTestError.Main(_))
      )
      linked <- compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        lock,
        repositories
      )
      classes = testClasses(repo, module.name, cell.version, cell.platform)
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        linked.hashes,
        quick,
        _ =>
          for
            _ <- fresh(linkDir)
            script <- JsLink
              .link(cell.version, binary, classes :: linked.paths, linkDir, jdk)
              .mapError(error => linkFailed(cell, error.message))
            work <- fresh(
              testWork(repo, module.name, cell.version, cell.platform)
            )
          yield runCommand(
            module.name,
            cell.version,
            cell.platform,
            List("node", script.toString),
            work,
            suiteEnv(repo)
          )
      )
    yield stream

  private def runJsProvider(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      quick: Boolean
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val linkDir = linkWork(repo, module.name, cell.version, cell.platform)
    val classes = testClasses(repo, module.name, cell.version, cell.platform)
    def launch(
        selected: List[SuitePattern],
        linked: Linked
    ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
      for
        keep <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            for
              _ <- fresh(linkDir)
              asked <- JsSuite
                .run(
                  cell.version,
                  module.jsEnv,
                  module.jsModule,
                  provider.framework.value,
                  classes :: linked.paths,
                  linkDir,
                  jdk,
                  suiteEnv(repo),
                  selected
                )
                .mapError(error => linkFailed(cell, error.message))
            yield session(
              module.name,
              cell.version,
              Some(provider),
              asked,
              cell.platform
            )
      yield stream
    for
      _ <- ZIO.unless(satisfied(module, provider))(
        ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider))
      )
      linked <- compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        lock,
        repositories
      )
      stream <-
        if !quick then launch(patterns, linked)
        else
          SuiteNames
            .discover(classes, linked.paths, provider.framework.value)
            .mapError(_ => ModuleTestError.Root(classes))
            .flatMap:
              case SuiteNames.Found.Unseen        => launch(patterns, linked)
              case SuiteNames.Found.Suites(names) =>
                val wanted =
                  names.filter(name => SuitePattern.allows(patterns, name))
                if patterns.nonEmpty && wanted.isEmpty then
                  ZIO.succeed(ZStream.empty)
                else if wanted.isEmpty then launch(patterns, linked)
                else
                  remember(
                    repo,
                    cell,
                    wanted,
                    classes :: linked.paths,
                    linked.hashes,
                    true,
                    dirty => launch(exact(dirty), linked)
                  )
    yield stream

  private def runNative(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      quick: Boolean
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val linkDir = linkWork(repo, module.name, cell.version, cell.platform)
    for
      major <- nativeMajor(module)
      binary <- ZIO.fromEither(
        MainClass.from(main.value).left.map(ModuleTestError.Main(_))
      )
      linked <- compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        lock,
        repositories
      )
      classes = testClasses(repo, module.name, cell.version, cell.platform)
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        linked.hashes,
        quick,
        _ =>
          for
            _ <- fresh(linkDir)
            program <- NativeLink
              .linkClasspath(
                major,
                module.nativeVersion.getOrElse(NativeToolchain.ReleaseRevision),
                suiteEnv(repo),
                classes :: linked.paths,
                linkDir,
                jdk,
                binary
              )
              .mapError(error => linkFailed(cell, error.message))
            work <- fresh(
              testWork(repo, module.name, cell.version, cell.platform)
            )
          yield runCommand(
            module.name,
            cell.version,
            cell.platform,
            List(program.toString),
            work,
            suiteEnv(repo)
          )
      )
    yield stream

  private def runNativeProvider(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      quick: Boolean
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val linkDir = linkWork(repo, module.name, cell.version, cell.platform)
    val classes = testClasses(repo, module.name, cell.version, cell.platform)
    val revision =
      module.nativeVersion.getOrElse(NativeToolchain.ReleaseRevision)
    val entry = MainClass.trusted("scala.scalanative.testinterface.TestMain")
    def launch(
        selected: List[SuitePattern],
        linked: Linked,
        major: histos.ClangMajor
    ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
      for
        keep <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            for
              _ <- fresh(linkDir)
              classpath = classes :: linked.paths
              binary <- NativeLink
                .linkClasspath(
                  major,
                  revision,
                  suiteEnv(repo),
                  classpath,
                  linkDir,
                  jdk,
                  entry
                )
                .mapError(error => linkFailed(cell, error.message))
              asked <- NativeSuite
                .run(
                  revision,
                  provider.framework.value,
                  binary,
                  classpath,
                  linkDir,
                  jdk,
                  suiteEnv(repo),
                  selected
                )
                .mapError(error => linkFailed(cell, error.message))
            yield session(
              module.name,
              cell.version,
              Some(provider),
              asked,
              cell.platform
            )
      yield stream
    for
      _ <- ZIO.unless(satisfied(module, provider))(
        ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider))
      )
      major <- nativeMajor(module)
      linked <- compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        lock,
        repositories
      )
      stream <-
        if !quick then launch(patterns, linked, major)
        else
          SuiteNames
            .discover(classes, linked.paths, provider.framework.value)
            .mapError(_ => ModuleTestError.Root(classes))
            .flatMap:
              case SuiteNames.Found.Unseen => launch(patterns, linked, major)
              case SuiteNames.Found.Suites(names) =>
                val wanted =
                  names.filter(name => SuitePattern.allows(patterns, name))
                if patterns.nonEmpty && wanted.isEmpty then
                  ZIO.succeed(ZStream.empty)
                else if wanted.isEmpty then launch(patterns, linked, major)
                else
                  remember(
                    repo,
                    cell,
                    wanted,
                    classes :: linked.paths,
                    linked.hashes,
                    true,
                    dirty => launch(exact(dirty), linked, major)
                  )
    yield stream

  private def nativeMajor(
      module: Module
  ): IO[ModuleTestError, histos.ClangMajor] =
    module.clang match
      case ClangPin.Major(major) => ZIO.succeed(major)
      case ClangPin.Unused       =>
        ZIO.fail(
          ModuleTestError.LinkFailed(
            module.name,
            module.versions.first,
            Platform.Native,
            "the module does not name a clang major"
          )
        )

  private def linkFailed(cell: CompiledCell, detail: String): ModuleTestError =
    ModuleTestError.LinkFailed(cell.module, cell.version, cell.platform, detail)

  private def runCommand(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      argv: List[String],
      work: Path,
      env: Map[String, String]
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.succeed(TestFeed.Begin(module, version, platform)) ++
      TestProcess
        .command(argv, work, env)
        .mapError(error => ModuleTestError.Process(module, version, error))
        .mapAccumZIO(Transcript("", "")) { (acc, event) =>
          hear(module, version, platform, None, acc, event)
        }

  private def finished(
      cell: CompiledCell,
      status: TestStatus
  ): ZStream[Any, Nothing, TestFeed] =
    ZStream.succeed(
      TestFeed.Done(
        TestedCell(cell.module, cell.version, cell.platform, status)
      )
    )

  /** When `quick` is set, a suite with a stored success for this digest does
    * not start. `run` receives the suites that still have to run.
    */
  private def remember(
      repo: Path,
      cell: CompiledCell,
      suites: List[String],
      directories: List[Path],
      hashes: List[Sha256],
      quick: Boolean,
      run: List[String] => ZIO[
        Resolve,
        ModuleTestError,
        ZStream[Any, ModuleTestError, TestFeed]
      ]
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    if !quick then run(suites)
    else
      val jars = hashes.map(_.value)
      ZIO
        .foldLeft(suites)(
          (List.empty[String], List.empty[(String, Option[String])])
        ):
          case ((cached, dirty), name) =>
            SuiteStamp.digest(name, directories, jars) match
              case Some(digest) =>
                QuickRecord
                  .hit(
                    repo,
                    cell.module,
                    cell.version,
                    cell.platform,
                    name,
                    digest
                  )
                  .map { hit =>
                    if hit then (name :: cached, dirty)
                    else (cached, (name, Some(digest)) :: dirty)
                  }
              case None =>
                ZIO.succeed((cached, (name, None) :: dirty))
        .flatMap { (cached, dirty) =>
          val shown = ZStream.fromIterable(
            cached.sorted.map(name =>
              TestFeed.Cached(cell.module, cell.version, cell.platform, name)
            )
          )
          if dirty.isEmpty then ZIO.succeed(shown)
          else
            run(dirty.map(_._1)).map { stream =>
              shown ++ stream.tap {
                case TestFeed.Done(done) =>
                  done.status match
                    case TestStatus.Passed(_, _) =>
                      ZIO.foreachDiscard(dirty):
                        case (name, Some(digest)) =>
                          QuickRecord.save(
                            repo,
                            cell.module,
                            cell.version,
                            cell.platform,
                            name,
                            digest
                          )
                        case _ => ZIO.unit
                    case TestStatus.Failed(_, _, _) =>
                      ZIO.foreachDiscard(dirty)((name, _) =>
                        QuickRecord.clear(
                          repo,
                          cell.module,
                          cell.version,
                          cell.platform,
                          name
                        )
                      )
                    case _ => ZIO.unit
                case _ => ZIO.unit
              }
            }
        }

  private def exact(names: List[String]): List[SuitePattern] =
    names.flatMap(name => SuitePattern.from(name).toOption)

  private def execute(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      quick: Boolean
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    for
      binary <- ZIO.fromEither(
        MainClass.from(main.value).left.map(ModuleTestError.Main(_))
      )
      linked <- compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        lock,
        repositories
      )
      classes = testClasses(repo, module.name, cell.version, cell.platform)
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        linked.hashes,
        quick,
        _ =>
          fresh(testWork(repo, module.name, cell.version, cell.platform)).map {
            work =>
              session(
                module.name,
                cell.version,
                None,
                request(repo, jdk, classes, linked.paths, binary, work),
                cell.platform
              )
          }
      )
    yield stream

  private def provide(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      quick: Boolean
  ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    val classes = testClasses(repo, module.name, cell.version, cell.platform)
    val runner = providerSource(repo, module.name, cell.version, cell.platform)
    def launch(
        selected: List[SuitePattern]
    ): ZIO[Resolve, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
      for
        _ <- writeRunner(
          runner,
          ProviderRunner.source(provider.framework.value, classes, selected)
        )
        linked <- compiledTests(
          repo,
          modules,
          module,
          cell,
          sources :+ runner,
          jdk,
          lock,
          repositories
        )
        keep <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            fresh(testWork(repo, module.name, cell.version, cell.platform)).map:
              work =>
                session(
                  module.name,
                  cell.version,
                  Some(provider),
                  request(
                    repo,
                    jdk,
                    classes,
                    linked.paths,
                    ProviderRunner.Main,
                    work
                  ),
                  cell.platform
                )
      yield stream
    for
      _ <- ZIO.unless(satisfied(module, provider))(
        ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider))
      )
      stream <-
        if !quick then launch(patterns)
        else
          for
            linked <- compiledTests(
              repo,
              modules,
              module,
              cell,
              sources,
              jdk,
              lock,
              repositories
            )
            found <- SuiteNames
              .discover(classes, linked.paths, provider.framework.value)
              .mapError(_ => ModuleTestError.Root(classes))
            ran <- found match
              case SuiteNames.Found.Unseen        => launch(patterns)
              case SuiteNames.Found.Suites(names) =>
                val wanted =
                  names.filter(name => SuitePattern.allows(patterns, name))
                if patterns.nonEmpty && wanted.isEmpty then
                  ZIO.succeed(ZStream.empty)
                else if wanted.isEmpty then launch(patterns)
                else
                  remember(
                    repo,
                    cell,
                    wanted,
                    classes :: linked.paths,
                    linked.hashes,
                    true,
                    dirty => launch(exact(dirty))
                  )
          yield ran
    yield stream

  private def request(
      repo: Path,
      jdk: ResolvedJdk,
      classes: Path,
      classpath: List[Path],
      main: MainClass,
      work: Path
  ): TestRequest =
    TestRequest(
      Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      TestClasspath(classes, classpath),
      main,
      work,
      suiteEnv(repo)
    )

  private final case class Transcript(out: String, err: String)

  /** Header, then lines as the process writes them, then the cell. `NoneFound`
    * fails the stream.
    */
  private def session(
      module: ProjectName,
      version: ScalaVersion,
      provider: Option[TestProvider],
      asked: TestRequest,
      shown: Platform
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.succeed(TestFeed.Begin(module, version, shown)) ++
      TestProcess
        .events(asked)
        .mapError(error => ModuleTestError.Process(module, version, error))
        .mapAccumZIO(Transcript("", "")) { (acc, event) =>
          hear(module, version, shown, provider, acc, event)
        }

  private def hear(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      provider: Option[TestProvider],
      acc: Transcript,
      event: TestEvent
  ): ZIO[Any, ModuleTestError, (Transcript, TestFeed)] =
    event match
      case TestEvent.Out(text) =>
        val next = acc.copy(out = join(acc.out, text))
        ZIO.succeed((next, TestFeed.Out(text)))
      case TestEvent.Err(text) =>
        val next = acc.copy(err = join(acc.err, text))
        ZIO.succeed((next, TestFeed.Err(text)))
      case TestEvent.Exited(code) =>
        provider match
          case Some(_) if code.value == ProviderRunner.NoneMatched =>
            ZIO.succeed(
              (
                acc,
                TestFeed.Done(
                  TestedCell(module, version, platform, TestStatus.Unmatched)
                )
              )
            )
          case Some(found) if code.value == ProviderRunner.NoneFound =>
            ZIO.fail(ModuleTestError.NoneDiscovered(module, found))
          case _ =>
            val status =
              if code.value == 0 then TestStatus.Passed(acc.out, acc.err)
              else TestStatus.Failed(code.value, acc.out, acc.err)
            ZIO.succeed(
              (
                acc,
                TestFeed.Done(TestedCell(module, version, platform, status))
              )
            )

  private def join(have: String, line: String): String =
    s"$have$line\n"

  private def satisfied(module: Module, provider: TestProvider): Boolean =
    val library = provider.library
    library.organization.nonEmpty && library.name.nonEmpty &&
    module.testDependencies.exists(dep =>
      dep.organization == library.organization && dep.name == library.name
    )

  /** JVM class files. No class files means the platform's child still filters.
    */
  private def narrowed(
      root: Path,
      patterns: List[SuitePattern]
  ): IO[ModuleTestError, Boolean] =
    if patterns.isEmpty then ZIO.succeed(true)
    else
      ZIO
        .attemptBlocking(classBinaries(root))
        .mapError(_ => ModuleTestError.Root(root))
        .map: names =>
          names.isEmpty || names.exists(name =>
            SuitePattern.allows(patterns, name)
          )

  private def classBinaries(root: Path): List[String] =
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.flatMap(path => classBinary(root, path)).toList
      finally walk.close()

  private def classBinary(root: Path, path: Path): Option[String] =
    if !Files.isRegularFile(path) then None
    else
      val name = root.relativize(path).toString.replace('\\', '/')
      if !name.endsWith(".class") then None
      else
        val binary = name.stripSuffix(".class").replace('/', '.')
        if binary.isEmpty || binary.contains("$$") then None else Some(binary)

  private def providerSource(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("provider-src")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))
      .resolve("subprocess")
      .resolve("Provider.scala")

  private def writeRunner(
      path: Path,
      source: String
  ): IO[ModuleTestError, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent)
          .foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, source)
        ()
      }
      .mapError(_ => ModuleTestError.Runner(path))

  /** The framework a test target launches, or a refusal when that library is
    * not a test dependency. A named test main opts out of the framework.
    */
  private[histos] def launchProvider(
      module: Module
  ): Either[ModuleTestError, Option[TestProvider]] =
    module.test match
      case Some(_) => Right(None)
      case None    =>
        val provider = module.provider.getOrElse(TestProvider.Zio)
        if satisfied(module, provider) then Right(Some(provider))
        else Left(ModuleTestError.ProviderDependency(module.name, provider))

  /** The work directory stays empty. `HISTOS_REPO` finds project files. `PATH` finds node and clang. `HOME` finds the Playwright cache. */
  private[histos] def suiteEnv(repo: Path): Map[String, String] =
    val root = Map("HISTOS_REPO" -> repo.toAbsolutePath.normalize.toString)
    val kept = List("PATH", "HOME", "XDG_CACHE_HOME").flatMap: key =>
      sys.env.get(key).filter(_.nonEmpty).map(key -> _)
    root ++ kept

  /** `src/test` inside the source root, plus `<module>/test`. The repository
    * `test` is only for a graph with one module.
    */
  private[histos] def testSources(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform
  ): IO[ModuleTestError, List[Path]] =
    for
      base <- GraphCompile
        .sourceBase(repo, module, modules)
        .mapError(ModuleTestError.Compile(_))
      inside <- base match
        case Some(root) =>
          Sources
            .tests(root, version, platform)
            .mapError(error =>
              ModuleTestError.Sources(module.name, version, error)
            )
        case None => ZIO.succeed(Nil)
      sibling <- testBase(repo, module, modules)
      outside <- sibling match
        case Some(root) =>
          Sources
            .select(root, version, platform)
            .mapError(error =>
              ModuleTestError.Sources(module.name, version, error)
            )
        case None => ZIO.succeed(Nil)
    yield distinct(inside ++ outside)

  private def distinct(paths: List[Path]): List[Path] =
    paths
      .foldLeft((List.empty[Path], Set.empty[Path])):
        case ((kept, seen), path) if seen.contains(path) => (kept, seen)
        case ((kept, seen), path) => (kept :+ path, seen + path)
      ._1

  /** `<module>/test` wins. The repository `test` is only for a graph with one
    * module.
    */
  private def testBase(
      repo: Path,
      module: Module,
      modules: List[Module]
  ): IO[ModuleTestError, Option[Path]] =
    ZIO
      .attemptBlocking {
        val named = repo.resolve(module.baseDirectory).resolve("test")
        val shared = repo.resolve("test")
        if Files.isDirectory(named) then Some(named)
        else if modules.length == 1 && Files.isDirectory(shared) then
          Some(shared)
        else None
      }
      .mapError(_ => ModuleTestError.Root(repo))

  private def upstreamClasses(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform
  ): IO[ModuleTestError, List[Path]] =
    ZIO.foreach(Module.chain(module, modules, platform)): dependency =>
      val path =
        GraphCompile.classesDir(repo, dependency.name, version, platform)
      ZIO
        .attemptBlocking(Files.isDirectory(path))
        .mapError(_ => ModuleTestError.Root(path))
        .flatMap: found =>
          if found then ZIO.succeed(path)
          else
            ZIO.fail(
              ModuleTestError
                .MissingUpstream(module.name, dependency.name, version)
            )

  private final case class Fetched(paths: List[Path], hashes: List[Sha256])

  /** Compiler library first, then locked jars. The first jar on the classpath
    * defines `scala.*`.
    */
  private[histos] final case class Linked(
      paths: List[Path],
      hashes: List[Sha256]
  )

  private[histos] def runtimeClasspath(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, ModuleTestError, Linked] =
    for
      fetched <- classpath(
        module,
        cell.version,
        cell.platform,
        lock,
        repositories
      ).mapError(ModuleTestError.Lock(_))
      upstream <- upstreamClasses(
        repo,
        module,
        modules,
        cell.version,
        cell.platform
      )
      libraries <- dependencyJars(
        module,
        modules,
        cell.version,
        cell.platform,
        lock,
        repositories
      )
      library <- standardLibrary(cell)
      jvm = cell.platform == Platform.Jvm
      kept =
        if jvm then
          fetched.paths
            .zip(fetched.hashes)
            .filterNot((path, _) => JvmCompile.isStandardLibrary(path))
        else fetched.paths.zip(fetched.hashes)
      tail =
        if jvm then libraries.filterNot(JvmCompile.isStandardLibrary)
        else libraries
      jars = (kept.map(_._1) ++ tail).distinct
    yield Linked(cell.classes :: (upstream ++ library ++ jars), kept.map(_._2))

  /** The JVM suite needs `scala-library` on the child classpath. Js and Native
    * put their own library on the compiler classpath.
    */
  private def standardLibrary(
      cell: CompiledCell
  ): ZIO[Any, ModuleTestError, List[Path]] =
    cell.platform match
      case Platform.Jvm =>
        JvmCompile
          .runtime(cell.version)
          .mapError(error =>
            ModuleTestError.Sources(cell.module, cell.version, error)
          )
      case Platform.Js | Platform.Native => ZIO.succeed(Nil)

  private[histos] def compiledTests(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      jdk: ResolvedJdk,
      lock: DependencyLock,
      repositories: Repositories,
      hear: CompileProblem => Unit = _ => ()
  ): ZIO[Resolve, ModuleTestError, Linked] =
    val classes = testClasses(repo, module.name, cell.version, cell.platform)
    val analysis = testAnalysis(repo, module.name, cell.version, cell.platform)
    for
      linked <- runtimeClasspath(
        repo,
        modules,
        module,
        cell,
        lock,
        repositories
      )
      request = CompileRequest(
        CompileToolchain(cell.version, cell.platform, jdk, module.flags),
        sources,
        linked.paths,
        classes,
        analysis,
        linked.hashes
      )
      _ <- compileSuite(
        module,
        request,
        SemanticDb.argv(repo, cell.version),
        hear
      )
    yield linked

  private def compileSuite(
      module: Module,
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit
  ): IO[ModuleTestError, Unit] =
    val compiled = request.toolchain.platform match
      case Platform.Jvm    => JvmCompile.compile(request, extra, hear)
      case Platform.Js     => ScalaJs.compile(request, extra, hear)
      case Platform.Native =>
        val revision =
          module.nativeVersion.getOrElse(NativeToolchain.ReleaseRevision)
        NativeCompile.compile(request, revision, extra, hear)
    compiled
      .mapError(error =>
        ModuleTestError.Sources(module.name, request.toolchain.version, error)
      )
      .unit

  /** Compile jars of `dependsOn`, not their tests. The child runs those
    * classes.
    */
  private def dependencyJars(
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, ModuleTestError, List[Path]] =
    val found = Module.chain(module, modules, platform)
    ZIO
      .foreach(found)(
        compileArtifacts(_, version, platform, lock, repositories)
      )
      .map(_.flatten.distinct)

  private def compileArtifacts(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, ModuleTestError, List[Path]] =
    val cell = ResolveCell(
      module.name,
      Dep.select(module.dependencies, platform),
      version,
      platform,
      repositories,
      ClasspathScope.Compile
    )
    Resolve
      .materialize(cell, lock)
      .mapBoth(ModuleTestError.Lock(_), _.artifacts.map(_.path))

  private def classpath(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      repositories: Repositories
  ): ZIO[Resolve, ResolveError, Fetched] =
    val (dependencies, scope) =
      val compileDeps = Dep.select(module.dependencies, platform)
      val testDeps = Dep.select(module.testDependencies, platform)
      if testDeps.isEmpty then (compileDeps, ClasspathScope.Compile)
      else (compileDeps ++ testDeps, ClasspathScope.Test)
    val cell = ResolveCell(
      module.name,
      dependencies,
      version,
      platform,
      repositories,
      scope
    )
    Resolve
      .materialize(cell, lock)
      .map(fetched =>
        Fetched(fetched.artifacts.map(_.path), fetched.artifacts.map(_.sha256))
      )

  private def readLock(repo: Path): IO[ModuleTestError, DependencyLock] =
    val path = repo.resolve(".histos").resolve("dependencies.lock")
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Right(Files.readString(path))
        else Left(ModuleTestError.Lock(ResolveError.NoLock(path)))
      }
      .mapError(_ => ModuleTestError.LockRead(path))
      .flatMap(ZIO.fromEither)
      .flatMap: text =>
        ZIO.fromEither(
          DependencyLock
            .parse(text)
            .left
            .map(error => ModuleTestError.Lock(ResolveError.Lock(error)))
        )

  private def fresh(work: Path): IO[ModuleTestError, Path] =
    ZIO
      .attemptBlocking {
        if Files.exists(work) then
          val walk = Files.walk(work)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
        Files.createDirectories(work)
        work
      }
      .mapError(_ => ModuleTestError.Work(work))

  private[histos] def testClasses(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-classes")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))

  private def testAnalysis(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-analysis")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))

  private[histos] def testWork(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-work")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))

  private def linkWork(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): Path =
    repo
      .resolve(".histos")
      .resolve("link")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platformToken(platform))

  private def platformRank(platform: Platform): Int =
    platform match
      case Platform.Jvm    => 0
      case Platform.Js     => 1
      case Platform.Native => 2
