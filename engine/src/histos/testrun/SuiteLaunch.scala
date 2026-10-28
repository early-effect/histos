package histos.testrun

import histos.jdk.{JdkRuntime, Sha256}
import histos.js.{JsRun, JsRunRequest}

import histos.resolve.LockedTools
import histos.js.{JsLink, JsLinkError, JsLinkRequest, JsSuite, JsSuiteRequest, ScalaJs}
import histos.native.{
  NativeCompile,
  NativeLink,
  NativeLinkRequest,
  NativeOrigin,
  NativeSuite,
  NativeSuiteRequest,
  NativeToolchain,
}
import histos.node.{NodeBinary, NodeMiss, NodeToolchain}
import histos.toolchain.Runtimes
import histos.resolve.{DependencyLock, Repositories, Resolve}
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

  ScalacFlags,
  SemanticDb,
  Sources,
}
import histos.pack.{MainClass, MainClassError}
import histos.{ClangPin, Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestMain, TestProvider}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Ref, UIO, ZIO}
import zio.stream.ZStream

private[testrun] object SuiteLaunch:
  private[testrun] def runJs(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      mode: TestMode,
      noted: histos.zinc.CompileFacts => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & TestContext,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    for
      binary <- ZIO.fromEither(
        MainClass.from(main.value).left.map(ModuleTestError.Main(_))
      )
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
        lock,
        repositories,
        onCompiled = noted,
      )
      classes = TestCompile.testClasses(
        repo,
        module.name,
        cell.version,
        cell.platform,
      )
      asked  <- prepared(module, modules, cell, jdk, runtimes, lock, repositories)
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        asked,
        mode,
        _ =>
          for
            chosen <- node(cell, runtimes)
            _      <- TestCompile.fresh(linkDir)
            linked <- JsLink
              .link(
                JsLinkRequest(
                  cell.version,
                  binary,
                  classes :: linked.paths,
                  linkDir,
                  jdk,
                  kind = module.jsModule,
                  revision = module.jsVersion,
                )
              )
              .mapError(error =>
                ModuleTestError.Tool(
                  cell.module,
                  cell.version,
                  histos.toolchain.PlatformToolError.JsLink(error),
                )
              )
            asked <- JsRun
              .start(
                JsRunRequest(
                  module.jsEnv,
                  module.jsModule,
                  linked.script,
                  linkDir,
                  jdk,
                  TestCompile.suiteEnv(repo, runtimes.env),
                  chosen,
                )
              )
              .mapError(error =>
                ModuleTestError.Tool(
                  cell.module,
                  cell.version,
                  histos.toolchain.PlatformToolError.JsRun(error),
                )
              )
          yield session(module.name, cell.version, None, asked, cell.platform),
      )
    yield stream

  private[testrun] def runJsProvider(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      mode: TestMode,
      noted: histos.zinc.CompileFacts => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & TestContext,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    val classes =
      TestCompile.testClasses(repo, module.name, cell.version, cell.platform)
    def launch(
        selected: List[SuitePattern],
        linked: TestCompile.Linked,
    ): ZIO[
      Resolve & ProviderInstall & LockedTools & TestContext,
      ModuleTestError,
      ZStream[Any, ModuleTestError, TestFeed],
    ] =
      for
        keep   <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            for
              chosen <- node(cell, runtimes)
              _      <- TestCompile.fresh(linkDir)
              asked  <- JsSuite
                .run(
                  JsSuiteRequest(
                    cell.version,
                    module.jsEnv,
                    module.jsModule,
                    provider.framework.value,
                    classes :: linked.paths,
                    linkDir,
                    jdk,
                    TestCompile.suiteEnv(repo, runtimes.env),
                    chosen,
                    selected,
                    revision = module.jsVersion,
                  )
                )
                .mapError(error =>
                  ModuleTestError.Tool(
                    cell.module,
                    cell.version,
                    histos.toolchain.PlatformToolError.JsSuite(error),
                  )
                )
            yield session(
              module.name,
              cell.version,
              Some(provider),
              asked,
              cell.platform,
            )
      yield stream
    for
      _ <- ZIO.unless(satisfied(module, provider))(
        ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider))
      )
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
        lock,
        repositories,
        onCompiled = noted,
      )
      stamp  <- prepared(module, modules, cell, jdk, runtimes, lock, repositories)
      stream <-
        SuiteNames
          .discover(classes, linked.paths, provider.framework.value)
          .mapError(_ => ModuleTestError.Root(classes))
          .flatMap:
            case SuiteNames.Found.Unseen        => launch(patterns, linked)
            case SuiteNames.Found.Suites(names) =>
              val wanted =
                names.filter(name => SuitePattern.allows(patterns, name))
              if patterns.nonEmpty && wanted.isEmpty then ZIO.succeed(ZStream.empty)
              else if wanted.isEmpty then launch(patterns, linked)
              else
                remember(
                  repo,
                  cell,
                  wanted,
                  classes :: linked.paths,
                  stamp,
                  mode,
                  dirty => launch(exact(dirty), linked),
                )
    yield stream

  private[testrun] def runNative(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      mode: TestMode,
      noted: histos.zinc.CompileFacts => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & TestContext,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    for
      major  <- nativeMajor(module)
      binary <- ZIO.fromEither(
        MainClass.from(main.value).left.map(ModuleTestError.Main(_))
      )
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
        lock,
        repositories,
        onCompiled = noted,
      )
      classes = TestCompile.testClasses(
        repo,
        module.name,
        cell.version,
        cell.platform,
      )
      asked  <- prepared(module, modules, cell, jdk, runtimes, lock, repositories)
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        asked,
        mode,
        _ =>
          for
            _         <- TestCompile.fresh(linkDir)
            toolchain <- nativeToolchain(module, cell, runtimes.native)
            program   <- NativeLink
              .linkClasspath(
                NativeLinkRequest(
                  major,
                  toolchain,
                  TestCompile.suiteEnv(repo, runtimes.env),
                  linkDir,
                  jdk,
                  binary,
                  histos.ScalaVersion("3.9.0"),
                ),
                classes :: linked.paths,
              )
              .mapError(error =>
                ModuleTestError.Tool(
                  cell.module,
                  cell.version,
                  histos.toolchain.PlatformToolError.Native(error),
                )
              )
            work <- TestCompile.fresh(
              TestCompile.testWork(
                repo,
                module.name,
                cell.version,
                cell.platform,
              )
            )
          yield runCommand(
            module.name,
            cell.version,
            cell.platform,
            List(program.toString),
            work,
            TestCompile.suiteEnv(repo, runtimes.env),
          ),
      )
    yield stream

  private[testrun] def runNativeProvider(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      mode: TestMode,
      noted: histos.zinc.CompileFacts => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & TestContext,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    val classes =
      TestCompile.testClasses(repo, module.name, cell.version, cell.platform)
    val entry = MainClass.trusted("scala.scalanative.testinterface.TestMain")
    def launch(
        selected: List[SuitePattern],
        linked: TestCompile.Linked,
        major: histos.ClangMajor,
    ): ZIO[
      Resolve & ProviderInstall & LockedTools & TestContext,
      ModuleTestError,
      ZStream[Any, ModuleTestError, TestFeed],
    ] =
      for
        keep   <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            for
              _ <- TestCompile.fresh(linkDir)
              classpath = classes :: linked.paths
              toolchain <- nativeToolchain(module, cell, runtimes.native)
              binary    <- NativeLink
                .linkClasspath(
                  NativeLinkRequest(
                    major,
                    toolchain,
                    TestCompile.suiteEnv(repo, runtimes.env),
                    linkDir,
                    jdk,
                    entry,
                    histos.ScalaVersion("3.9.0"),
                  ),
                  classpath,
                )
                .mapError(error =>
                  ModuleTestError.Tool(
                    cell.module,
                    cell.version,
                    histos.toolchain.PlatformToolError.Native(error),
                  )
                )
              asked <- NativeSuite
                .run(
                  NativeSuiteRequest(
                    toolchain,
                    provider.framework.value,
                    binary,
                    classpath,
                    linkDir,
                    jdk,
                    TestCompile.suiteEnv(repo, runtimes.env),
                    selected,
                  )
                )
                .mapError(error =>
                  ModuleTestError.Tool(
                    cell.module,
                    cell.version,
                    histos.toolchain.PlatformToolError.NativeSuite(error),
                  )
                )
            yield session(
              module.name,
              cell.version,
              Some(provider),
              asked,
              cell.platform,
            )
      yield stream
    for
      _ <- ZIO.unless(satisfied(module, provider))(
        ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider))
      )
      major  <- nativeMajor(module)
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
        lock,
        repositories,
        onCompiled = noted,
      )
      stamp  <- prepared(module, modules, cell, jdk, runtimes, lock, repositories)
      stream <-
        SuiteNames
          .discover(classes, linked.paths, provider.framework.value)
          .mapError(_ => ModuleTestError.Root(classes))
          .flatMap:
            case SuiteNames.Found.Unseen        => launch(patterns, linked, major)
            case SuiteNames.Found.Suites(names) =>
              val wanted =
                names.filter(name => SuitePattern.allows(patterns, name))
              if patterns.nonEmpty && wanted.isEmpty then ZIO.succeed(ZStream.empty)
              else if wanted.isEmpty then launch(patterns, linked, major)
              else
                remember(
                  repo,
                  cell,
                  wanted,
                  classes :: linked.paths,
                  stamp,
                  mode,
                  dirty => launch(exact(dirty), linked, major),
                )
    yield stream

  private[testrun] def nativeMajor(
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
            "the module does not name a clang major",
          )
        )

  private[testrun] def linkFailed(
      cell: CompiledCell,
      detail: String,
  ): ModuleTestError =
    ModuleTestError.LinkFailed(cell.module, cell.version, cell.platform, detail)

  private[testrun] def nativeToolchain(
      module: Module,
      cell: CompiledCell,
      native: NativeOrigin,
  ): ZIO[LockedTools, ModuleTestError, NativeToolchain] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      val revision =
        module.nativeVersion.getOrElse(tools.catalog.native.revision)
      ZIO
        .fromEither(NativeToolchain.of(revision, native))
        .mapError(error =>
          ModuleTestError.Tool(
            cell.module,
            cell.version,
            histos.toolchain.PlatformToolError.NativeOrigin(error),
          )
        )

  private[testrun] def node(
      cell: CompiledCell,
      runtimes: Runtimes,
  ): IO[ModuleTestError, NodeBinary] =
    runtimes.node.mapError(miss => ModuleTestError.NoNode(cell.module, cell.version, miss))

  private[testrun] def runCommand(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      argv: List[String],
      work: Path,
      env: Map[String, String],
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.succeed(TestFeed.Begin(module, version, platform)) ++
      ChildProcess
        .command(argv, work, env)
        .mapError(error => ModuleTestError.Process(module, version, error))
        .mapZIO(hear(module, version, platform, None, _))

  private[testrun] def finished(
      cell: CompiledCell,
      status: TestStatus,
  ): ZStream[Any, Nothing, TestFeed] =
    ZStream.succeed(
      TestFeed.Done(
        TestedCell(cell.module, cell.version, cell.platform, status)
      )
    )

  private final case class Stamped(name: String, suite: SuiteName, digest: Option[Sha256])

  /** A passing run stores each suite's fingerprint. [[TestMode.Quick]] skips a suite whose stored fingerprint still
    * matches. [[TestMode.Run]] receives the suites that still have to run. The class tree is read once and reused for
    * every suite. The digest is [[SuiteCover.stored]].
    */
  private[testrun] def remember[R](
      repo: Path,
      cell: CompiledCell,
      suites: List[String],
      directories: List[Path],
      ask: SuiteAsk,
      mode: TestMode,
      run: List[String] => ZIO[
        R,
        ModuleTestError,
        ZStream[Any, ModuleTestError, TestFeed],
      ],
  ): ZIO[R, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    ZIO
      .fromEither(SuiteCover.stored(ask))
      .mapError(refused(cell, _))
      .flatMap: cover =>
        SuiteStamp
          .index(directories)
          .mapError(ModuleTestError.Stamp(_))
          .flatMap: tree =>
            ZIO
              .foreach(suites): name =>
                for
                  suite <- suiteOf(name)
                  found <- ZIO
                    .fromEither(SuiteStamp.digest(name, tree, cover))
                    .mapError(ModuleTestError.Stamp(_))
                yield Stamped(name, suite, found)
              .flatMap: stamped =>
                if mode == TestMode.Run then
                  run(suites)
                    .map(stream => stream.tap(keep(repo, cell, stamped, _)))
                else
                  ZIO
                    .foldLeft(stamped)(
                      (List.empty[String], List.empty[Stamped])
                    ):
                      case ((cached, dirty), row @ Stamped(name, suite, Some(digest))) =>
                        QuickRecord
                          .hit(
                            repo,
                            cell.module,
                            cell.version,
                            cell.platform,
                            suite,
                            digest,
                          )
                          .mapError(ModuleTestError.Record(_))
                          .map: hit =>
                            if hit then (name :: cached, dirty)
                            else (cached, row :: dirty)
                      case ((cached, dirty), row) =>
                        ZIO.succeed((cached, row :: dirty))
                    .flatMap: (cached, dirty) =>
                      val shown = ZStream.fromIterable(
                        cached.sorted.map(name =>
                          TestFeed.Cached(
                            cell.module,
                            cell.version,
                            cell.platform,
                            name,
                          )
                        )
                      )
                      if dirty.isEmpty then ZIO.succeed(shown)
                      else
                        run(dirty.map(_.name)).map: stream =>
                          shown ++ stream.tap(keep(repo, cell, dirty, _))

  private def prepared(
      module: Module,
      modules: List[Module],
      cell: CompiledCell,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: histos.resolve.DependencyLock,
      repositories: histos.resolve.Repositories,
  ): ZIO[LockedTools & TestContext, Nothing, SuiteAsk] =
    for
      tools   <- ZIO.service[LockedTools]
      context <- ZIO.service[TestContext]
    yield SuiteAsk(
      module,
      modules,
      cell.version,
      cell.platform,
      lock,
      repositories,
      tools.catalog,
      runtimes.native,
      jdk.identity,
      nodeIdentity(context),
    )

  private def nodeIdentity(context: TestContext): Option[histos.toolchain.RuntimeIdentity] =
    context.runtimes match
      case _ :: identity :: Nil => Some(identity)
      case _                    => None

  private def refused(cell: CompiledCell, gap: CoverGap): ModuleTestError =
    gap match
      case CoverGap.Locked(error)                            => ModuleTestError.Lock(error)
      case CoverGap.Absent(module, version, platform, scope) =>
        ModuleTestError.Lock(histos.resolve.ResolveError.LockDrift(module, version, platform, scope))
      case CoverGap.NoNode =>
        ModuleTestError.NoNode(cell.module, cell.version, NodeMiss.Absent(NodeToolchain.Floor))

  /** The fingerprint is the digest of the classes that ran. A pass stores it. A failure deletes it.
    */
  private def suiteOf(raw: String): IO[ModuleTestError, SuiteName] =
    ZIO
      .fromEither(SuiteName.from(raw))
      .mapError(error => ModuleTestError.Record(QuickError.Suite(error)))

  private def keep(
      repo: Path,
      cell: CompiledCell,
      stamped: List[Stamped],
      event: TestFeed,
  ): IO[ModuleTestError, Unit] =
    event match
      case TestFeed.Done(done) =>
        done.status match
          case TestStatus.Passed =>
            ZIO.foreachDiscard(stamped):
              case Stamped(_, suite, Some(digest)) =>
                QuickRecord
                  .save(
                    repo,
                    cell.module,
                    cell.version,
                    cell.platform,
                    suite,
                    digest,
                  )
                  .mapError(ModuleTestError.Record(_))
              case _ => ZIO.unit
          case TestStatus.Failed(_) =>
            ZIO.foreachDiscard(stamped): row =>
              QuickRecord
                .clear(
                  repo,
                  cell.module,
                  cell.version,
                  cell.platform,
                  row.suite,
                )
                .mapError(ModuleTestError.Record(_))
          case _ => ZIO.unit
      case _ => ZIO.unit

  private[testrun] def exact(names: List[String]): List[SuitePattern] =
    names.flatMap(name => SuitePattern.from(name).toOption)

  private[testrun] def execute(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      mode: TestMode,
      noted: histos.zinc.CompileFacts => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & TestContext,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    for
      binary <- ZIO.fromEither(
        MainClass.from(main.value).left.map(ModuleTestError.Main(_))
      )
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
        lock,
        repositories,
        onCompiled = noted,
      )
      classes = TestCompile.testClasses(
        repo,
        module.name,
        cell.version,
        cell.platform,
      )
      asked  <- prepared(module, modules, cell, jdk, runtimes, lock, repositories)
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        asked,
        mode,
        _ =>
          TestCompile
            .fresh(
              TestCompile
                .testWork(repo, module.name, cell.version, cell.platform)
            )
            .map { work =>
              session(
                module.name,
                cell.version,
                None,
                request(
                  repo,
                  jdk,
                  runtimes,
                  classes,
                  linked.paths,
                  binary,
                  work,
                ),
                cell.platform,
              )
            },
      )
    yield stream

  private[testrun] def provide(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      mode: TestMode,
      noted: histos.zinc.CompileFacts => UIO[Unit] = _ => ZIO.unit,
  ): ZIO[
    Resolve & ProviderInstall & LockedTools & TestContext,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val classes =
      TestCompile.testClasses(repo, module.name, cell.version, cell.platform)
    def launch(
        selected: List[SuitePattern],
        linked: TestCompile.Linked,
    ): ZIO[
      Resolve & ProviderInstall & LockedTools & TestContext,
      ModuleTestError,
      ZStream[Any, ModuleTestError, TestFeed],
    ] =
      for
        runnerClasses <- ZIO.serviceWithZIO[ProviderInstall](
          _.classes(repo, module.name, cell.version, jdk, linked.paths)
        )
        keep   <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            TestCompile
              .fresh(
                TestCompile
                  .testWork(repo, module.name, cell.version, cell.platform)
              )
              .map: work =>
                val note =
                  ProviderNote(provider.framework.value, classes, selected)
                // The runner is the program, so it precedes the test classes. Those classes can contain another subprocess.Provider.
                session(
                  module.name,
                  cell.version,
                  Some(provider),
                  request(
                    repo,
                    jdk,
                    runtimes,
                    runnerClasses,
                    classes :: linked.paths,
                    ProviderRunner.Main,
                    work,
                    note.environment,
                  ),
                  cell.platform,
                )
      yield stream
    for
      _ <- ZIO.unless(satisfied(module, provider))(
        ZIO.fail(ModuleTestError.ProviderDependency(module.name, provider))
      )
      stream <-
        for
          linked <- TestCompile.compiledTests(
            repo,
            modules,
            module,
            cell,
            sources,
            jdk,
            runtimes.native,
            lock,
            repositories,
            onCompiled = noted,
          )
          stamp <- prepared(module, modules, cell, jdk, runtimes, lock, repositories)
          found <- SuiteNames
            .discover(classes, linked.paths, provider.framework.value)
            .mapError(_ => ModuleTestError.Root(classes))
          ran <- found match
            case SuiteNames.Found.Unseen        => launch(patterns, linked)
            case SuiteNames.Found.Suites(names) =>
              val wanted =
                names.filter(name => SuitePattern.allows(patterns, name))
              if patterns.nonEmpty && wanted.isEmpty then ZIO.succeed(ZStream.empty)
              else if wanted.isEmpty then launch(patterns, linked)
              else
                remember(
                  repo,
                  cell,
                  wanted,
                  classes :: linked.paths,
                  stamp,
                  mode,
                  dirty => launch(exact(dirty), linked),
                )
        yield ran
    yield stream

  private[testrun] def request(
      repo: Path,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      classes: Path,
      classpath: List[Path],
      main: MainClass,
      work: Path,
      extra: Map[String, String] = Map.empty,
  ): ChildRequest =
    ChildRequest(
      Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      ChildClasspath(classes, classpath),
      main,
      work,
      TestCompile.suiteEnv(repo, runtimes.env) ++ extra,
    )

  /** Header, then lines as the process writes them, then the cell. `NoneFound` fails the stream.
    */
  private[testrun] def session(
      module: ProjectName,
      version: ScalaVersion,
      provider: Option[TestProvider],
      asked: ChildRequest,
      shown: Platform,
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.succeed(TestFeed.Begin(module, version, shown)) ++
      ChildProcess
        .events(asked)
        .mapError(error => ModuleTestError.Process(module, version, error))
        .mapZIO(hear(module, version, shown, provider, _))

  private[testrun] def hear(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      provider: Option[TestProvider],
      event: ChildEvent,
  ): ZIO[Any, ModuleTestError, TestFeed] =
    event match
      case ChildEvent.Out(text)    => ZIO.succeed(TestFeed.Out(text))
      case ChildEvent.Err(text)    => ZIO.succeed(TestFeed.Err(text))
      case ChildEvent.Exited(code) =>
        provider match
          case Some(_) if code.value == ProviderRunner.NoneMatched =>
            ZIO.succeed(
              TestFeed.Done(
                TestedCell(module, version, platform, TestStatus.Unmatched)
              )
            )
          case Some(found) if code.value == ProviderRunner.NoneFound =>
            ZIO.fail(ModuleTestError.NoneDiscovered(module, found))
          case _ =>
            val status =
              if code.value == 0 then TestStatus.Passed
              else TestStatus.Failed(code.value)
            ZIO.succeed(
              TestFeed.Done(TestedCell(module, version, platform, status))
            )

  private[testrun] def satisfied(
      module: Module,
      provider: TestProvider,
  ): Boolean =
    val library = provider.library
    library.organization.nonEmpty && library.name.nonEmpty &&
    module.testDependencies.exists(dep => dep.organization == library.organization && dep.name == library.name)

  /** JVM class files. No class files means the platform's child still filters.
    */
  private[testrun] def narrowed(
      root: Path,
      patterns: List[SuitePattern],
  ): IO[ModuleTestError, Boolean] =
    if patterns.isEmpty then ZIO.succeed(true)
    else
      ZIO
        .attemptBlocking(classBinaries(root))
        .mapError(_ => ModuleTestError.Root(root))
        .map: names =>
          names.isEmpty || names.exists(name => SuitePattern.allows(patterns, name))

  private[testrun] def classBinaries(root: Path): List[String] =
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.flatMap(path => classBinary(root, path)).toList
      finally walk.close()

  private[testrun] def classBinary(root: Path, path: Path): Option[String] =
    if !Files.isRegularFile(path) then None
    else
      val name = root.relativize(path).toString.replace('\\', '/')
      if !name.endsWith(".class") then None
      else
        val binary = name.stripSuffix(".class").replace('/', '.')
        if binary.isEmpty || binary.contains("$$") then None else Some(binary)

end SuiteLaunch
