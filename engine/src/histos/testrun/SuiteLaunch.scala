package histos.testrun

import histos.jdk.Sha256
import histos.js.{
  JsLink,
  JsLinkError,
  JsLinkRequest,
  JsSuite,
  JsSuiteRequest,
  ScalaJs
}
import histos.native.{
  NativeCompile,
  NativeLink,
  NativeLinkRequest,
  NativeOrigin,
  NativeSuite,
  NativeSuiteRequest,
  NativeToolchain
}
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
import histos.process.{
  ChildClasspath,
  ChildError,
  ChildEvent,
  ChildProcess,
  ChildRequest
}
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

private[testrun] object SuiteLaunch:
  private[testrun] def runJs(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      quick: Boolean
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
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
        repositories
      )
      classes = TestCompile.testClasses(
        repo,
        module.name,
        cell.version,
        cell.platform
      )
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        linked.hashes,
        quick,
        _ =>
          for
            chosen <- node(cell, runtimes)
            _ <- TestCompile.fresh(linkDir)
            script <- JsLink
              .link(
                JsLinkRequest(
                  cell.version,
                  binary,
                  classes :: linked.paths,
                  linkDir,
                  jdk
                )
              )
              .mapError(error => linkFailed(cell, error.message))
            work <- TestCompile.fresh(
              TestCompile.testWork(
                repo,
                module.name,
                cell.version,
                cell.platform
              )
            )
          yield runCommand(
            module.name,
            cell.version,
            cell.platform,
            List(chosen.path.toString, script.toString),
            work,
            TestCompile.suiteEnv(repo, runtimes.env)
          )
      )
    yield stream

  private[testrun] def runJsProvider(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      quick: Boolean
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    val classes =
      TestCompile.testClasses(repo, module.name, cell.version, cell.platform)
    def launch(
        selected: List[SuitePattern],
        linked: TestCompile.Linked
    ): ZIO[
      Resolve & ProviderInstall,
      ModuleTestError,
      ZStream[Any, ModuleTestError, TestFeed]
    ] =
      for
        keep <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            for
              chosen <- node(cell, runtimes)
              _ <- TestCompile.fresh(linkDir)
              asked <- JsSuite
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
                    selected
                  )
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
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
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

  private[testrun] def runNative(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      quick: Boolean
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    for
      major <- nativeMajor(module)
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
        repositories
      )
      classes = TestCompile.testClasses(
        repo,
        module.name,
        cell.version,
        cell.platform
      )
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        linked.hashes,
        quick,
        _ =>
          for
            _ <- TestCompile.fresh(linkDir)
            toolchain <- nativeToolchain(module, cell, runtimes.native)
            program <- NativeLink
              .linkClasspath(
                NativeLinkRequest(
                  major,
                  toolchain,
                  TestCompile.suiteEnv(repo, runtimes.env),
                  linkDir,
                  jdk,
                  binary
                ),
                classes :: linked.paths
              )
              .mapError(error => linkFailed(cell, error.message))
            work <- TestCompile.fresh(
              TestCompile.testWork(
                repo,
                module.name,
                cell.version,
                cell.platform
              )
            )
          yield runCommand(
            module.name,
            cell.version,
            cell.platform,
            List(program.toString),
            work,
            TestCompile.suiteEnv(repo, runtimes.env)
          )
      )
    yield stream

  private[testrun] def runNativeProvider(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      quick: Boolean
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
  ] =
    val linkDir =
      TestCompile.linkWork(repo, module.name, cell.version, cell.platform)
    val classes =
      TestCompile.testClasses(repo, module.name, cell.version, cell.platform)
    val entry = MainClass.trusted("scala.scalanative.testinterface.TestMain")
    def launch(
        selected: List[SuitePattern],
        linked: TestCompile.Linked,
        major: histos.ClangMajor
    ): ZIO[
      Resolve & ProviderInstall,
      ModuleTestError,
      ZStream[Any, ModuleTestError, TestFeed]
    ] =
      for
        keep <- narrowed(classes, selected)
        stream <-
          if !keep then ZIO.succeed(ZStream.empty)
          else
            for
              _ <- TestCompile.fresh(linkDir)
              classpath = classes :: linked.paths
              toolchain <- nativeToolchain(module, cell, runtimes.native)
              binary <- NativeLink
                .linkClasspath(
                  NativeLinkRequest(
                    major,
                    toolchain,
                    TestCompile.suiteEnv(repo, runtimes.env),
                    linkDir,
                    jdk,
                    entry
                  ),
                  classpath
                )
                .mapError(error => linkFailed(cell, error.message))
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
                    selected
                  )
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
      linked <- TestCompile.compiledTests(
        repo,
        modules,
        module,
        cell,
        sources,
        jdk,
        runtimes.native,
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
            "the module does not name a clang major"
          )
        )

  private[testrun] def linkFailed(
      cell: CompiledCell,
      detail: String
  ): ModuleTestError =
    ModuleTestError.LinkFailed(cell.module, cell.version, cell.platform, detail)

  private[testrun] def nativeToolchain(
      module: Module,
      cell: CompiledCell,
      native: NativeOrigin
  ): IO[ModuleTestError, NativeToolchain] =
    val revision = NativeToolchain.revision(module.nativeVersion)
    ZIO
      .fromEither(NativeToolchain.of(revision, native))
      .mapError(error => linkFailed(cell, error.message))

  private[testrun] def node(
      cell: CompiledCell,
      runtimes: Runtimes
  ): IO[ModuleTestError, NodeBinary] =
    runtimes.node.mapError(miss =>
      ModuleTestError.NoNode(cell.module, cell.version, miss)
    )

  private[testrun] def runCommand(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      argv: List[String],
      work: Path,
      env: Map[String, String]
  ): ZStream[Any, ModuleTestError, TestFeed] =
    ZStream.succeed(TestFeed.Begin(module, version, platform)) ++
      ChildProcess
        .command(argv, work, env)
        .mapError(error => ModuleTestError.Process(module, version, error))
        .mapZIO(hear(module, version, platform, None, _))

  private[testrun] def finished(
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
  private[testrun] def remember(
      repo: Path,
      cell: CompiledCell,
      suites: List[String],
      directories: List[Path],
      hashes: List[Sha256],
      quick: Boolean,
      run: List[String] => ZIO[
        Resolve & ProviderInstall,
        ModuleTestError,
        ZStream[Any, ModuleTestError, TestFeed]
      ]
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
  ] =
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
                    case TestStatus.Passed =>
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
                    case TestStatus.Failed(_) =>
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

  private[testrun] def exact(names: List[String]): List[SuitePattern] =
    names.flatMap(name => SuitePattern.from(name).toOption)

  private[testrun] def execute(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      main: TestMain,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      quick: Boolean
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
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
        repositories
      )
      classes = TestCompile.testClasses(
        repo,
        module.name,
        cell.version,
        cell.platform
      )
      stream <- remember(
        repo,
        cell,
        List(main.value),
        classes :: linked.paths,
        linked.hashes,
        quick,
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
                  work
                ),
                cell.platform
              )
            }
      )
    yield stream

  private[testrun] def provide(
      repo: Path,
      modules: List[Module],
      module: Module,
      cell: CompiledCell,
      sources: List[Path],
      provider: TestProvider,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      lock: DependencyLock,
      repositories: Repositories,
      patterns: List[SuitePattern],
      quick: Boolean
  ): ZIO[
    Resolve & ProviderInstall,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed]
  ] =
    val classes =
      TestCompile.testClasses(repo, module.name, cell.version, cell.platform)
    def launch(
        selected: List[SuitePattern]
    ): ZIO[
      Resolve & ProviderInstall,
      ModuleTestError,
      ZStream[Any, ModuleTestError, TestFeed]
    ] =
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
          repositories
        )
        runnerClasses <- ZIO.serviceWithZIO[ProviderInstall](
          _.classes(repo, module.name, cell.version, jdk, linked.paths)
        )
        keep <- narrowed(classes, selected)
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
                    note.environment
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
            linked <- TestCompile.compiledTests(
              repo,
              modules,
              module,
              cell,
              sources,
              jdk,
              runtimes.native,
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

  private[testrun] def request(
      repo: Path,
      jdk: ResolvedJdk,
      runtimes: Runtimes,
      classes: Path,
      classpath: List[Path],
      main: MainClass,
      work: Path,
      extra: Map[String, String] = Map.empty
  ): ChildRequest =
    ChildRequest(
      Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      ChildClasspath(classes, classpath),
      main,
      work,
      TestCompile.suiteEnv(repo, runtimes.env) ++ extra
    )

  /** Header, then lines as the process writes them, then the cell. `NoneFound`
    * fails the stream.
    */
  private[testrun] def session(
      module: ProjectName,
      version: ScalaVersion,
      provider: Option[TestProvider],
      asked: ChildRequest,
      shown: Platform
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
      event: ChildEvent
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
      provider: TestProvider
  ): Boolean =
    val library = provider.library
    library.organization.nonEmpty && library.name.nonEmpty &&
    module.testDependencies.exists(dep =>
      dep.organization == library.organization && dep.name == library.name
    )

  /** JVM class files. No class files means the platform's child still filters.
    */
  private[testrun] def narrowed(
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
