package histos.testrun

import histos.build.{ExecutionError, ExecutionProducts, StageProduct}
import histos.jdk.{JdkRuntime, Sha256}
import histos.js.{JsLink, JsLinkRequest, JsRun, JsRunRequest, JsSuite, JsSuiteRequest}
import histos.native.{NativeLink, NativeLinkRequest, NativeRequests, NativeSuite, NativeSuiteRequest, NativeToolchain}
import histos.pack.MainClass
import histos.plugin.SourceSet
import histos.process.{ChildEvent, ChildProcess, ChildRequest, ChildClasspath}
import histos.resolve.LockedTools
import histos.toolchain.Runtimes
import histos.zinc.StageRoots
import histos.{Cell, ClangPin, Dep, JsOptimize, JvmOptions, Module, Platform, SystemProperty, TestProvider, TestStore}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}
import zio.stream.ZStream

private[testrun] object SuiteLaunch:
  private type Env = ModuleTest.RunEnv & ProviderInstall
  private final case class Target(module: Module, main: StageProduct, tests: StageProduct):
    def cell: Cell = tests.stage.cell

  private final case class Suite(
      target: Target,
      provider: Option[TestProvider],
      name: String,
      id: SuiteName,
      digest: Option[Sha256],
  )

  private enum Launch:
    case Jvm(request: ChildRequest)
    case Command(binary: Path, work: Path, environment: Map[String, String])

  /** Discovery is restricted to requested Test outputs. Runtime dependencies are never discovery roots. */
  def run(asked: TestRun, group: TestExecutionGroup, completed: ExecutionProducts): ZIO[
    Env,
    ModuleTestError | ExecutionError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] =
    val ownerCell = group match
      case TestExecutionGroup.Direct(cell)              => cell
      case TestExecutionGroup.NativeAggregate(owner, _) => owner
    for
      owner     <- module(asked, ownerCell)
      ownerMain <- ZIO.fromEither(completed.require(histos.CompilationStage(ownerCell, SourceSet.Main)))
      targets   <- ZIO.foreach(group.requestedCells): cell =>
        for
          selected <- module(asked, cell)
          main     <- ZIO.fromEither(completed.require(histos.CompilationStage(cell, SourceSet.Main)))
          tests    <- ZIO.fromEither(completed.require(histos.CompilationStage(cell, SourceSet.Test)))
        yield Target(selected, main, tests)
      paths = (targets.flatMap(_.tests.classpath) ++ ownerMain.classpath).distinct
      identities <- NativeLink.inputDigests(paths).mapError(error => nativeFailure(ownerCell, error))
      context    <- ZIO.service[LockedTools]
      tools      <- ZIO
        .foreach(targets)(target =>
          context.compilation(target.module, target.cell.version, target.cell.platform, asked.runtimes.native)
        )
        .mapError(ModuleTestError.Lock(_))
      cover = SuiteStamp.Cover(
        (identities ++ tools.flatten.map(_.fingerprint)).map(_.value),
        Nil,
        Runtimes.identityToken(asked.jdk.identity),
        asked.filter.argv ++ targets.flatMap(target =>
          target.module.flags.argv ++ target.module.jvmOptions.arguments ++
            target.module.provider.toList.flatMap(_.properties.map(_.argument))
        ) ++
          List(s"owner:${owner.name.value}", s"stack:${owner.threadStack.value}") ++ owner.libraries.toList
            .map(_.token)
            .sorted,
        targets.map(target => s"${target.cell.module.value}:${target.cell.version.value}:${target.tests.api.value}"),
      )
      suites <- ZIO
        .foreach(targets)(target =>
          discover(asked, target).flatMap: discovered =>
            SuiteStamp
              .digests(discovered.map(_._2), paths, cover)
              .mapError(ModuleTestError.Stamp(_))
              .flatMap: digests =>
                ZIO.foreach(discovered): (provider, name) =>
                  ZIO
                    .fromEither(SuiteName.from(name))
                    .mapError(error => ModuleTestError.Record(QuickError.Suite(error)))
                    .map(id => Suite(target, provider, name, id, digests.get(name).flatten))
        )
        .map(_.flatten)
      duplicate = suites
        .groupBy(_.id)
        .toList
        .flatMap: (name, found) =>
          val modules = found.map(_.target.cell.module).distinct
          if modules.size > 1 then
            zio.NonEmptyChunk.fromIterableOption(modules).map(ScopedTestFailure.DuplicateSuite(name, _)).toList
          else Nil
      _ <- duplicate.headOption match
        case Some(error) => ZIO.fail(ModuleTestError.Scoped(error))
        case None        => ZIO.unit
      cached <- ZIO.filter(suites): suite =>
        if asked.mode == TestMode.Quick || suite.target.module.testStore == TestStore.Local then
          suite.digest match
            case Some(digest) =>
              QuickRecord
                .hit(
                  asked.repo,
                  suite.target.cell.module,
                  suite.target.cell.version,
                  suite.target.cell.platform,
                  suite.id,
                  digest,
                )
                .mapError(ModuleTestError.Record(_))
            case None => ZIO.succeed(false)
        else ZIO.succeed(false)
      dirty = suites.filterNot(cached.contains)
      resources <- ZIO.foreach(completed.stages.values.toList.filter(product => paths.contains(product.classes))):
        product =>
          StageRoots
            .of(asked.graph)
            .read(asked.repo, product.stage, completed.generatedRoots)
            .mapError(ExecutionError.Roots(_))
            .map(_.resources.map(_._1))
      launched <-
        if dirty.isEmpty then ZIO.succeed(ZStream.empty)
        else
          group match
            case TestExecutionGroup.Direct(_) => direct(asked, owner, ownerMain, dirty, resources.flatten.distinct)
            case TestExecutionGroup.NativeAggregate(_, _) =>
              aggregate(asked, owner, ownerMain, paths, targets, dirty, resources.flatten.distinct)
      empty = if asked.patterns.nonEmpty then Nil else targets.filter(_.tests.compiled.classFiles == 0)
      shown = ZStream.fromIterable(
        cached.map(suite =>
          TestFeed.Cached(suite.target.cell.module, suite.target.cell.version, suite.target.cell.platform, suite.name)
        )
      )
      noTests = ZStream.fromIterable(empty.map(target => done(target.cell, TestStatus.NoTests)))
    yield shown ++ launched ++ noTests

  private def module(asked: TestRun, cell: Cell): IO[ModuleTestError, Module] =
    ZIO
      .fromOption(asked.graph.projects.collectFirst { case module: Module if module.name == cell.module => module })
      .orElseFail(ModuleTestError.Absent(cell.module))

  private def discover(asked: TestRun, target: Target): IO[ModuleTestError, List[(Option[TestProvider], String)]] =
    if target.tests.compiled.classFiles == 0 then
      (target.module.test, target.module.provider) match
        case (Some(_), Some(_)) => ZIO.fail(ModuleTestError.Mixed(target.cell.module))
        case (Some(main), None) => ZIO.fail(ModuleTestError.MissingSources(target.cell.module, main))
        case (None, Some(provider))
            if target.module.testAggregates.isEmpty || target.cell.platform != Platform.Native =>
          ZIO.fail(ModuleTestError.MissingProvider(target.cell.module, provider))
        case (None, Some(_)) | (None, None) => ZIO.succeed(Nil)
    else
      (target.module.test, target.module.provider) match
        case (Some(_), Some(_)) => ZIO.fail(ModuleTestError.Mixed(target.cell.module))
        case (Some(main), None) =>
          ZIO.succeed(if SuiteMatch.allows(asked.patterns, main.value) then List(None -> main.value) else Nil)
        case (None, configured) =>
          val provider = configured.getOrElse(TestProvider.Zio)
          if !satisfied(target.module, provider, target.cell.platform) then
            ZIO.fail(ModuleTestError.ProviderDependency(target.cell.module, provider))
          else
            SuiteNames
              .discover(target.tests.classes, target.tests.classpath, provider.framework.value)
              .mapError(_ => ModuleTestError.Root(target.tests.classes))
              .flatMap:
                case SuiteNames.Found.Suites(names) =>
                  if names.isEmpty then ZIO.fail(ModuleTestError.NoneDiscovered(target.cell.module, provider))
                  else ZIO.succeed(names.filter(SuiteMatch.allows(asked.patterns, _)).map(Some(provider) -> _))
                case SuiteNames.Found.Unseen =>
                  irNames(target.tests.classes).map(names =>
                    names.filter(SuiteMatch.allows(asked.patterns, _)).map(Some(provider) -> _)
                  )

  private def direct(
      asked: TestRun,
      owner: Module,
      main: StageProduct,
      suites: List[Suite],
      resources: List[histos.ResourceName],
  ): ZIO[
    Env,
    ModuleTestError,
    ZStream[Any, ModuleTestError, TestFeed],
  ] = suites match
    case Nil        => ZIO.succeed(ZStream.empty)
    case first :: _ =>
      val target = first.target
      val cell   = target.cell
      val paths  = target.tests.classpath
      val work   = TestCompile.testWork(asked.repo, cell.module, cell.version, cell.platform)
      val link   = TestCompile.linkWork(asked.repo, cell.module, cell.version, cell.platform)
      val launch: ZIO[Env, ModuleTestError, Launch] = first.provider match
        case None =>
          ZIO
            .fromEither(MainClass.from(first.name))
            .mapError(ModuleTestError.Main(_))
            .flatMap: binary =>
              cell.platform match
                case Platform.Jvm =>
                  TestCompile
                    .fresh(work)
                    .map: scratch =>
                      Launch.Jvm(
                        request(
                          asked.repo,
                          asked.jdk,
                          asked.runtimes,
                          target.tests.classes,
                          paths.filterNot(_ == target.tests.classes),
                          binary,
                          scratch,
                          jvmOptions = owner.jvmOptions,
                        )
                      )
                case Platform.Js     => jsMain(asked, owner, cell, paths, binary, link).map(Launch.Jvm(_))
                case Platform.Native => nativeMain(asked, owner, cell, paths, binary, link, work, resources)
        case Some(provider) =>
          cell.platform match
            case Platform.Jvm =>
              for
                permitted <- histos.zinc.ScalacClasspath
                  .substituted(asked.repo, target.module, histos.load.ApiDep.modules(asked.graph))
                  .mapError(ModuleTestError.Compile(_))
                runner <- ZIO.serviceWithZIO[ProviderInstall](
                  _.classes(asked.repo, cell.module, cell.version, asked.jdk, paths, permitted)
                )
                scratch  <- TestCompile.fresh(work)
                patterns <- exact(suites.map(_.name))
              yield Launch.Jvm(
                request(
                  asked.repo,
                  asked.jdk,
                  asked.runtimes,
                  runner,
                  paths,
                  ProviderRunner.Main,
                  scratch,
                  ProviderNote(provider.framework.value, target.tests.classes, patterns, asked.filter).environment,
                  provider.properties,
                  owner.jvmOptions,
                )
              )
            case Platform.Js =>
              for
                node     <- asked.runtimes.node.mapError(ModuleTestError.NoNode(cell.module, cell.version, _))
                _        <- TestCompile.fresh(link)
                patterns <- exact(suites.map(_.name))
                child    <- JsSuite
                  .run(
                    JsSuiteRequest(
                      cell.version,
                      owner.jsEnv,
                      owner.jsModule,
                      provider.framework.value,
                      paths,
                      link,
                      asked.jdk,
                      TestCompile.suiteEnv(asked.repo, asked.runtimes.env) ++ TestFilter.environment(asked.filter),
                      node,
                      patterns,
                      owner.jsVersion,
                    )
                  )
                  .mapError(error =>
                    ModuleTestError.Tool(cell.module, cell.version, histos.toolchain.PlatformToolError.JsSuite(error))
                  )
              yield Launch.Jvm(child.copy(jvmOptions = merge(owner.jvmOptions, provider.properties)))
            case Platform.Native =>
              nativeProvider(asked, owner, main, paths, List(owner), suites, provider, resources).map(Launch.Jvm(_))
      launch.map(child => completed(asked, cell, suites, child))

  private def aggregate(
      asked: TestRun,
      owner: Module,
      main: StageProduct,
      paths: List[Path],
      targets: List[Target],
      suites: List[Suite],
      resources: List[histos.ResourceName],
  ): ZIO[Env, ModuleTestError, ZStream[Any, ModuleTestError, TestFeed]] =
    suites match
      case Nil        => ZIO.succeed(ZStream.empty)
      case first :: _ =>
        first.provider match
          case None =>
            ZIO.fail(
              ModuleTestError.LinkFailed(
                owner.name,
                main.stage.cell.version,
                Platform.Native,
                s"native aggregate member ${first.target.cell.module.value} names a test main",
              )
            )
          case Some(provider) =>
            val compatible = suites.forall(_.provider.exists(_.framework == provider.framework))
            if !compatible then
              ZIO.fail(
                ModuleTestError.LinkFailed(
                  owner.name,
                  main.stage.cell.version,
                  Platform.Native,
                  "native aggregate members must use the same framework",
                )
              )
            else
              nativeProvider(asked, owner, main, paths, targets.map(_.module), suites, provider, resources)
                .map(child => completed(asked, main.stage.cell, suites, Launch.Jvm(child)))

  private def nativeProvider(
      asked: TestRun,
      owner: Module,
      main: StageProduct,
      paths: List[Path],
      members: List[Module],
      suites: List[Suite],
      provider: TestProvider,
      resources: List[histos.ResourceName],
  ): ZIO[
    Env,
    ModuleTestError,
    ChildRequest,
  ] =
    val cell = main.stage.cell
    val work = TestCompile.linkWork(asked.repo, owner.name, cell.version, Platform.Native)
    for
      major     <- nativeMajor(owner)
      toolchain <- nativeToolchain(owner, cell, asked)
      view      <- ZIO.service[LockedTools]
      iface     <- view
        .materialize(NativeRequests.testInterface(cell.version, toolchain, view.repositories))
        .mapError(ModuleTestError.Lock(_))
        .map(_.artifacts.map(_.path))
      classpath = (paths.filterNot(replacedInterface) ++ iface).distinct
      _      <- TestCompile.fresh(work)
      binary <- NativeLink
        .linkClasspath(
          NativeLinkRequest(
            major,
            toolchain,
            TestCompile.suiteEnv(asked.repo, asked.runtimes.env),
            work,
            asked.jdk,
            MainClass.trusted("scala.scalanative.testinterface.TestMain"),
            cell.version,
            libraries = owner.libraries ++ members.flatMap(_.libraries),
            resources = resources,
          ),
          classpath,
        )
        .mapError(error => nativeFailure(cell, error))
      patterns <- exact(suites.map(_.name))
      child    <- NativeSuite
        .run(
          NativeSuiteRequest(
            toolchain,
            provider.framework.value,
            binary,
            classpath,
            work,
            asked.jdk,
            TestCompile.suiteEnv(asked.repo, asked.runtimes.env) ++ TestFilter.environment(asked.filter),
            owner.threadStack,
            patterns,
          )
        )
        .mapError(error =>
          ModuleTestError.Tool(cell.module, cell.version, histos.toolchain.PlatformToolError.NativeSuite(error))
        )
      properties = suites.flatMap(_.provider.toList.flatMap(_.properties)).distinct
    yield child.copy(jvmOptions = merge(owner.jvmOptions, properties))

  private def completed(
      asked: TestRun,
      owner: Cell,
      suites: List[Suite],
      child: Launch,
  ): ZStream[Any, ModuleTestError, TestFeed] =
    val cells   = suites.map(_.target.cell).distinct
    val process = child match
      case Launch.Jvm(request)                       => ChildProcess.events(request)
      case Launch.Command(binary, work, environment) => ChildProcess.command(List(binary.toString), work, environment)
    val events = process
      .mapError(ModuleTestError.Process(owner.module, owner.version, _))
      .flatMap:
        case ChildEvent.Out(text)    => ZStream.succeed(TestFeed.Out(owner.module, owner.version, owner.platform, text))
        case ChildEvent.Err(text)    => ZStream.succeed(TestFeed.Err(owner.module, owner.version, owner.platform, text))
        case ChildEvent.Bytes(bytes) =>
          ZStream.succeed(
            TestFeed.Out(owner.module, owner.version, owner.platform, new String(bytes.toArray, StandardCharsets.UTF_8))
          )
        case ChildEvent.Exited(code) =>
          val provider = suites.flatMap(_.provider).headOption
          provider match
            case Some(found) if code.value == ProviderRunner.NoneFound =>
              ZStream.fail(ModuleTestError.NoneDiscovered(owner.module, found))
            case Some(_) if code.value == ProviderRunner.NoneMatched =>
              ZStream.fromIterable(cells.map(done(_, TestStatus.Unmatched)))
            case Some(_) | None =>
              val status = if code.value == 0 then TestStatus.Passed else TestStatus.Failed(code.value)
              ZStream.fromZIO(record(asked, suites, status)).drain ++ ZStream.fromIterable(cells.map(done(_, status)))
    ZStream.succeed(TestFeed.Begin(owner.module, owner.version, owner.platform)) ++ events

  private def record(asked: TestRun, suites: List[Suite], status: TestStatus): IO[ModuleTestError, Unit] =
    status match
      case TestStatus.Passed =>
        ZIO.foreachDiscard(suites): suite =>
          suite.digest match
            case None         => ZIO.unit
            case Some(digest) =>
              QuickRecord
                .save(
                  asked.repo,
                  suite.target.cell.module,
                  suite.target.cell.version,
                  suite.target.cell.platform,
                  suite.id,
                  digest,
                )
                .mapError(ModuleTestError.Record(_))
      case TestStatus.Failed(_) =>
        ZIO.foreachDiscard(suites)(suite =>
          QuickRecord
            .clear(
              asked.repo,
              suite.target.cell.module,
              suite.target.cell.version,
              suite.target.cell.platform,
              suite.id,
            )
            .mapError(ModuleTestError.Record(_))
        )
      case TestStatus.NoTests | TestStatus.Unmatched => ZIO.unit

  private def done(cell: Cell, status: TestStatus): TestFeed =
    TestFeed.Done(TestedCell(cell.module, cell.version, cell.platform, status))

  private def jsMain(asked: TestRun, module: Module, cell: Cell, paths: List[Path], binary: MainClass, work: Path): ZIO[
    Env,
    ModuleTestError,
    ChildRequest,
  ] =
    for
      node   <- asked.runtimes.node.mapError(ModuleTestError.NoNode(cell.module, cell.version, _))
      view   <- ZIO.service[LockedTools]
      _      <- TestCompile.fresh(work)
      linked <- JsLink
        .link(
          JsLinkRequest(
            cell.version,
            binary,
            paths,
            work,
            asked.jdk,
            kind = module.jsModule,
            revision = module.jsVersion,
            optimize = JsOptimize.effective(view.catalog.js.optimize, module.jsOptimize, None),
          )
        )
        .mapError(error =>
          ModuleTestError.Tool(cell.module, cell.version, histos.toolchain.PlatformToolError.JsLink(error))
        )
      child <- JsRun
        .start(
          JsRunRequest(
            module.jsEnv,
            module.jsModule,
            linked.script,
            work,
            asked.jdk,
            TestCompile.suiteEnv(asked.repo, asked.runtimes.env),
            node,
          )
        )
        .mapError(error =>
          ModuleTestError.Tool(cell.module, cell.version, histos.toolchain.PlatformToolError.JsRun(error))
        )
    yield child

  private def nativeMain(
      asked: TestRun,
      module: Module,
      cell: Cell,
      paths: List[Path],
      binary: MainClass,
      link: Path,
      work: Path,
      resources: List[histos.ResourceName],
  ): ZIO[Env, ModuleTestError, Launch] =
    for
      major     <- nativeMajor(module)
      toolchain <- nativeToolchain(module, cell, asked)
      _         <- TestCompile.fresh(link)
      program   <- NativeLink
        .linkClasspath(
          NativeLinkRequest(
            major,
            toolchain,
            asked.runtimes.env,
            link,
            asked.jdk,
            binary,
            cell.version,
            libraries = module.libraries,
            resources = resources,
          ),
          paths,
        )
        .mapError(nativeFailure(cell, _))
      scratch <- TestCompile.fresh(work)
    yield Launch.Command(
      program,
      scratch,
      TestCompile.suiteEnv(asked.repo, asked.runtimes.env) ++
        histos.native.NativeStack.environment(module.threadStack),
    )

  private def nativeMajor(module: Module): IO[ModuleTestError, histos.ClangMajor] = module.clang match
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

  private def nativeToolchain(
      module: Module,
      cell: Cell,
      asked: TestRun,
  ): ZIO[LockedTools, ModuleTestError, NativeToolchain] =
    ZIO.serviceWithZIO[LockedTools](view =>
      ZIO
        .fromEither(
          NativeToolchain.of(module.nativeVersion.getOrElse(view.catalog.native.revision), asked.runtimes.native)
        )
        .mapError(error =>
          ModuleTestError.Tool(cell.module, cell.version, histos.toolchain.PlatformToolError.NativeOrigin(error))
        )
    )

  private def nativeFailure(cell: Cell, error: histos.native.NativeError): ModuleTestError =
    ModuleTestError.Tool(cell.module, cell.version, histos.toolchain.PlatformToolError.Native(error))

  private def exact(names: List[String]): IO[ModuleTestError, List[SuitePattern]] =
    ZIO.foreach(names.distinct)(name => ZIO.fromEither(SuitePattern.from(name)).mapError(ModuleTestError.Pattern(_)))

  private def irNames(root: Path): IO[ModuleTestError, List[String]] =
    ZIO
      .attemptBlocking:
        val walk = Files.walk(root)
        try
          walk.iterator.asScala
            .filter(Files.isRegularFile(_))
            .flatMap: path =>
              val name = root.relativize(path).toString.replace('\\', '/')
              if name.endsWith(".sjsir") then Some(name.stripSuffix(".sjsir").replace('/', '.').stripSuffix("$"))
              else None
            .toList
            .distinct
        finally walk.close()
      .mapError(_ => ModuleTestError.Root(root))

  private def replacedInterface(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.startsWith("test-interface_native"))

  private[testrun] def satisfied(module: Module, provider: TestProvider): Boolean =
    module.testDependencies.exists(dep =>
      dep.organization == provider.library.organization && dep.name == provider.library.name
    )

  private def satisfied(module: Module, provider: TestProvider, platform: Platform): Boolean =
    Dep
      .select(module.testDependencies, platform)
      .exists(dep => dep.organization == provider.library.organization && dep.name == provider.library.name)

  private[testrun] def merge(options: JvmOptions, properties: List[SystemProperty]): JvmOptions =
    val supplied = properties.map(property => property.value.takeWhile(_ != '=')).toSet
    options.copy(properties =
      options.properties.filterNot(property => supplied.contains(property.value.takeWhile(_ != '='))) ++ properties
    )

  private[testrun] def request(
      repo: Path,
      jdk: JdkRuntime,
      runtimes: Runtimes,
      classes: Path,
      classpath: List[Path],
      main: MainClass,
      work: Path,
      extra: Map[String, String] = Map.empty,
      properties: List[SystemProperty] = Nil,
      jvmOptions: JvmOptions = JvmOptions.empty,
  ): ChildRequest =
    ChildRequest(
      Platform.Jvm,
      jdk.home.resolve("bin/java"),
      ChildClasspath(classes, classpath),
      main,
      work,
      TestCompile.suiteEnv(repo, runtimes.env) ++ extra,
      jvmOptions = merge(jvmOptions, properties),
    )
end SuiteLaunch
