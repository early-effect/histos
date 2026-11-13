package histos

import histos.cache.{ContentStore, LinkerStarts}
import histos.clean.Products
import histos.jdk.{Machine, Sha256}
import histos.native.{NativeError, NativeLink, NativeLinkRequest, NativeToolchain, NativeToolchainError}
import histos.program.{Program, ProgramError, ProgramFeed}
import histos.resolve.{
  BuildResolvers,
  DependencyLock,
  LockedCell,
  LockedTools,
  ResolutionService,
  Resolve,
  ResolveError,
}
import histos.schedule.{CacheError, Parallelism, RemoteCache}
import histos.testrun.{ModuleTest, ModuleTestError, SessionFeed, TestFeed, TestMode, TestRun}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileFeed, CompileMemo, CompilerStarts, Compilers, GraphCompile, GraphCompileError}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Ref, Scope, UIO, ZEnvironment, ZIO, ZLayer}
import zio.stream.ZStream

/** One content store, one compiler-start count, one linker-start count, and the checkouts that share them. Not
  * [[histos.zinc.CompileReplay.processStore]]: that store is one directory for the test JVM.
  */
final class Scenario private (
    env: ZEnvironment[Resolve & ContentStore & CompilerStarts],
    tools: DependencyLock,
    linker: LinkerStarts,
    cache: Ref[RemoteCache],
):
  /** The cache the next compile, test, run, or link reads. A laptop test leaves this unconfigured. */
  def use(remote: RemoteCache): UIO[Unit] = cache.set(remote)

  /** Compiler starts that [[Checkout.compile]] counted. A test run's compiles keep their own count. */
  def starts: UIO[Int] = env.get[CompilerStarts].get

  /** Linker starts that [[Checkout.link]] counted. A restored binary does not tick. */
  def linkerStarts: UIO[Int] = linker.get

  /** The store this scenario's compiles read. */
  def store: ContentStore = env.get[ContentStore]

  /** Whether the scenario store holds a blob under `sha`. */
  def holds(sha: Sha256): IO[CacheError, Boolean] =
    env.get[ContentStore].list.map(_.exists(_.digest.value == sha.value))

  /** Delete every blob in the scenario store. A compile that still needs one after this starts the compiler. */
  def emptyStore: IO[CacheError | IoFailure, Unit] =
    env.get[ContentStore].list.flatMap(entries => ZIO.foreachDiscard(entries)(entry => Workspace.delete(entry.path)))

  /** A work tree of `build`. The lock `build` reads pins every cell with no jars, plus this repository's tools. A
    * module that names dependencies drifts from that pin. The checkout keeps its compilers and its input memo for the
    * scope, the way that checkout's server keeps them between commands.
    */
  def checkout(build: ProjectGraph): ZIO[Scope, IoFailure, Checkout] =
    for
      root <- Workspace.work("histos-scenario")
      _    <- Workspace.writeString(ResolutionService.file(root, build.resolution), DependencyLock.render(pin(build)))
      resident <- (Compilers.live(Parallelism.default.cpu) ++ CompileMemo.live).build
    yield Checkout(root, build, env ++ resident, linker, cache)

  private def pin(build: ProjectGraph): DependencyLock =
    val cells = Resolve
      .cells(build, BuildResolvers(build.resolvers))
      .map(cell => LockedCell(cell.module, cell.scalaVersion, cell.platform, Nil, Nil, Nil, cell.scope))
    DependencyLock(cells, tools = tools.tools)
end Scenario

object Scenario:
  /** The store dies with the scope. Every checkout pins the compiler tools of [[CompilerClosure.lock]]. */
  def open: ZIO[Scope & Resolve & CompilerClosure, IoFailure, Scenario] =
    for
      dir     <- Workspace.work("histos-scenario-cas")
      count   <- Ref.make(0)
      linked  <- Ref.make(0)
      resolve <- ZIO.service[Resolve]
      closure <- ZIO.service[CompilerClosure]
      cache   <- Ref.make[RemoteCache](RemoteCache.unconfigured)
    yield Scenario(
      ZEnvironment[Resolve, ContentStore, CompilerStarts](resolve, ContentStore.in(dir), CompilerStarts(count)),
      closure.lock,
      LinkerStarts(linked),
      cache,
    )

/** One work tree of one build. `compile` and `test` run it against the scenario store. `link` restores a stored native
  * binary from that store.
  */
final class Checkout private[histos] (
    val root: Path,
    build: ProjectGraph,
    env: ZEnvironment[Resolve & ContentStore & CompilerStarts & Compilers & CompileMemo],
    linker: LinkerStarts,
    cache: Ref[RemoteCache],
):
  /** The build this checkout compiles. */
  def graph: ProjectGraph = build

  def write(relative: String, text: String): IO[IoFailure, Unit] =
    Workspace.writeString(root.resolve(relative), text)

  /** `./histos clean`: this work tree's products go, the server drops its memo and compilers, and the scenario store
    * stays.
    */
  def clean: IO[IoFailure, Unit] =
    Products.wipe(root) *> env.get[CompileMemo].forget *> env.get[Compilers].drop

  /** Link `name` after it has been compiled. The work directory is created again each call, so a hit writes the binary
    * back. The bytes are that file. A miss counts toward [[Scenario.linkerStarts]].
    */
  def link(name: String): IO[LinkFault, Chunk[Byte]] =
    ZIO.fromEither(linked(name)).flatMap(linkPrepared)

  /** Every module of the build. Starts count toward [[Scenario.starts]]. */
  def compile: IO[GraphCompileError, Chunk[CompileFeed]] =
    compileDuring(_ => ZIO.unit)

  /** Compile, and run `inside` after the key is fixed and before the compiler sees the sources. */
  def compileDuring(inside: ProjectName => UIO[Unit]): IO[GraphCompileError, Chunk[CompileFeed]] =
    for
      machine <- here
      jdk     <- GraphCompile.jdk(machine)
      remote  <- cache.get
      feeds   <- GraphCompile
        .consoleCounted(root, build, Nil, jdk, machine.native, None, inside)
        .runCollect
        .provideEnvironment(env.add[RemoteCache](remote))
    yield feeds

  /** Every module's tests, on this checkout's compilers and memo, as the server's `test` runs them. A quick record is
    * written under this work tree.
    */
  def test(mode: TestMode): IO[ModuleTestError | GraphCompileError | ResolveError, Chunk[TestFeed]] =
    for
      machine  <- here
      jdk      <- GraphCompile.jdk(machine)
      runtimes <- Runtimes.read(machine, None)
      lock     <- Resolve.readLock(root)
      remote   <- cache.get
      feeds    <- ModuleTest
        .sessionOn(TestRun(root, build, Nil, jdk, runtimes, mode))
        .collect { case SessionFeed.Test(feed) => feed }
        .runCollect
        .provideLayer(
          ZLayer.succeedEnvironment(env) ++ ZLayer.succeed(remote) >+> ResolutionService.pinned(lock)
        )
    yield feeds

  /** The module's main, compiled and started, with each feed as the program produces it. */
  def run(name: String): ZStream[Any, ProgramError | GraphCompileError | ResolveError, ProgramFeed] =
    ZStream.unwrap:
      for
        machine  <- here
        jdk      <- GraphCompile.jdk(machine)
        runtimes <- Runtimes.read(machine, None)
        lock     <- Resolve.readLock(root)
        remote   <- cache.get
      yield Program
        .console(root, build, name, jdk, runtimes, None, _ => ZIO.unit, ZIO.unit)
        .provideLayer(
          ZLayer.succeedEnvironment(env) ++ ZLayer.succeed(remote) >+> ResolutionService.pinned(lock)
        )

  /** Needs `TestAspect.withLiveSystem`. */
  private def here: IO[GraphCompileError, Machine] =
    Machine.load(root).mapError(GraphCompileError.Runtime(_))

  private def linked(name: String): Either[LinkFault, LinkedModule] =
    build.projects.collectFirst { case module: Module if module.name.value == name => module } match
      case None                                                            => Left(LinkFault.Missing(name))
      case Some(module) if !module.platforms.all.contains(Platform.Native) => Left(LinkFault.NotNative(name))
      case Some(module) if module.application.isEmpty                      => Left(LinkFault.NoMain(name))
      case Some(module) if module.clang == ClangPin.Unused                 => Left(LinkFault.NoClang(name))
      case Some(module)                                                    =>
        (module.application, module.clang) match
          case (Some(main), ClangPin.Major(major)) =>
            histos.pack.MainClass.from(main.value) match
              case Left(error)   => Left(LinkFault.BadMain(error.message))
              case Right(binary) => Right(LinkedModule(module, binary, major))
          case _ => Left(LinkFault.NoMain(name))

  private def linkPrepared(asked: LinkedModule): IO[LinkFault, Chunk[Byte]] =
    val LinkedModule(module, binary, major) = asked
    val version                             = module.versions.first
    val classes                             = GraphCompile.classesDir(root, module.name, version, Platform.Native)
    val work                                = root.resolve(".histos").resolve("link-proof")
    for
      machine   <- here.mapError(LinkFault.Graph(_))
      jdk       <- GraphCompile.jdk(machine).mapError(LinkFault.Graph(_))
      runtimes  <- Runtimes.read(machine, None)
      lock      <- Resolve.readLock(root).mapError(LinkFault.Resolve(_))
      tools     <- LockedTools.make(lock, build).provide(ZLayer.succeed(env.get[Resolve]))
      toolchain <- ZIO
        .fromEither(
          NativeToolchain.of(module.nativeVersion.getOrElse(tools.catalog.native.revision), runtimes.native)
        )
        .mapError(LinkFault.Toolchain(_))
      _      <- Workspace.recreate(work).mapError(LinkFault.Work(_))
      remote <- cache.get
      path   <- NativeLink
        .linkClasspath(
          NativeLinkRequest(major, toolchain, runtimes.env, work, jdk, binary, version),
          List(classes),
        )
        .provideEnvironment(
          ZEnvironment[LockedTools, ContentStore, LinkerStarts, RemoteCache](
            tools,
            env.get[ContentStore],
            linker,
            remote,
          )
        )
        .mapError(LinkFault.Native(_))
      bytes <- ZIO
        .attemptBlocking(Files.readAllBytes(path))
        .mapBoth(_ => LinkFault.Work(IoFailure(path, IoFault.Read)), Chunk.fromArray)
    yield bytes

  private final case class LinkedModule(module: Module, binary: histos.pack.MainClass, major: ClangMajor)
end Checkout

/** Why [[Checkout.link]] returned no binary. */
enum LinkFault:
  case Missing(name: String)
  case NotNative(name: String)
  case NoMain(name: String)
  case NoClang(name: String)
  case BadMain(text: String)
  case Graph(error: GraphCompileError)
  case Resolve(error: ResolveError)
  case Native(error: NativeError)
  case Toolchain(error: NativeToolchainError)
  case Work(failure: IoFailure)

  def message: String = this match
    case Missing(name)    => s"'$name' is not in the build"
    case NotNative(name)  => s"'$name' is not a Native module"
    case NoMain(name)     => s"'$name' has no main class to link"
    case NoClang(name)    => s"'$name' does not name a clang major"
    case BadMain(text)    => text
    case Graph(error)     => error.message
    case Resolve(error)   => error.message
    case Native(error)    => error.message
    case Toolchain(error) => error.message
    case Work(failure)    => failure.message
end LinkFault
