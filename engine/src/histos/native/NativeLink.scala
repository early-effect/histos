package histos.native

import histos.cache.{ContentStore, LinkReplay, LinkerStarts, ProductCache}
import histos.schedule.{CacheError, RemoteCache, RemoteError}
import histos.jdk.JdkRuntime
import histos.jdk.{Sha256, Sha256Digest}
import histos.resolve.LockedTools
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileOutput,
  CompileRequest,
  CompileReplay,
  CompileToolchain,
  JvmCompile,
  ScalacFlags,
}
import histos.{Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.util.Try
import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

object NativeKey:
  /** Identity includes ordered physical classpath content and the declared resource selection. */
  def of(
      version: ScalaVersion,
      clang: ClangVersion,
      target: ClangTarget,
      main: MainClass,
      products: List[Sha256],
      artifacts: List[Sha256],
      optimize: Boolean,
      libraries: Set[histos.NativeLibrary] = Set.empty,
  ): Sha256 =
    val mode   = if optimize then "optimize" else "no-optimize"
    val linked = libraries.toList.map(_.token).sorted.mkString(",")
    val text   =
      s"""native-link-2
         |${version.value}
         |${clang.value}
         |${target.value}
         |${main.binary}
         |stubs
         |$mode
         |libraries
         |$linked
         |products
         |${products.map(_.value).mkString("\n")}
         |${artifacts.map(_.value).mkString("\n")}
         |""".stripMargin
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

final case class Linked(binary: Path, key: Sha256)

object NativeProject:
  def declaration(graph: ProjectGraph): NativeDeclaration =
    val pins = graph.projects.flatMap:
      case module: histos.Module if module.platforms.all.contains(Platform.Native) =>
        val name  = module.name
        val clang = module.clang
        clang match
          case histos.ClangPin.Major(major) => List(name -> Some(major))
          case histos.ClangPin.Unused       => List(name -> None)
      case _ => Nil
    pins match
      case Nil                         => NativeDeclaration.NotNative
      case (name, None) :: _           => NativeDeclaration.Missing(name)
      case (name, Some(major)) :: rest =>
        rest.foldLeft[NativeDeclaration](NativeDeclaration.Major(major)):
          case (NativeDeclaration.Major(chosen), (_, Some(found))) if found == chosen =>
            NativeDeclaration.Major(chosen)
          case (NativeDeclaration.Major(_), (other, Some(_))) =>
            NativeDeclaration.Disagree(name, other)
          case (NativeDeclaration.Major(_), (missing, None)) =>
            NativeDeclaration.Missing(missing)
          case (done, _) => done

/** Clang, the toolchain, and where the link runs. The sources or the classpath are the other argument.
  */
final case class NativeLinkRequest(
    major: histos.ClangMajor,
    toolchain: NativeToolchain,
    env: Map[String, String],
    work: Path,
    jdk: JdkRuntime,
    main: MainClass,
    version: ScalaVersion,
    optimize: Boolean = false,
    libraries: Set[histos.NativeLibrary] = Set.empty,
    resources: List[histos.ResourceName] = Nil,
)

object NativeLink:
  /** `env` is the machine's: clang is found on its `PATH`, and the linker child gets that `PATH`.
    */
  def link(
      request: NativeLinkRequest,
      sourcesRoot: Path,
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, NativeError, Linked] =
    val NativeLinkRequest(major, toolchain, env, work, jdk, main, version, optimize, libraries, resources) =
      request
    for
      view <- ZIO.service[LockedTools]
      libs <- NativeClasspath
        .fetch(toolchain, Some(version))
        .mapError(NativeError.Fetch(_))
      clang   <- ClangProbe.resolve(major, env).mapError(NativeError.Clang(_))
      sources <- histos.zinc.Sources
        .select(sourcesRoot, version, Platform.Native)
        .mapError(NativeError.Compile(_))
      compiled <- compile(request, sources, libs, sourcesRoot).mapError(NativeError.Compile(_))
      prints   <- toolchainPrints(view, toolchain, libs)
      classpath = compiled.classes :: libs.libraries
      products <- inputDigests(classpath)
      _        <- validateResources(classpath, resources)
      resourceIdentity = Sha256Digest.of(
        Chunk.fromArray(resources.map(_.value).distinct.sorted.mkString("\n").getBytes(StandardCharsets.UTF_8))
      )
      key = NativeKey.of(
        version,
        clang.version,
        clang.target,
        main,
        products,
        prints :+ resourceIdentity,
        optimize,
        libraries,
      )
      binary <- restoreOr(
        key,
        work,
        invoke(
          clang,
          main,
          classpath,
          work,
          jdk,
          toolchain,
          env,
          optimize,
          libraries,
          resources,
        ).tap(_ => unchanged(classpath, products)),
      )
      _ <- unchanged(classpath, products)
    yield Linked(binary, key)

  /** Link `.nir` that is already compiled. The native libraries are added after `classpath`, and any other copy of
    * those libraries is dropped first.
    */
  def linkClasspath(
      request: NativeLinkRequest,
      classpath: List[Path],
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, NativeError, Path] =
    val NativeLinkRequest(major, toolchain, env, work, jdk, main, version, optimize, libraries, resources) =
      request
    for
      view <- ZIO.service[LockedTools]
      libs <- NativeClasspath
        .fetch(toolchain, Some(version))
        .mapError(NativeError.Fetch(_))
      clang  <- ClangProbe.resolve(major, env).mapError(NativeError.Clang(_))
      prints <- toolchainPrints(view, toolchain, libs)
      actual = withoutLibraries(classpath, libs.libraries) ++ libs.libraries
      products <- inputDigests(actual)
      _        <- validateResources(actual, resources)
      resourceIdentity = Sha256Digest.of(
        Chunk.fromArray(resources.map(_.value).distinct.sorted.mkString("\n").getBytes(StandardCharsets.UTF_8))
      )
      key = NativeKey.of(
        version,
        clang.version,
        clang.target,
        main,
        products,
        prints :+ resourceIdentity,
        optimize,
        libraries,
      )
      binary <- restoreOr(
        key,
        work,
        invoke(
          clang,
          main,
          actual,
          work,
          jdk,
          toolchain,
          env,
          optimize,
          libraries,
          resources,
        ).tap(_ => unchanged(actual, products)),
      )
      _ <- unchanged(actual, products)
    yield binary

  /** A stored key is restored into `work` and the linker does not start. A miss ticks, links, then stores the binary.
    */
  private def restoreOr(
      key: Sha256,
      work: Path,
      link: ZIO[LockedTools & LinkerStarts, NativeError, Path],
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, NativeError, Path] =
    for
      store  <- ZIO.service[ContentStore]
      remote <- ZIO.service[RemoteCache]
      _      <- ProductCache.stage(remote, store, key).mapError(carried)
      hit    <- LinkReplay.load(store, key, work).mapError(stored)
      binary <- hit match
        case Some(path) => ZIO.succeed(path)
        case None       =>
          link.tap: path =>
            LinkReplay.save(store, key, path).mapError(stored) *>
              ProductCache.publish(remote, store, key).mapError(carried)
    yield binary

  private def stored(error: CacheError): NativeError =
    NativeError.Compile(CompileError.Stored(error))

  private def carried(error: RemoteError | CacheError): NativeError =
    error match
      case found: RemoteError => NativeError.Compile(CompileError.Remote(found))
      case found: CacheError  => NativeError.Compile(CompileError.Stored(found))

  private def toolchainPrints(
      view: LockedTools,
      toolchain: NativeToolchain,
      libs: NativeClasspath,
  ): ZIO[LockedTools, NativeError, List[Sha256]] =
    for
      linker <- view
        .selected(NativeRequests.linker(toolchain, view.catalog, view.repositories))
        .mapError(error => NativeError.Fetch(NativeFetch.Resolved(error)))
      bridges <- ZIO
        .foreach(
          histos.zinc.CompilerRequests.supporting(
            List(view.catalog.native.bridgeScala),
            view.catalog,
            view.repositories,
          )
        )(view.selected)
        .mapError(error => NativeError.Fetch(NativeFetch.Resolved(error)))
    yield List(libs.input.fingerprint, linker.fingerprint) ++ bridges.map(_.fingerprint)

  /** Ordered physical inputs include NIR, helper bodies, jars and declared resource bytes, not just ABI metadata. */
  private[histos] def inputDigests(classpath: List[Path]): IO[NativeError, List[Sha256]] =
    ZIO.foreach(classpath): path =>
      val checked = ZIO.attemptBlocking:
        if Files.isSymbolicLink(path) then Some(path)
        else if Files.isDirectory(path) then
          val walk = Files.walk(path)
          try walk.iterator().asScala.find(Files.isSymbolicLink(_))
          finally walk.close()
        else None
      checked
        .mapError(_ =>
          NativeError.Compile(CompileError.Io(histos.workspace.IoFailure(path, histos.workspace.IoFault.Walk)))
        )
        .flatMap:
          case Some(link) => ZIO.fail(NativeError.InputLink(link))
          case None       => histos.zinc.CellCapture.implementation(path).mapError(NativeError.Compile(_))

  private def validateResources(classpath: List[Path], resources: List[histos.ResourceName]): IO[NativeError, Unit] =
    ZIO.foreachDiscard(resources.distinct): name =>
      ZIO
        .foreach(classpath): path =>
          ZIO
            .attemptBlocking:
              if Files.isDirectory(path) then Files.isRegularFile(path.resolve(name.value))
              else
                val zip = new ZipFile(path.toFile)
                try Option(zip.getEntry(name.value)).exists(entry => !entry.isDirectory)
                finally zip.close()
            .mapError(_ =>
              NativeError.Compile(CompileError.Io(histos.workspace.IoFailure(path, histos.workspace.IoFault.Read)))
            )
        .flatMap(found => if found.contains(true) then ZIO.unit else ZIO.fail(NativeError.MissingResource(name)))

  private def unchanged(classpath: List[Path], expected: List[Sha256]): IO[NativeError, Unit] =
    inputDigests(classpath).flatMap: observed =>
      if observed == expected then ZIO.unit else ZIO.fail(NativeError.InputsChanged(classpath))

  private def compile(
      link: NativeLinkRequest,
      sources: List[Path],
      libs: NativeClasspath,
      checkout: Path,
  ): ZIO[LockedTools & RemoteCache, CompileError, CompileOutput] =
    val request = CompileRequest(
      CompileToolchain(
        link.version,
        Platform.Native,
        link.jdk,
        ScalacFlags.empty,
      ),
      sources,
      libs.libraries,
      link.work.resolve("classes"),
      link.work.resolve("analysis"),
      Nil,
      Nil,
      checkout = checkout,
    )
    CompileReplay.isolated(
      JvmCompile.compilePrepared(request, histos.zinc.CellPlatform.Native(link.toolchain), List(libs.plugin))
    )

  /** The toolchain appends its own native libraries. A resolved jar of the same library, at any revision, is dropped.
    * The kit does not keep the original file name, so a jar whose manifest names the library is the same jar.
    */
  private[histos] def withoutLibraries(
      classpath: List[Path],
      libraries: List[Path],
  ): List[Path] =
    val names = libraries.flatMap(libraryName).toSet
    classpath.filterNot(path => sameLibrary(path, names))

  /** Drop the toolchain libraries before a kit is written. The link fetches the nightly and appends it.
    */
  private[histos] def withoutStandardLibraries(classpath: List[Path]): List[Path] =
    classpath.filterNot(isStandardLibrary)

  private def sameLibrary(path: Path, names: Set[String]): Boolean =
    Option(path.getFileName).exists(file => names.exists(name => file.toString.startsWith(s"$name-"))) ||
      isStandardLibrary(path)

  private def isStandardLibrary(path: Path): Boolean =
    manifestTitle(path).exists(NativeComponent.standardLibrary)

  private def manifestTitle(path: Path): Option[String] =
    val file = Option(path.getFileName).map(_.toString).getOrElse("")
    if !file.endsWith(".jar") || !Files.isRegularFile(path) then None
    else
      Try {
        val zip = new ZipFile(path.toFile)
        try
          Option(zip.getEntry("META-INF/MANIFEST.MF")) match
            case None        => None
            case Some(entry) =>
              val input = zip.getInputStream(entry)
              val text  =
                try new String(input.readAllBytes(), StandardCharsets.UTF_8)
                finally input.close()
              text.linesIterator.map(_.trim).collectFirst {
                case line if line.startsWith("Implementation-Title:") =>
                  line.drop("Implementation-Title:".length).trim
              }
        finally zip.close()
      }.toOption.flatten

  /** `nativelib_native0.5_3-0.5.12.jar` and the snapshot jar are one library. The revision starts at the first hyphen.
    */
  private def libraryName(path: Path): Option[String] =
    Option(path.getFileName).map(_.toString) match
      case Some(file) if file.endsWith(".jar") =>
        val base = file.stripSuffix(".jar")
        val name = base.takeWhile(_ != '-')
        if name.nonEmpty && name.length < base.length then Some(name) else None
      case _ => None

  /** The linker JVM is a fresh environment. Linux clang finds `ld` on `PATH`. An empty environment makes that exec
    * fail.
    */
  private[histos] def linkerEnv(env: Map[String, String]): Map[String, String] =
    env.get("PATH").filter(_.nonEmpty) match
      case Some(path) => Map("PATH" -> path)
      case None       => Map.empty

  /** Compiles the bridge against this revision's tools, then links in a child JVM.
    */
  private def invoke(
      clang: Clang,
      main: MainClass,
      classpath: List[Path],
      work: Path,
      jdk: JdkRuntime,
      toolchain: NativeToolchain,
      env: Map[String, String],
      optimize: Boolean,
      libraries: Set[histos.NativeLibrary],
      resources: List[histos.ResourceName],
  ): ZIO[LockedTools & LinkerStarts, NativeError, Path] =
    val bridgeRoot = work.resolve("bridge").resolve(toolchain.revision.value)
    val classes    = bridgeRoot.resolve("classes")
    val source     = bridgeRoot
      .resolve("src")
      .resolve("subprocess")
      .resolve("NativeBridge.scala")
    val child = work.resolve("child")
    for
      view <- ZIO.service[LockedTools]
      jars <- NativeClasspath
        .bridgeJars(toolchain)
        .mapError(NativeError.Fetch(_))
      os = Option(System.getProperty("os.name"))
      opensslHome <- if NativeFlags.usesOpenssl(libraries) then HomebrewOpenssl.discover(os.getOrElse("")) else ZIO.none
      _           <- write(
        source,
        NativeBridge.source(
          "hello",
          main.binary,
          clang.clang,
          clang.clangpp,
          classpath,
          optimize,
          libraries,
          os,
          opensslHome,
          resources,
        ),
      )
      _ <- JvmCompile
        .compile(
          CompileRequest(
            CompileToolchain(
              view.catalog.native.bridgeScala,
              Platform.Jvm,
              jdk,
              ScalacFlags.empty,
            ),
            List(source),
            jars,
            classes,
            bridgeRoot.resolve("analysis"),
            Nil,
            Nil,
            checkout = bridgeRoot,
          )
        )
        .mapError(NativeError.Compile(_))
      _      <- emptyDir(child)
      bridge <- ZIO
        .fromEither(MainClass.from(NativeBridge.MainName))
        .mapError(error => NativeError.Link(LinkError.Linker(error.message)))
      _      <- LinkerStarts.tick
      binary <- ChildProcess
        .run(
          ChildRequest(
            Platform.Jvm,
            jdk.home.resolve("bin").resolve("java"),
            ChildClasspath(classes, jars),
            bridge,
            child,
            linkerEnv(env),
          )
        )
        .mapError(error => NativeError.Link(LinkError.Linker(error.message)))
      linked <- binary.exit.value match
        case 0 => readBinary(child.resolve("binary.path"))
        case _ =>
          ZIO.fail(
            NativeError.Link(
              LinkError.Linker(shown(binary.out.text, binary.err.text))
            )
          )
    yield linked

  private def shown(out: String, err: String): String =
    val text = List(out, err).filter(_.nonEmpty).mkString("\n")
    if text.isEmpty then "the native linker failed" else text

  private def readBinary(path: Path): ZIO[LockedTools, NativeError, Path] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Right(Path.of(Files.readString(path).trim))
        else
          Left(
            NativeError.Link(
              LinkError.Linker("the native bridge did not write a binary")
            )
          )
      }
      .mapError(_ => NativeError.Link(LinkError.Linker(s"could not read $path")))
      .flatMap(ZIO.fromEither)

  private def write(
      path: Path,
      text: String,
  ): ZIO[LockedTools, NativeError, Unit] =
    Workspace
      .writeString(path, text)
      .mapError(_ => NativeError.Link(LinkError.Linker(s"could not write $path")))

  private def emptyDir(path: Path): ZIO[LockedTools, NativeError, Unit] =
    Workspace
      .recreate(path)
      .mapError(_ => NativeError.Link(LinkError.Linker(s"could not prepare $path")))
      .unit
end NativeLink
