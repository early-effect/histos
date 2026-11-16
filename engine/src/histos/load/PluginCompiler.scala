package histos.load

import histos.cache.ContentStore
import histos.jdk.{JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.plugin.{
  ArtifactEntry,
  ArtifactFormat,
  ArtifactIdentity,
  ArtifactPath,
  ClosureArtifact,
  ClosureRole,
  ContentHash,
  Digests,
  FrozenEnv,
  FrozenFile,
  HostRole,
  HostSubstitution,
  RepoPath,
  SealedArtifactManifest,
  SelectedInput,
  SemanticSetting,
  SourceKind,
  SourceProvenance,
  SourceSet,
  StageAssurance,
  StageContract,
}
import histos.process.{ChildEvent, ChildProcess, ProcessExit}
import histos.schedule.{CacheError, Digest, RemoteCache}
import histos.toolchain.{HostCompilerError, HostCompilerSession, HostToolRole, MaterializedTool, RuntimeIdentity}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{
  CompilationSnapshot,
  CompileError,
  CompileInputs,
  CompileRequest,
  CompileToolchain,
  CompilerStarts,
  Compilers,
  JvmCompile,
}
import histos.{Platform, ProjectName, ScalacFlags}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Promise, Ref, UIO, ZIO, ZLayer}

/** One plugin compile. The session already holds the compiler, bridge, and support jars. */
final case class PluginCompileJob(
    module: ProjectName,
    session: HostCompilerSession,
    checkout: Path,
    recipe: Path,
    sources: List[Path],
    classpath: List[Path] = Nil,
    apiJars: List[Path] = Nil,
    classes: Path,
    analysis: Path,
    stages: List[StageContract] = Nil,
    helpers: List[ArtifactIdentity] = Nil,
    settings: List[SemanticSetting] = Nil,
    offline: Boolean = false,
    caller: Map[String, String] = Map.empty,
    probe: UIO[Unit] = ZIO.unit,
)

final case class PluginCompileResult(
    snapshot: CompilationSnapshot,
    key: Sha256,
)

/** One compile in flight per key. The owner stores the class bytes. A waiter unpacks those bytes itself. */
final case class PluginFlights(active: Ref[Map[String, Promise[PluginCompileError, Chunk[Byte]]]]):
  def run[R](
      key: String,
      compile: ZIO[R, PluginCompileError, Chunk[Byte]],
  ): ZIO[R, PluginCompileError, Chunk[Byte]] =
    Promise.make[PluginCompileError, Chunk[Byte]].flatMap { created =>
      active
        .modify { current =>
          current.get(key) match
            case Some(waiting) => (Left(waiting), current)
            case None          => (Right(created), current.updated(key, created))
        }
        .flatMap:
          case Left(waiting) => waiting.await
          case Right(owned)  =>
            ZIO.uninterruptibleMask: restore =>
              restore(compile).exit.flatMap: exit =>
                owned.done(exit) *> active.update(_.removed(key)) *> exit
    }

object PluginFlights:
  val live: ZLayer[Any, Nothing, PluginFlights] =
    ZLayer.fromZIO(Ref.make(Map.empty[String, Promise[PluginCompileError, Chunk[Byte]]]).map(PluginFlights(_)))

object PluginCompiler:
  def compile(
      job: PluginCompileJob
  ): ZIO[
    ContentStore & CompilerStarts & Compilers & RemoteCache & PluginFlights,
    PluginCompileError,
    PluginCompileResult,
  ] =
    for
      _        <- ZIO.fromEither(KnownStageRegistry.admit(job.stages, job.offline))
      selected <- ZIO.foreach(job.sources)(selectedInput(job.checkout, _))
      apis     <- ZIO.foreach(job.apiJars)(hashFile)
      closed   <- ZIO.foreach(job.classpath)(closureOf)
      key = identity(job, selected, apis, closed.map(_._1))
      _       <- revalidate(job)
      bytes   <- ZIO.serviceWithZIO[PluginFlights](_.run(key.value, produce(job, key, apis)))
      _       <- unpack(job.classes, bytes)
      outputs <- seal(job.classes)
    yield PluginCompileResult(snapshot(job, selected, apis, closed.map(_._2), outputs), key)

  /** In-process compile with jars the caller already checked. */
  private[histos] def supplied(
      request: CompileRequest,
      runtime: List[Path],
      compiler: List[Path],
      bridge: Path,
      support: List[Path],
      key: Sha256,
  ): ZIO[ContentStore & CompilerStarts & Compilers & RemoteCache, PluginCompileError, Unit] =
    for
      sources <- JvmCompile.sourceDigest(request.checkout, request.sources).mapError(PluginCompileError.Compile(_))
      rows = CompileInputs.declared(request, sources)
      _ <- JvmCompile
        .compileSupplied(request, runtime, compiler, bridge, support, rows, key)
        .mapError(PluginCompileError.Compile(_))
    yield ()

  private def produce(
      job: PluginCompileJob,
      key: Sha256,
      apis: List[Sha256],
  ): ZIO[ContentStore & CompilerStarts & Compilers & RemoteCache, PluginCompileError, Chunk[Byte]] =
    cached(key).flatMap:
      case Some(bytes) => ZIO.succeed(bytes)
      case None        =>
        job.probe *> reset(job.classes) *>
          (if job.stages.exists(_.assurance == StageAssurance.ProducerDeclared) then worker(job, key, apis)
           else local(job, key, apis))

  private def local(
      job: PluginCompileJob,
      key: Sha256,
      apis: List[Sha256],
  ): ZIO[ContentStore & CompilerStarts & Compilers & RemoteCache, PluginCompileError, Chunk[Byte]] =
    for
      tools <- ZIO.fromEither(split(job.session))
      request = compileRequest(job, apis)
      _     <- supplied(request, tools.runtime, tools.compiler, tools.bridge, tools.support, key)
      bytes <- zip(job.classes)
      _     <- keep(key, bytes)
    yield bytes

  private def worker(
      job: PluginCompileJob,
      key: Sha256,
      apis: List[Sha256],
  ): ZIO[ContentStore & CompilerStarts & Compilers & RemoteCache, PluginCompileError, Chunk[Byte]] =
    for
      _      <- CompilerStarts.tick
      tools  <- ZIO.fromEither(split(job.session))
      java   <- javaHome
      cp     <- classPath
      path   <- PluginCompileWorker.write(job, tools.compiler, tools.bridge, tools.support, tools.runtime, apis)
      events <- ZIO
        .withSystem(zio.System.SystemLive)(
          ChildProcess
            .command(workerArgv(java, cp, path), job.recipe, workerEnv(job))
            .runCollect
        )
        .mapError(PluginCompileError.Child(_))
      _     <- finished(path, events)
      bytes <- zip(job.classes)
      _     <- keep(key, bytes)
    yield bytes

  private def workerArgv(java: Path, classpath: String, request: Path): List[String] =
    List(
      java.toString,
      histos.jdk.Toolchain.NativeAccess,
      histos.jdk.Toolchain.LazyVals,
      "-cp",
      classpath,
      "histos.load.PluginCompileWorker",
      request.toString,
    )

  /** Frozen public bindings, then the hashed bootstrap variable. The caller map is not copied. */
  private def workerEnv(job: PluginCompileJob): Map[String, String] =
    val bound = job.stages
      .flatMap(_.inputs.env)
      .collect:
        case FrozenEnv.Present(key, value) => key.value -> value
    bound.toMap + ("HISTOS_WORKER" -> "1")

  private def finished(request: Path, events: Chunk[ChildEvent]): ZIO[Any, PluginCompileError, Unit] =
    val exit = events.collect { case ChildEvent.Exited(code) => code }.lastOption
    val err  = events.collect { case ChildEvent.Err(text) => text }.mkString
    val out  = events.collect { case ChildEvent.Out(text) => text }.mkString
    val code = exit.getOrElse(ProcessExit(1)).value
    readText(PluginCompileWorker.resultFile(request)).flatMap: body =>
      body.split("\n", 2).toList match
        case "ok" :: _ if code == 0 => ZIO.unit
        case "failed" :: detail     =>
          ZIO.fail(PluginCompileError.Worker(code, (detail :: out :: err :: Nil).mkString))
        case _ =>
          ZIO.fail(PluginCompileError.Worker(code, s"$body\n$out\n$err"))

  private def identity(
      job: PluginCompileJob,
      selected: List[SelectedInput],
      apis: List[Sha256],
      classpath: List[Sha256],
  ): Sha256 =
    PluginCompileKey.of(
      job.session.manifest.scalaVersion,
      job.session.jdk,
      ScalacFlags.empty.canonical,
      selected,
      job.settings,
      job.stages,
      job.helpers,
      classpath,
      apis,
      job.session.tools.map(_.entry.digest),
      job.session.fingerprint,
    )

  private def compileRequest(job: PluginCompileJob, apis: List[Sha256]): CompileRequest =
    CompileRequest(
      CompileToolchain(job.session.manifest.scalaVersion, Platform.Jvm, jdkRuntime(job.session), ScalacFlags.empty),
      job.sources,
      job.classpath ++ job.apiJars,
      job.classes,
      job.analysis,
      Nil,
      Nil,
      checkout = job.checkout,
      apiBytes = apis,
    )

  private def jdkRuntime(session: HostCompilerSession): JdkRuntime =
    JdkRuntime(
      session.jdkHome,
      session.jdk,
      RuntimeIdentity.Local(session.jdkHome, Sha256Digest.of(Chunk.fromArray(session.jdk.value.getBytes))),
    )

  private final case class Split(compiler: List[Path], bridge: Path, support: List[Path], runtime: List[Path])

  private def split(session: HostCompilerSession): Either[PluginCompileError, Split] =
    def jars(role: HostToolRole): List[MaterializedTool] =
      session.tools.filter(_.entry.role == role)
    jars(HostToolRole.Bridge) match
      case tool :: Nil =>
        val library = jars(HostToolRole.Support).filter(_.entry.coordinate.name.startsWith("scala3-library"))
        val support = jars(HostToolRole.Support).filterNot(_.entry.coordinate.name.startsWith("scala3-library"))
        Right(
          Split(
            jars(HostToolRole.Compiler).map(_.path),
            tool.path,
            support.map(_.path),
            jars(HostToolRole.Runtime).map(_.path) ++ library.map(_.path),
          )
        )
      case _ => Left(PluginCompileError.Host(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))

  private def selectedInput(checkout: Path, path: Path): ZIO[Any, PluginCompileError, SelectedInput] =
    relative(checkout, path).flatMap: rel =>
      ZIO
        .fromEither(RepoPath.from(rel))
        .mapError(PluginCompileError.Path(_))
        .flatMap { repo =>
          readBytes(path).map: bytes =>
            SelectedInput(
              repo,
              kindOf(rel),
              SourceSet.Main,
              SourceProvenance.Unmanaged,
              Digests.content(bytes),
              bytes.length.toLong,
            )
        }

  private def kindOf(relative: String): SourceKind =
    if relative.endsWith(".scala") then SourceKind.Scala
    else if relative.endsWith(".java") then SourceKind.Java
    else SourceKind.Resource

  private def relative(checkout: Path, path: Path): ZIO[Any, PluginCompileError, String] =
    ZIO
      .attemptBlocking:
        val root = checkout.toAbsolutePath.normalize
        val file = path.toAbsolutePath.normalize
        if file.startsWith(root) then Some(root.relativize(file).toString.replace('\\', '/'))
        else None
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Stat)))
      .flatMap:
        case Some(rel) => ZIO.succeed(rel)
        case None      => ZIO.fail(PluginCompileError.Compile(CompileError.Outside(path, checkout)))

  private def revalidate(job: PluginCompileJob): ZIO[Any, PluginCompileError, Unit] =
    val files = job.stages.flatMap(stage => stage.inputs.files ++ stage.inputs.directories.flatMap(_.files))
    ZIO.foreachDiscard(files)(frozen => matches(job.recipe, frozen))

  private def matches(recipe: Path, frozen: FrozenFile): ZIO[Any, PluginCompileError, Unit] =
    val path = recipe.resolve(frozen.path.value)
    readBytes(path).flatMap: bytes =>
      val actual = Digests.content(bytes)
      if actual == frozen.digest && bytes.length.toLong == frozen.length then ZIO.unit
      else ZIO.fail(PluginCompileError.InputChanged(frozen.path))

  private def snapshot(
      job: PluginCompileJob,
      selected: List[SelectedInput],
      apis: List[Sha256],
      closure: List[ClosureArtifact],
      outputs: SealedArtifactManifest,
  ): CompilationSnapshot =
    val (resources, sources) = selected.partition(_.kind == SourceKind.Resource)
    val runtime              = identities(job.session, HostToolRole.Runtime)
    val compiler             = job.session.tools.filter(_.entry.role != HostToolRole.Runtime).map(identityOf)
    CompilationSnapshot(
      job.module,
      job.session.manifest.scalaVersion,
      Platform.Jvm,
      sources,
      resources,
      job.settings,
      job.stages,
      closure,
      job.helpers,
      apis.map(hash =>
        HostSubstitution(HostRole.Api, ArtifactIdentity(ArtifactFormat.Jar, ContentHash.trusted(hash.value)))
      ),
      compiler,
      runtime,
      job.session.jdk,
      outputs,
      job.session.knownStages,
    )

  private def identities(session: HostCompilerSession, role: HostToolRole): List[ArtifactIdentity] =
    session.tools.filter(_.entry.role == role).map(identityOf)

  private def identityOf(tool: MaterializedTool): ArtifactIdentity =
    ArtifactIdentity(ArtifactFormat.Jar, tool.entry.digest)

  /** One compile-classpath member. A directory is the classes tree, not a jar. */
  private def closureOf(path: Path): ZIO[Any, PluginCompileError, (Sha256, ClosureArtifact)] =
    ZIO
      .attemptBlocking(Files.isDirectory(path))
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Stat)))
      .flatMap: directory =>
        val hashed = if directory then hashDirectory(path) else hashFile(path)
        hashed.map: hash =>
          val format   = if directory then ArtifactFormat.Directory else ArtifactFormat.Jar
          val artifact = ClosureArtifact(
            ArtifactIdentity(format, ContentHash.trusted(hash.value)),
            ClosureRole.Compile,
          )
          (hash, artifact)

  /** Content of a classes directory. The absolute root is not an input. */
  private def hashDirectory(path: Path): ZIO[Any, PluginCompileError, Sha256] =
    val names = ZIO
      .attemptBlocking {
        val root   = path.toAbsolutePath.normalize
        val stream = Files.walk(root)
        try
          stream.iterator.asScala.filter(Files.isRegularFile(_)).map(root.relativize).toList.sortBy(_.toString)
        finally stream.close()
      }
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Read)))
    names.flatMap: listed =>
      ZIO
        .foreach(listed)(name => hashFile(path.resolve(name)).map(hash => s"${slash(name)} ${hash.value}"))
        .map: lines =>
          Sha256Digest.of(Chunk.fromArray(lines.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  private def slash(path: Path): String =
    path.toString.replace('\\', '/')

  private def hashFile(path: Path): ZIO[Any, PluginCompileError, Sha256] =
    readBytes(path).map(bytes => Sha256Digest.of(Chunk.fromArray(bytes)))

  private def readBytes(path: Path): ZIO[Any, PluginCompileError, Array[Byte]] =
    ZIO
      .attemptBlocking(if Files.isRegularFile(path) then Some(Files.readAllBytes(path)) else None)
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Read)))
      .flatMap:
        case Some(bytes) => ZIO.succeed(bytes)
        case None        => ZIO.fail(PluginCompileError.Io(IoFailure(path, IoFault.Stat)))

  private def archiveId(key: Sha256): Either[PluginCompileError, Digest] =
    val hash = Sha256Digest.of(Chunk.fromArray(s"plugin-classes\n${key.value}".getBytes))
    Digest.from(hash.value).toRight(PluginCompileError.Io(IoFailure(Path.of("plugin-classes"), IoFault.Read)))

  private def cached(key: Sha256): ZIO[ContentStore, PluginCompileError, Option[Chunk[Byte]]] =
    ZIO
      .fromEither(archiveId(key))
      .flatMap: id =>
        ZIO.serviceWithZIO[ContentStore](_.read(id).mapError(stored))

  private def keep(key: Sha256, bytes: Chunk[Byte]): ZIO[ContentStore, PluginCompileError, Unit] =
    for
      id  <- ZIO.fromEither(archiveId(key))
      now <- ZIO.clockWith(_.instant)
      _   <- ZIO.serviceWithZIO[ContentStore](_.put(id, bytes, now).mapError(stored))
    yield ()

  private def stored(error: CacheError): PluginCompileError =
    error match
      case CacheError.Io(failure) => PluginCompileError.Io(failure)
      case CacheError.Corrupt(id) => PluginCompileError.Io(IoFailure(Path.of(id.value), IoFault.Read))

  private def seal(root: Path): ZIO[Any, PluginCompileError, SealedArtifactManifest] =
    ZIO
      .attemptBlocking(sealNow(root))
      .mapError(_ => PluginCompileError.Io(IoFailure(root, IoFault.Walk)))
      .flatMap(ZIO.fromEither)

  private def sealNow(root: Path): Either[PluginCompileError, SealedArtifactManifest] =
    if !Files.exists(root) then SealedArtifactManifest.from(Nil).left.map(PluginCompileError.Seal(_))
    else
      val walk = Files.walk(root)
      try
        val found = walk.iterator.asScala.flatMap(entryOf(root, _)).toList
        found
          .foldLeft[Either[PluginCompileError, List[ArtifactEntry]]](Right(Nil)):
            case (Left(error), _)           => Left(error)
            case (Right(_), Left(error))    => Left(error)
            case (Right(acc), Right(entry)) => Right(acc :+ entry)
          .flatMap(entries => SealedArtifactManifest.from(entries).left.map(PluginCompileError.Seal(_)))
      finally walk.close()

  private def entryOf(root: Path, path: Path): Option[Either[PluginCompileError, ArtifactEntry]] =
    if !Files.isRegularFile(path) then None
    else
      val name = root.relativize(path).toString.replace('\\', '/')
      Some(
        ArtifactPath.from(name) match
          case Left(_)      => Left(PluginCompileError.Io(IoFailure(path, IoFault.Read)))
          case Right(entry) =>
            val bytes = Files.readAllBytes(path)
            Right(ArtifactEntry(entry, bytes.length.toLong, Digests.content(bytes)))
      )

  private def zip(root: Path): ZIO[Any, PluginCompileError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(zipNow(root))
      .mapError(_ => PluginCompileError.Io(IoFailure(root, IoFault.Read)))
      .flatMap(packed => ZIO.fromEither(packed.left.map(PluginCompileError.Io(_))))

  private def zipNow(root: Path): Either[IoFailure, Chunk[Byte]] =
    val out = new ByteArrayOutputStream
    val zip = new ZipOutputStream(out)
    if !Files.isDirectory(root) then
      zip.close()
      Right(Chunk.fromArray(out.toByteArray))
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.foreach: path =>
          if Files.isRegularFile(path) then
            val name = root.relativize(path).toString.replace('\\', '/')
            if safe(name) then
              zip.putNextEntry(new ZipEntry(name))
              zip.write(Files.readAllBytes(path))
              zip.closeEntry()
        zip.finish()
        Right(Chunk.fromArray(out.toByteArray))
      finally
        zip.close()
        walk.close()

  private def unpack(root: Path, bytes: Chunk[Byte]): ZIO[Any, PluginCompileError, Unit] =
    ZIO
      .attemptBlocking(unpackNow(root, bytes))
      .mapError(_ => PluginCompileError.Io(IoFailure(root, IoFault.Write)))
      .flatMap:
        case Left(failure) => ZIO.fail(PluginCompileError.Io(failure))
        case Right(_)      => ZIO.unit

  /** The zip is the sealed tree. Files already in `root` belong to a previous key. */
  private def unpackNow(root: Path, bytes: Chunk[Byte]): Either[IoFailure, Unit] =
    if accepted(root, bytes) then Right(replace(root, bytes))
    else Left(IoFailure(root, IoFault.Read))

  private def reset(root: Path): ZIO[Any, PluginCompileError, Unit] =
    ZIO
      .attemptBlocking(resetNow(root))
      .mapError(_ => PluginCompileError.Io(IoFailure(root, IoFault.Write)))

  private def resetNow(root: Path): Unit =
    val base = root.toAbsolutePath.normalize
    if Files.isDirectory(base) then
      val walk   = Files.walk(base)
      val doomed =
        try walk.iterator.asScala.filterNot(_.toAbsolutePath.normalize == base).toList
        finally walk.close()
      doomed.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    else if Files.exists(base) then Files.delete(base)
    Files.createDirectories(base)

  private def accepted(root: Path, bytes: Chunk[Byte]): Boolean =
    val base = root.toAbsolutePath.normalize
    val zip  = new ZipInputStream(new ByteArrayInputStream(bytes.toArray))
    try
      def loop: Boolean =
        Option(zip.getNextEntry) match
          case None        => true
          case Some(entry) =>
            val name = entry.getName
            val dest = base.resolve(name).normalize
            val ok   = safe(name) && dest.startsWith(base)
            zip.closeEntry()
            ok && loop
      loop
    finally zip.close()

  private def replace(root: Path, bytes: Chunk[Byte]): Unit =
    val base = root.toAbsolutePath.normalize
    resetNow(base)
    val zip = new ZipInputStream(new ByteArrayInputStream(bytes.toArray))
    try
      def loop: Unit =
        Option(zip.getNextEntry) match
          case None        => ()
          case Some(entry) =>
            val dest = base.resolve(entry.getName).normalize
            if entry.isDirectory then Files.createDirectories(dest)
            else
              Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
              Files.write(dest, zip.readAllBytes)
            zip.closeEntry()
            loop
      loop
    finally zip.close()

  private def safe(name: String): Boolean =
    val parts = name.split("/", -1)
    name.nonEmpty &&
    !name.startsWith("/") &&
    !name.contains('\\') &&
    !name.contains("//") &&
    parts.forall(part => part.nonEmpty && part != "." && part != "..")

  private def javaHome: ZIO[Any, PluginCompileError, Path] =
    ZIO
      .attemptBlocking(Option(java.lang.System.getProperty("java.home")))
      .orDie
      .flatMap:
        case Some(home) if home.nonEmpty =>
          val bin = Path.of(home, "bin", "java")
          ZIO
            .attemptBlocking(Files.isRegularFile(bin))
            .mapError(_ => PluginCompileError.Io(IoFailure(bin, IoFault.Stat)))
            .flatMap:
              case true  => ZIO.succeed(bin)
              case false => ZIO.fail(PluginCompileError.Io(IoFailure(bin, IoFault.Stat)))
        case _ => ZIO.fail(PluginCompileError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))

  private def classPath: ZIO[Any, PluginCompileError, String] =
    ZIO
      .attemptBlocking(Option(java.lang.System.getProperty("java.class.path")))
      .orDie
      .flatMap:
        case Some(cp) if cp.nonEmpty => ZIO.succeed(cp)
        case _ => ZIO.fail(PluginCompileError.Io(IoFailure(Path.of("java.class.path"), IoFault.Stat)))

  private def readText(path: Path): ZIO[Any, PluginCompileError, String] =
    ZIO
      .attemptBlocking(if Files.isRegularFile(path) then Some(Files.readString(path)) else None)
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Read)))
      .flatMap:
        case Some(text) => ZIO.succeed(text)
        case None       => ZIO.succeed("")

end PluginCompiler
