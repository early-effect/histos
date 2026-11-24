package histos.zinc

import histos.jdk.{JdkVersion, Sha256, Sha256Digest}
import histos.plugin.{
  ArtifactEntry,
  ArtifactFormat,
  ArtifactIdentity,
  ArtifactPath,
  ClosureArtifact,
  ClosureRole,
  ContentHash,
  Digests,
  RepoPath,
  SealedArtifactManifest,
  SelectedInput,
  RecipeError,
  SourceKind,
  SourceProvenance,
}
import histos.workspace.{IoFailure, IoFault}
import histos.{CompilationStage, Platform, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

final case class CaptureProduct(classes: Path, classpath: List[Path])
final case class CaptureTools(jdk: JdkVersion, runtime: List[Path], compiler: List[Path], bridge: Path)
final case class CaptureRequest(
    repo: Path,
    stage: CompilationStage,
    files: StageFiles,
    product: CaptureProduct,
    tools: CaptureTools,
)

/** The snapshot a plugin-library compile leaves on its cell. Compiler jars are the bridge, then the compiler classpath.
  * Runtime jars are the library jars that compile loaded. An empty list fails.
  */
object CellCapture:
  private val CompileTimeFile = "compile-time.inputs"

  /** Macro implementations may execute ordinary helpers. Their full classpath bytes are compile inputs. */
  def implementation(path: Path): IO[CompileError, Sha256] =
    ZIO
      .attemptBlocking {
        if Files.isDirectory(path) then
          val walk = Files.walk(path)
          try
            val files = walk.iterator.asScala
              .filter(Files.isRegularFile(_))
              .toList
              .sortBy(file => path.relativize(file).toString)
            val bytes = files.flatMap { file =>
              val name    = path.relativize(file).toString.replace('\\', '/')
              val content = Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(file))).value
              s"$name $content\n".getBytes(java.nio.charset.StandardCharsets.UTF_8).toList
            }
            Sha256Digest.of(Chunk.fromIterable(bytes))
          finally walk.close()
        else Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path)))
      }
      .mapError(_ => CompileError.Io(IoFailure(path, IoFault.Read)))

  /** An implementation change must invalidate Zinc's old analysis as well as the outer compile key. */
  def refreshCompileTime(analysis: Path, classes: Path, inputs: List[Sha256]): IO[CompileError, Unit] =
    val file    = analysis.resolve(CompileTimeFile)
    val current = inputs.map(_.value).mkString("\n")
    for
      previous <- histos.workspace.Workspace.readIfPresent(file).mapError(CompileError.Io(_))
      _        <-
        if previous.contains(current) || (previous.isEmpty && inputs.isEmpty) then ZIO.unit
        else
          histos.workspace.Workspace.recreate(analysis).mapError(CompileError.Io(_)) *>
            histos.workspace.Workspace.recreate(classes).mapError(CompileError.Io(_)).unit
    yield ()

  def recordCompileTime(analysis: Path, inputs: List[Sha256]): IO[CompileError, Unit] =
    histos.workspace.Workspace
      .writeString(analysis.resolve(CompileTimeFile), inputs.map(_.value).mkString("\n"))
      .mapError(CompileError.Io(_))

  def record(request: CaptureRequest): IO[GraphCompileError, CompilationCapture] =
    val cell     = request.stage.cell
    val module   = cell.module
    val version  = cell.version
    val platform = cell.platform
    val files    = request.files
    val tools    = request.tools
    if tools.compiler.isEmpty then ZIO.fail(missing(module, version, platform, compilerPart(version)))
    else if tools.runtime.isEmpty then ZIO.fail(missing(module, version, platform, CompilerPart.Library))
    else
      for
        inputs    <- selected(request, files.sources.map(_.path), SourceKind.Scala)
        listed    <- selected(request, files.resources.map(_._2), SourceKind.Resource)
        outputs   <- seal(module, version, platform, request.product.classes)
        closure   <- ZIO.foreach(request.product.classpath)(member(module, version, platform, _))
        compilers <- sealJars(module, version, platform, tools.bridge :: tools.compiler)
        libraries <- sealJars(module, version, platform, tools.runtime)
      yield CompilationCapture.Recorded(
        CompilationSnapshot(
          module,
          version,
          platform,
          inputs,
          listed,
          Nil,
          Nil,
          closure,
          Nil,
          Nil,
          compilers,
          libraries,
          tools.jdk,
          outputs,
          Nil,
        )
      )

  private def compilerPart(version: ScalaVersion): CompilerPart =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala2(13)) => CompilerPart.Compiler
      case _                            => CompilerPart.Scala3Compiler

  private def missing(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      part: CompilerPart,
  ): GraphCompileError =
    GraphCompileError.Failed(module, version, platform, CompileError.Toolchain(ToolchainMiss.Missing(version, part)))

  private def sealJars(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      paths: List[Path],
  ): IO[GraphCompileError, List[ArtifactIdentity]] =
    ZIO.foreach(paths): path =>
      hashFile(module, version, platform, path).map: hash =>
        ArtifactIdentity(ArtifactFormat.Jar, ContentHash.trusted(hash.value))

  /** Scalac sees the runtime, then upstream class directories, then jars. This cell's own classes stay the output. */
  def classpath(assembled: ScalacClasspath, upstream: List[UpstreamCell]): List[Path] =
    val dirs    = upstream.map(_.classes)
    val jars    = assembled.jars
    val runtime = assembled.paths.length - jars.length - dirs.length
    if runtime < 0 then assembled.paths
    else assembled.paths.take(runtime) ++ dirs ++ jars

  private def selected(
      request: CaptureRequest,
      paths: List[Path],
      fallback: SourceKind,
  ): IO[GraphCompileError, List[SelectedInput]] =
    val cell     = request.stage.cell
    val module   = cell.module
    val version  = cell.version
    val platform = cell.platform
    val root     = request.repo.toAbsolutePath.normalize
    ZIO.foreach(paths): path =>
      val file   = path.toAbsolutePath.normalize
      val parsed =
        if !file.startsWith(root) then Left(CompileError.Outside(file, root))
        else RepoPath.from(slash(root.relativize(file))).left.map(_ => CompileError.InvalidPath(file))
      for
        relative <- ZIO.fromEither(parsed).mapError(GraphCompileError.Failed(module, version, platform, _))
        bytes    <- read(module, version, platform, file)
      yield
        val provenance =
          if request.files.generatedPaths.contains(file) then SourceProvenance.Generated else SourceProvenance.Unmanaged
        SelectedInput(
          relative,
          kind(file, fallback),
          request.stage.scope,
          provenance,
          Digests.content(bytes),
          bytes.length.toLong,
        )

  private def seal(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
  ): IO[GraphCompileError, SealedArtifactManifest] =
    ZIO
      .attemptBlocking(entries(classes))
      .mapError(_ => unread(module, version, platform, classes))
      .flatMap: found =>
        found match
          case Left(error)   => ZIO.fail(GraphCompileError.Failed(module, version, platform, error))
          case Right(listed) =>
            ZIO
              .fromEither(SealedArtifactManifest.from(listed))
              .mapError(error => GraphCompileError.Recipe(RecipeError.Seal(error)))

  private def entries(classes: Path): Either[CompileError, List[ArtifactEntry]] =
    if !Files.isDirectory(classes) then Left(CompileError.Io(IoFailure(classes, IoFault.Read)))
    else
      val walk = Files.walk(classes)
      try
        val found = walk.iterator.asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString)
        found.foldLeft[Either[CompileError, List[ArtifactEntry]]](Right(Nil)):
          case (Left(error), _)   => Left(error)
          case (Right(acc), file) =>
            val name = slash(classes.relativize(file))
            ArtifactPath.from(name) match
              case Left(_)     => Left(CompileError.InvalidPath(file))
              case Right(path) =>
                val bytes = Files.readAllBytes(file)
                Right(acc :+ ArtifactEntry(path, bytes.length.toLong, Digests.content(bytes)))
      finally walk.close()

  private def member(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      path: Path,
  ): IO[GraphCompileError, ClosureArtifact] =
    ZIO
      .attemptBlocking(Files.isDirectory(path))
      .mapError(_ => unread(module, version, platform, path))
      .flatMap: directory =>
        val hashed = if directory then hashDirectory(module, version, platform, path)
        else hashFile(module, version, platform, path)
        hashed.map: hash =>
          val format = if directory then ArtifactFormat.Directory else ArtifactFormat.Jar
          ClosureArtifact(ArtifactIdentity(format, ContentHash.trusted(hash.value)), ClosureRole.Compile)

  private def hashDirectory(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      path: Path,
  ): IO[GraphCompileError, Sha256] =
    ZIO
      .attemptBlocking {
        val root   = path.toAbsolutePath.normalize
        val stream = Files.walk(root)
        try
          stream.iterator.asScala.filter(Files.isRegularFile(_)).map(root.relativize).toList.sortBy(_.toString)
        finally stream.close()
      }
      .mapError(_ => unread(module, version, platform, path))
      .flatMap: listed =>
        ZIO
          .foreach(listed)(name =>
            hashFile(module, version, platform, path.resolve(name)).map(hash => s"${slash(name)} ${hash.value}")
          )
          .map: lines =>
            Sha256Digest.of(Chunk.fromArray(lines.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  private def hashFile(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      path: Path,
  ): IO[GraphCompileError, Sha256] =
    read(module, version, platform, path).map(bytes => Sha256Digest.of(Chunk.fromArray(bytes)))

  private def read(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      path: Path,
  ): IO[GraphCompileError, Array[Byte]] =
    ZIO
      .attemptBlocking(Option.when(Files.isRegularFile(path))(Files.readAllBytes(path)))
      .mapError(_ => unread(module, version, platform, path))
      .flatMap:
        case Some(bytes) => ZIO.succeed(bytes)
        case None        => ZIO.fail(unread(module, version, platform, path))

  private def kind(path: Path, fallback: SourceKind): SourceKind =
    Option(path.getFileName).map(_.toString) match
      case Some(name) if name.endsWith(".java")  => SourceKind.Java
      case Some(name) if name.endsWith(".scala") => SourceKind.Scala
      case _                                     => fallback

  private def slash(path: Path): String =
    path.iterator.asScala.map(_.toString).mkString("/")

  private def unread(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      path: Path,
  ): GraphCompileError =
    GraphCompileError.Failed(module, version, platform, CompileError.Io(IoFailure(path, IoFault.Read)))
end CellCapture
