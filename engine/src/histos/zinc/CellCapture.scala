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
  SourceKind,
  SourceProvenance,
  SourceSet,
}
import histos.workspace.{IoFailure, IoFault}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

/** The snapshot a plugin-library compile leaves on its cell. Compiler jars are the bridge, then the compiler classpath.
  * Runtime jars are the library jars that compile loaded. An empty list fails.
  */
object CellCapture:
  def record(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      sources: List[Path],
      unmanagedRoot: Path,
      sourceBase: Path,
      classes: Path,
      classpath: List[Path],
      jdk: JdkVersion,
      runtime: List[Path],
      compiler: List[Path],
      bridge: Path,
  ): IO[GraphCompileError, CompilationCapture] =
    if compiler.isEmpty then ZIO.fail(missing(module, version, platform, compilerPart(version)))
    else if runtime.isEmpty then ZIO.fail(missing(module, version, platform, CompilerPart.Library))
    else
      val refused = (error: histos.AssetError) => GraphCompileError.Assets(module, version, platform, error)
      for
        inputs    <- selected(repo, module, version, platform, unmanagedRoot, sources, SourceKind.Scala)
        resources <- histos.pack.Resources
          .filesAt(sourceBase.resolve("main").resolve("resources"))
          .mapError(refused)
        listed <- selected(
          repo,
          module,
          version,
          platform,
          unmanagedRoot,
          resources.map(_._2),
          SourceKind.Resource,
        )
        outputs   <- seal(module, version, platform, classes)
        closure   <- ZIO.foreach(classpath)(member(module, version, platform, _))
        compilers <- sealJars(module, version, platform, bridge :: compiler)
        libraries <- sealJars(module, version, platform, runtime)
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
          jdk,
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
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      unmanagedRoot: Path,
      paths: List[Path],
      fallback: SourceKind,
  ): IO[GraphCompileError, List[SelectedInput]] =
    ZIO.foreach(paths): path =>
      val file = path.toAbsolutePath.normalize
      val root = unmanagedRoot.toAbsolutePath.normalize
      val name = repo.relativize(file).toString.replace('\\', '/')
      for
        relative <- ZIO.fromEither(RepoPath.from(name)).mapError(_ => unread(module, version, platform, file))
        bytes    <- read(module, version, platform, file)
      yield
        val provenance = if file.startsWith(root) then SourceProvenance.Generated else SourceProvenance.Unmanaged
        SelectedInput(
          relative,
          kind(file, fallback),
          SourceSet.Main,
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
          case Left(path)    => ZIO.fail(unread(module, version, platform, path))
          case Right(listed) =>
            ZIO
              .fromEither(SealedArtifactManifest.from(listed))
              .mapError(_ => unread(module, version, platform, classes))

  private def entries(classes: Path): Either[Path, List[ArtifactEntry]] =
    if !Files.isDirectory(classes) then Left(classes)
    else
      val walk = Files.walk(classes)
      try
        val found = walk.iterator.asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString)
        found.foldLeft[Either[Path, List[ArtifactEntry]]](Right(Nil)):
          case (Left(path), _)    => Left(path)
          case (Right(acc), file) =>
            val name = classes.relativize(file).toString.replace('\\', '/')
            ArtifactPath.from(name) match
              case Left(_)     => Left(file)
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
    path.toString.replace('\\', '/')

  private def unread(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      path: Path,
  ): GraphCompileError =
    GraphCompileError.Failed(module, version, platform, CompileError.Io(IoFailure(path, IoFault.Read)))
end CellCapture
