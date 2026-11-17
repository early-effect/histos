package histos.pack

import histos.jdk.{JdkVersion, VersionError}
import histos.load.BuildClasspath
import histos.resolve.{Repositories, Resolve, ResolveCell, ResolveError, ResolvedArtifact}
import histos.toolchain.{HostCompilerError, HostCompilerPackage}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.CompilerRequests
import histos.{Dep, DepKind, Platform, ProjectName, ScalaVersion, ScalaVersionError}

import java.nio.file.{Files, Path}
import java.util.zip.{ZipException, ZipFile}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}

/** The published coordinate the installing machine resolves. */
final case class Coordinate(organization: String, name: String, revision: String)

/** Builds the toolchain jar on the machine that installs it. The published module stays thin. */
object Distribution:
  private val Program: MainClass = MainClass.trusted("histos.cli.Main")

  /** Resolve `coordinate`, nest the api jar, and shade the rest. Compiler jars come from the compiler request, not from
    * the application classpath.
    */
  def install(
      coordinate: Coordinate,
      repositories: Repositories,
      destination: Path,
  ): ZIO[Resolve & Scope, DistributionError, Assembled] =
    for
      _       <- ZIO.fromEither(present(coordinate))
      version <- running
      cell = ResolveCell(
        ProjectName("pack"),
        List(Dep.trusted(coordinate.organization, coordinate.name, coordinate.revision, DepKind.Java)),
        version,
        Platform.Jvm,
        repositories,
      )
      classpath <- ZIO.serviceWithZIO[Resolve](_.resolve(cell)).mapError(DistributionError.Resolve(_))
      picked    <- ZIO.fromEither(pick(classpath.artifacts, coordinate))
      jdk       <- ZIO.fromEither(jdkNow)
      compiler  <- ZIO
        .serviceWithZIO[Resolve](_.resolveTool(CompilerRequests.compiler(picked.version, repositories)))
        .mapError(DistributionError.Resolve(_))
      nested <- HostCompilerPackage
        .files(picked.version, jdk, compiler.artifacts)
        .mapError(DistributionError.Compiler(_))
      assembled <- write(picked.application, picked.jars, picked.api, destination, nested)
    yield assembled

  /** `application` is exploded so its classes win. `api` is stored at `bootstrap/histos-api.jar` and may also be one of
    * `jars`. `compiler` entries are stored, not unzipped.
    */
  def write(
      application: Path,
      jars: List[Path],
      api: Path,
      destination: Path,
      compiler: List[AssemblyFile] = Nil,
  ): ZIO[Scope, DistributionError, Assembled] =
    for
      classes   <- Workspace.work("histos-explode").mapError(DistributionError.Io(_))
      _         <- explode(application, classes)
      assembled <- Assembly
        .write(
          List(classes),
          jars.filterNot(same(application)),
          Program,
          destination,
          AssemblyFile(BuildClasspath.Entry, api) :: compiler,
        )
        .mapError(DistributionError.Assembly(_))
    yield assembled

  private[histos] def pick(
      artifacts: List[ResolvedArtifact],
      coordinate: Coordinate,
  ): Either[DistributionError, Picked] =
    for
      application <- one(
        artifacts.filter(item => matches(item, coordinate)),
        DistributionError.MissingApplication(coordinate),
        DistributionError.AmbiguousApplication(coordinate),
      )
      api <- one(
        artifacts.filter(apiJar),
        DistributionError.MissingApi,
        DistributionError.AmbiguousApi,
      )
      compiler <- one(
        artifacts.filter(compilerJar),
        DistributionError.MissingCompiler,
        DistributionError.AmbiguousCompiler,
      )
      version <- ScalaVersion.from(compiler.revision).left.map(DistributionError.Scala(_))
    yield Picked(
      application.path,
      artifacts.filterNot(_.path == application.path).map(_.path),
      api.path,
      version,
    )

  private[histos] final case class Picked(application: Path, jars: List[Path], api: Path, version: ScalaVersion)

  private def present(coordinate: Coordinate): Either[DistributionError, Unit] =
    if coordinate.organization.isEmpty || coordinate.name.isEmpty || coordinate.revision.isEmpty then
      Left(DistributionError.BlankCoordinate)
    else Right(())

  private def one(
      found: List[ResolvedArtifact],
      missing: DistributionError,
      ambiguous: DistributionError,
  ): Either[DistributionError, ResolvedArtifact] =
    found match
      case item :: Nil => Right(item)
      case Nil         => Left(missing)
      case _           => Left(ambiguous)

  private def matches(artifact: ResolvedArtifact, coordinate: Coordinate): Boolean =
    artifact.classifier.isEmpty &&
      artifact.organization == coordinate.organization &&
      artifact.name == coordinate.name &&
      artifact.revision == coordinate.revision

  private def apiJar(artifact: ResolvedArtifact): Boolean =
    artifact.classifier.isEmpty && artifact.name.startsWith("histos-api_") && !artifact.name.contains("_native")

  private def compilerJar(artifact: ResolvedArtifact): Boolean =
    artifact.classifier.isEmpty && artifact.name == "scala3-compiler_3"

  private def running: ZIO[Any, DistributionError, ScalaVersion] =
    ZIO.fromEither(ScalaVersion.from(scala.util.Properties.versionNumberString).left.map(DistributionError.Scala(_)))

  private def jdkNow: Either[DistributionError, JdkVersion] =
    val raw = Option(System.getProperty("java.version")).getOrElse("")
    JdkVersion.from(raw).left.map(DistributionError.Jdk(_))

  private def same(application: Path)(jar: Path): Boolean =
    jar.toAbsolutePath.normalize == application.toAbsolutePath.normalize

  private def explode(jar: Path, dest: Path): ZIO[Any, DistributionError, Unit] =
    ZIO
      .attemptBlocking(unzip(jar, dest))
      .mapError(_ => DistributionError.Io(IoFailure(jar, IoFault.Read)))
      .flatMap(ZIO.fromEither)

  private def unzip(jar: Path, dest: Path): Either[DistributionError, Unit] =
    open(jar).flatMap: zip =>
      try
        val root = dest.toAbsolutePath.normalize
        zip.entries.asScala.toList
          .foldLeft[Either[DistributionError, Boolean]](Right(false)): (acc, entry) =>
            acc.flatMap(seen => copyEntry(zip, jar, root, entry, seen))
          .flatMap: seen =>
            if seen then Right(()) else Left(DistributionError.Empty(jar))
      finally zip.close()

  private def open(jar: Path): Either[DistributionError, ZipFile] =
    if !Files.isRegularFile(jar) then Left(DistributionError.NotAJar(jar))
    else
      try Right(new ZipFile(jar.toFile))
      catch case _: ZipException => Left(DistributionError.NotAJar(jar))

  private def copyEntry(
      zip: ZipFile,
      jar: Path,
      root: Path,
      entry: java.util.zip.ZipEntry,
      seen: Boolean,
  ): Either[DistributionError, Boolean] =
    val name = Option(entry.getName).getOrElse("")
    if omit(name) || entry.isDirectory then Right(seen)
    else if escaped(name) then Left(DistributionError.Escaped(name))
    else
      val target = root.resolve(name).normalize
      if !target.startsWith(root) then Left(DistributionError.Escaped(name))
      else
        Option(zip.getInputStream(entry)) match
          case None        => Left(DistributionError.NotAJar(jar))
          case Some(input) =>
            try
              Option(target.getParent).foreach(parent => Files.createDirectories(parent))
              Files.copy(input, target)
              Right(seen || name.endsWith(".class"))
            finally input.close()

  private def omit(name: String): Boolean =
    name.isEmpty || name.endsWith("/") || ClassJar.semanticdb(name) || packaged(name)

  /** Names `Assembly` drops from a class directory, so a signature is not the first writer. */
  private def packaged(name: String): Boolean =
    name == "META-INF/MANIFEST.MF" ||
      name == "META-INF/INDEX.LIST" ||
      name == "module-info.class" ||
      (name.startsWith("META-INF/versions/") && name.endsWith("/module-info.class")) ||
      name.startsWith("META-INF/SIG-") ||
      topLevelSignature(name)

  private def topLevelSignature(name: String): Boolean =
    if !name.startsWith("META-INF/") then false
    else
      val rest = name.drop("META-INF/".length)
      if rest.contains('/') then false
      else
        val upper = rest.toUpperCase(java.util.Locale.ROOT)
        upper.endsWith(".SF") || upper.endsWith(".DSA") || upper.endsWith(".RSA") || upper.endsWith(".EC")

  private def escaped(name: String): Boolean =
    name.startsWith("/") || name.contains('\\') || name.split('/').exists(part => part.isEmpty || part == "..")
end Distribution

enum DistributionError:
  case BlankCoordinate
  case MissingApplication(coordinate: Coordinate)
  case AmbiguousApplication(coordinate: Coordinate)
  case MissingApi
  case AmbiguousApi
  case MissingCompiler
  case AmbiguousCompiler
  case NotAJar(path: Path)
  case Escaped(name: String)
  case Empty(path: Path)
  case Scala(error: ScalaVersionError)
  case Jdk(error: VersionError)
  case Assembly(error: AssemblyError)
  case Resolve(error: ResolveError)
  case Compiler(error: HostCompilerError)
  case Io(failure: IoFailure)

  def message: String = this match
    case BlankCoordinate                => "a coordinate needs an organization, a name, and a revision"
    case MissingApplication(coordinate) =>
      s"${shown(coordinate)} is not on the classpath"
    case AmbiguousApplication(coordinate) =>
      s"${shown(coordinate)} is on the classpath more than once"
    case MissingApi        => "the classpath has no histos-api jar"
    case AmbiguousApi      => "the classpath has more than one histos-api jar"
    case MissingCompiler   => "the classpath has no scala3-compiler jar"
    case AmbiguousCompiler => "the classpath has more than one scala3-compiler jar"
    case NotAJar(path)     => s"$path is not a jar"
    case Escaped(name)     => s"'$name' leaves the jar"
    case Empty(path)       => s"$path has no class files"
    case Scala(error)      => error.message
    case Jdk(error)        => error.message
    case Assembly(error)   => error.message
    case Resolve(error)    => error.message
    case Compiler(error)   => error.message
    case Io(failure)       => failure.message

  private def shown(coordinate: Coordinate): String =
    s"${coordinate.organization}:${coordinate.name}:${coordinate.revision}"
end DistributionError
