package histos.testrun

import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.resolve.{
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  LockedTools,
  Repositories,
  ResolveError,
  ToolInput,
}
import histos.toolchain.{RuntimeIdentity, Runtimes}
import histos.zinc.{ApiDigest, GraphCompile}
import histos.{Dep, Module, Platform, ProjectName, ScalaVersion, ToolCatalog}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.Chunk

/** Why [[SuiteCover.stored]] cannot build the cover a quick record stores. */
private[histos] enum CoverGap:
  case Locked(error: ResolveError)
  case Absent(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
  )
  case NoNode
  case NoApi(module: ProjectName, version: ScalaVersion, platform: Platform)

  def message: String = this match
    case Locked(error)                            => error.message
    case Absent(module, version, platform, scope) =>
      s"${scope.header} ${module.value} ${version.value} ${platform.token} is not in the dependency lock"
    case NoNode                           => "node is not on disk"
    case NoApi(module, version, platform) =>
      s"'${module.value}' ${version.value} ${platform.token} has no readable API digest"
end CoverGap

/** Inputs of the cover a quick record stores. Preview and [[SuiteLaunch.remember]] both pass this to
  * [[SuiteCover.stored]].
  */
private[histos] final case class SuiteAsk(
    module: Module,
    modules: List[Module],
    version: ScalaVersion,
    platform: Platform,
    lock: DependencyLock,
    repositories: Repositories,
    catalog: ToolCatalog,
    native: NativeOrigin,
    jdk: RuntimeIdentity,
    node: Option[RuntimeIdentity],
)

/** The cover a quick record stores. Jar lines are lock shas and tool fingerprints. Nothing here fetches.
  */
private[histos] object SuiteCover:
  def stored(repo: Path, ask: SuiteAsk): Either[CoverGap, SuiteStamp.Cover] =
    val runtimes = ask.platform match
      case Platform.Js =>
        ask.node match
          case Some(identity) => Right(List(ask.jdk, identity))
          case None           => Left(CoverGap.NoNode)
      case Platform.Jvm | Platform.Native => Right(List(ask.jdk))
    for
      identities <- runtimes
      own        <- ownHashes(ask)
      upstream   <- upstreamHashes(ask, own.toSet)
      tools      <- toolHashes(ask)
      apis       <- upstreamApis(repo, ask)
    yield SuiteStamp.Cover(
      (own ++ tools ++ identities.map(SuiteCover.tokenHash)).map(_.value),
      upstream.map(_.value),
      Runtimes.identityToken(ask.jdk),
      ask.module.flags.argv,
      apis,
    )

  /** Sha256 of the token [[TestContext]] records for one runtime. */
  private[histos] def tokenHash(identity: RuntimeIdentity): Sha256 =
    Sha256.of(
      Chunk.fromArray(Runtimes.identityToken(identity).getBytes(StandardCharsets.UTF_8))
    )

  private def ownHashes(ask: SuiteAsk): Either[CoverGap, List[Sha256]] =
    val (dependencies, scope) = classpath(ask.module, ask.platform)
    cellHashes(ask, ask.module, scope, dependencies, dropStandardLibrary = ask.platform == Platform.Jvm)

  private def upstreamHashes(ask: SuiteAsk, own: Set[Sha256]): Either[CoverGap, List[Sha256]] =
    Module
      .chain(ask.module, ask.modules, ask.platform)
      .foldLeft[Either[CoverGap, List[Sha256]]](Right(Nil)):
        case (Left(gap), _)           => Left(gap)
        case (Right(acc), dependency) =>
          val dependencies = Dep.select(dependency.dependencies, ask.platform)
          cellHashes(ask, dependency, ClasspathScope.Compile, dependencies, dropStandardLibrary = false)
            .map(found => acc ++ found)
      .map(found => found.distinct.filterNot(own.contains))

  /** A missing file adds no line. [[ApiDigest.empty]] would claim this upstream defined no classes. */
  private def upstreamApis(repo: Path, ask: SuiteAsk): Either[CoverGap, List[String]] =
    Module
      .chain(ask.module, ask.modules, ask.platform)
      .foldLeft[Either[CoverGap, List[String]]](Right(Nil)):
        case (Left(gap), _)           => Left(gap)
        case (Right(acc), dependency) =>
          apiLine(repo, dependency.name, ask.version, ask.platform).map:
            case None       => acc
            case Some(line) => acc :+ line

  private def apiLine(
      repo: Path,
      name: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Either[CoverGap, Option[String]] =
    GraphCompile.analysisDirOf(GraphCompile.classesDir(repo, name, version, platform)) match
      case None      => Left(CoverGap.NoApi(name, version, platform))
      case Some(dir) =>
        val path = dir.resolve(ApiDigest.fileName)
        if !Files.isRegularFile(path) then Right(None)
        else
          ApiDigest.read(dir) match
            case Left(_)    => Left(CoverGap.NoApi(name, version, platform))
            case Right(sha) => Right(Some(s"${name.value} ${sha.value}"))

  private def cellHashes(
      ask: SuiteAsk,
      module: Module,
      scope: ClasspathScope,
      dependencies: List[Dep],
      dropStandardLibrary: Boolean,
  ): Either[CoverGap, List[Sha256]] =
    ask.lock.cells.find(cell =>
      cell.module == module.name && cell.scalaVersion == ask.version && cell.platform == ask.platform && cell.scope == scope
    ) match
      case Some(cell) =>
        val kept =
          if dropStandardLibrary then cell.artifacts.filterNot(artifact => standardLibrary(artifact.name))
          else cell.artifacts
        Right(kept.map(_.sha256))
      case None if dependencies.nonEmpty =>
        Left(CoverGap.Absent(module.name, ask.version, ask.platform, scope))
      case None => Right(Nil)

  private def classpath(module: Module, platform: Platform): (List[Dep], ClasspathScope) =
    val compileDeps = Dep.select(module.dependencies, platform)
    val testDeps    = Dep.select(module.testDependencies, platform)
    if testDeps.isEmpty then (compileDeps, ClasspathScope.Compile)
    else (compileDeps ++ testDeps, ClasspathScope.Test)

  private def toolHashes(ask: SuiteAsk): Either[CoverGap, List[Sha256]] =
    for
      compiled <- LockedTools
        .compilationRequests(
          ask.module,
          ask.version,
          ask.platform,
          ask.native,
          ask.catalog,
          ask.repositories,
        )
        .left
        .map(CoverGap.Locked(_))
      executed <- LockedTools
        .executionRequests(ask.module, ask.platform, ask.native, ask.catalog, ask.repositories)
        .left
        .map(CoverGap.Locked(_))
      hashed <- fingerprints(ask.lock, (compiled ++ executed).distinctBy(_.id)).left.map(CoverGap.Locked(_))
    yield hashed

  private def fingerprints(
      lock: DependencyLock,
      requests: List[histos.resolve.ToolRequest],
  ): Either[ResolveError, List[Sha256]] =
    requests.foldLeft[Either[ResolveError, List[Sha256]]](Right(Nil)):
      case (Left(error), _)      => Left(error)
      case (Right(acc), request) =>
        lock.tools.find(_.id == request.id) match
          case None       => Left(ResolveError.NoTool(request.id))
          case Some(cell) =>
            LockedTools
              .validate(request, cell)
              .map: _ =>
                acc :+ ToolInput.fingerprint(request.id, cell.artifacts.map(row))

  private def row(artifact: LockedArtifact): ToolInput.Row =
    ToolInput.Row(
      artifact.organization,
      artifact.name,
      artifact.revision,
      artifact.classifier,
      artifact.sha256,
    )

  /** The artifact name `JvmCompile.isStandardLibrary` drops once coursier has named the file. */
  private def standardLibrary(name: String): Boolean =
    name == "scala-library" || name.startsWith("scala3-library")
end SuiteCover
