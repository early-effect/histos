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
import histos.{Dep, Module, Platform, ProjectName, ScalaVersion, ToolCatalog}

import java.nio.charset.StandardCharsets

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

  def message: String = this match
    case Locked(error)                               => error.message
    case Absent(module, version, platform, scope)    =>
      s"${scope.header} ${module.value} ${version.value} ${platform.token} is not in the dependency lock"
    case NoNode                                      => "node is not on disk"
end CoverGap

/** Inputs of the cover a quick record stores. Preview and [[SuiteLaunch.remember]] both pass this to [[SuiteCover.stored]].
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
  def stored(ask: SuiteAsk): Either[CoverGap, SuiteStamp.Cover] =
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
    yield SuiteStamp.Cover(
      (own ++ tools ++ identities.map(SuiteCover.tokenHash)).map(_.value),
      upstream.map(_.value),
      Runtimes.identityToken(ask.jdk),
      ask.module.flags.argv,
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
        case (Left(gap), _)               => Left(gap)
        case (Right(acc), dependency)     =>
          val dependencies = Dep.select(dependency.dependencies, ask.platform)
          cellHashes(ask, dependency, ClasspathScope.Compile, dependencies, dropStandardLibrary = false).map(found =>
            acc ++ found
          )
      .map(found => found.distinct.filterNot(own.contains))

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
      case (Left(error), _)     => Left(error)
      case (Right(acc), request) =>
        lock.tools.find(_.id == request.id) match
          case None       => Left(ResolveError.NoTool(request.id))
          case Some(cell) =>
            LockedTools.validate(request, cell).map: _ =>
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
