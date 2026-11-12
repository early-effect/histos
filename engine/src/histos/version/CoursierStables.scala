package histos.version

import histos.central.MavenModule
import histos.resolve.{BuildResolvers, Repository}
import histos.Resolver

import zio.{IO, ZIO}

/** `coursier.Versions` lists each repository beside the versions it found. Ivy2 local is always asked. A maven
  * repository is asked only when this build resolves from it. An empty resolver list is Central.
  *
  * `eitherResult` blocks on Coursier's `Task`, which is a `Future`. The failure becomes [[BumpFault]] here.
  */
object CoursierStables:
  def list(module: MavenModule, resolvers: List[Resolver]): IO[BumpFault, List[ListedStable]] =
    ZIO
      .attemptBlockingInterrupt(ask(module, resolvers))
      .mapError(_ => BumpFault.Unanswered(coordinate(module)))
      .flatMap:
        case Left(_)       => ZIO.fail(BumpFault.Unanswered(coordinate(module)))
        case Right(result) => ZIO.fromEither(fromResult(coordinate(module), result))

  /** Stable ship numbers from a `Versions` result. A snapshot is not one. A failed repository is skipped. */
  def fromResult(coordinate: String, result: coursier.Versions.Result): Either[BumpFault, List[ListedStable]] =
    val answered = result.results.collect { case (repo, Right(versions)) => repo -> versions }
    if answered.isEmpty then Left(BumpFault.Unanswered(coordinate))
    else Right(stables(answered.toList))

  def stables(rows: List[(coursier.core.Repository, coursier.core.Versions)]): List[ListedStable] =
    rows.flatMap: (repo, versions) =>
      val place = placeOf(repo)
      versions.available0.flatMap: version =>
        ShipNumber.from(version.asString).toOption.map(number => ListedStable(number, place))

  def placeOf(repository: coursier.core.Repository): StablePlace =
    if repository == coursier.LocalRepositories.ivy2Local then StablePlace.Local else StablePlace.Remote

  private def ask(
      module: MavenModule,
      resolvers: List[Resolver],
  ): Either[coursier.error.CoursierError, coursier.Versions.Result] =
    val cache = coursier.cache.FileCache().withLocation(coursier.cache.CacheDefaults.location.toString)
    coursier
      .Versions(cache)
      .withModule(
        coursier.Module(coursier.Organization(module.group.value), coursier.ModuleName(module.artifact.value))
      )
      .copy(repositories = repositories(resolvers))
      .noMirrors
      .eitherResult()

  /** Ivy2 local first, then the repositories this build declared. */
  def repositories(declared: List[Resolver]): Seq[coursier.core.Repository] =
    coursier.LocalRepositories.ivy2Local +: BuildResolvers(declared).all.map(toCoursier)

  private def toCoursier(repository: Repository): coursier.core.Repository =
    repository match
      case Repository.Central    => coursier.Repositories.central
      case Repository.IvyLocal   => coursier.LocalRepositories.ivy2Local
      case Repository.Maven(url) => coursier.MavenRepository(url.value)

  private def coordinate(module: MavenModule): String =
    s"${module.group.value}:${module.artifact.value}"
end CoursierStables

enum BumpFault:
  case Unanswered(coordinate: String)
  case Rewrite(error: ShipBump.Rewrite)
  case MissingShip(name: ShipName)
  case Io(path: String)

  def message: String = this match
    case Unanswered(coordinate) => s"$coordinate did not answer"
    case Rewrite(error)         => error.message
    case MissingShip(name)      => s"${name.value} is not a ship in this catalog"
    case Io(path)               => s"could not write $path"
end BumpFault
