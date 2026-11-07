package histos

/** The six floor options as zio-cli read them, before the 0 to 100 check. */
final case class FloorInput(
    statements: Option[BigInt] = None,
    branches: Option[BigInt] = None,
    fileStatements: Option[BigInt] = None,
    fileBranches: Option[BigInt] = None,
    packageStatements: Option[BigInt] = None,
    packageBranches: Option[BigInt] = None,
)

/** Opt-in floors. `None` does not fail the command. Each set floor is an integer from 0 to 100. */
final case class CoverageFloors(
    statements: Option[Int] = None,
    branches: Option[Int] = None,
    fileStatements: Option[Int] = None,
    fileBranches: Option[Int] = None,
    packageStatements: Option[Int] = None,
    packageBranches: Option[Int] = None,
):
  def argv: List[String] =
    flag("--min-stmt", statements) ++
      flag("--min-branch", branches) ++
      flag("--min-stmt-file", fileStatements) ++
      flag("--min-branch-file", fileBranches) ++
      flag("--min-stmt-package", packageStatements) ++
      flag("--min-branch-package", packageBranches)

  private def flag(name: String, value: Option[Int]): List[String] =
    value.toList.flatMap(number => List(name, number.toString))
end CoverageFloors

/** One of the six floor options. The token is the flag the command prints. */
enum FloorName:
  case Statements, Branches, FileStatements, FileBranches, PackageStatements, PackageBranches

  def token: String = this match
    case Statements        => "--min-stmt"
    case Branches          => "--min-branch"
    case FileStatements    => "--min-stmt-file"
    case FileBranches      => "--min-branch-file"
    case PackageStatements => "--min-stmt-package"
    case PackageBranches   => "--min-branch-package"
end FloorName

object FloorName:
  def known(raw: String): Boolean =
    FloorName.values.exists(_.token == raw)

object CoverageFloors:
  def parse(raw: FloorInput): Either[CoverageFloorError, CoverageFloors] =
    for
      statements        <- one(FloorName.Statements, raw.statements)
      branches          <- one(FloorName.Branches, raw.branches)
      fileStatements    <- one(FloorName.FileStatements, raw.fileStatements)
      fileBranches      <- one(FloorName.FileBranches, raw.fileBranches)
      packageStatements <- one(FloorName.PackageStatements, raw.packageStatements)
      packageBranches   <- one(FloorName.PackageBranches, raw.packageBranches)
    yield CoverageFloors(
      statements,
      branches,
      fileStatements,
      fileBranches,
      packageStatements,
      packageBranches,
    )

  private def one(flag: FloorName, value: Option[BigInt]): Either[CoverageFloorError, Option[Int]] =
    value match
      case None                                         => Right(None)
      case Some(number) if number >= 0 && number <= 100 =>
        Right(Some(number.toInt))
      case Some(number) => Left(CoverageFloorError.OutOfRange(flag, number))
end CoverageFloors

enum CoverageFloorError:
  case OutOfRange(flag: FloorName, value: BigInt)

  def message: String = this match
    case OutOfRange(flag, value) => s"${flag.token} is $value, and a floor is an integer from 0 to 100"
end CoverageFloorError

/** Whether CI runs `histos coverage`. Off unless a build asks. A floor on the ask is a flag on the job. */
enum CoverageJob:
  case Off
  case Ask(floors: CoverageFloors)
