package histos

/** All supported platforms, or an explicitly nonempty selection. */
enum PlatformSelection:
  case All
  case Only(platforms: Platforms)

  def contains(platform: Platform): Boolean = this match
    case All             => true
    case Only(platforms) => platforms.all.contains(platform)

  def selected(supported: Platforms): List[Platform] =
    supported.all.filter(contains).distinct

  /** Named-module selection refuses unsupported platforms; whole-build selection uses [[selected]]. */
  def validate(module: ProjectName, supported: Platforms): Either[SelectionError, List[Platform]] = this match
    case All             => Right(supported.all.distinct)
    case Only(platforms) =>
      platforms.all.find(platform => !supported.all.contains(platform)) match
        case Some(platform) => Left(SelectionError.Unsupported(module, platform))
        case None           => Right(selected(supported))
end PlatformSelection

object PlatformSelection:
  def only(first: Platform, rest: Platform*): PlatformSelection =
    PlatformSelection.Only(Platforms(first, rest.toList.distinct.filterNot(_ == first)))

  def from(values: List[String]): Either[SelectionError, PlatformSelection] =
    TextList
      .parse(values)
      .left
      .map(SelectionError.Text(_))
      .flatMap: entries =>
        entries
          .foldLeft[Either[SelectionError, List[Platform]]](Right(Nil)): (acc, token) =>
            acc.flatMap: found =>
              val platform = token match
                case "jvm"    => Right(Platform.Jvm)
                case "js"     => Right(Platform.Js)
                case "native" => Right(Platform.Native)
                case other    => Left(SelectionError.UnknownPlatform(other))
              platform.map(value => (found :+ value).distinct)
          .flatMap:
            case first :: rest => Right(Only(Platforms(first, rest)))
            case Nil           => Left(SelectionError.Empty)
end PlatformSelection

enum SelectionError:
  case Empty
  case Text(error: TextListError)
  case UnknownPlatform(raw: String)
  case Unsupported(module: ProjectName, platform: Platform)
  case RunMany(platforms: Platforms)
  case RunAmbiguous(module: ProjectName, platforms: Platforms)

/** Repeated flags form a union. Only comma and backslash may be escaped; empty entries are refused. */
object TextList:
  def parse(values: List[String]): Either[TextListError, List[String]] =
    values.foldLeft[Either[TextListError, List[String]]](Right(Nil)): (acc, raw) =>
      acc.flatMap(found => one(raw).map(entries => (found ++ entries).distinct))

  private def one(raw: String): Either[TextListError, List[String]] =
    def walk(chars: List[Char], current: List[Char], entries: List[String]): Either[TextListError, List[String]] =
      chars match
        case Nil =>
          if current.isEmpty then Left(TextListError.EmptyEntry(raw))
          else Right(entries :+ current.reverse.mkString)
        case ',' :: rest =>
          if current.isEmpty then Left(TextListError.EmptyEntry(raw))
          else walk(rest, Nil, entries :+ current.reverse.mkString)
        case '\\' :: Nil             => Left(TextListError.TrailingEscape(raw))
        case '\\' :: escaped :: rest =>
          escaped match
            case ',' | '\\' => walk(rest, escaped :: current, entries)
            case other      => Left(TextListError.InvalidEscape(raw, other))
        case char :: rest => walk(rest, char :: current, entries)
    walk(raw.toList, Nil, Nil)
end TextList

enum TextListError:
  case EmptyEntry(raw: String)
  case TrailingEscape(raw: String)
  case InvalidEscape(raw: String, escaped: Char)

/** Test targets are requested modules, not their compilation dependency closure. */
enum TestTargets:
  case All
  case Named(first: ProjectName, rest: List[ProjectName])

  /** Whole-suite eligibility, without named validation or focused-singleton intent. */
  case Filtered(modules: List[ProjectName])
end TestTargets

/** Affected masks intersect validated cells before grouping, without changing the original target intent. */
final case class TestSelection(
    targets: TestTargets = TestTargets.All,
    platforms: PlatformSelection = PlatformSelection.All,
    affected: Option[List[ProjectName]] = None,
)

object TestSelection:
  def named(names: List[ProjectName], platforms: PlatformSelection = PlatformSelection.All): TestSelection =
    val targets = names match
      case first :: rest => TestTargets.Named(first, rest)
      case Nil           => TestTargets.All
    TestSelection(targets, platforms)

object RunPlatform:
  /** An explicit selection wins over the module default, then a sole supported platform. */
  def resolve(module: Module, explicit: Option[PlatformSelection]): Either[SelectionError, Platform] =
    explicit match
      case Some(selection) =>
        selection
          .validate(module.name, module.platforms)
          .flatMap:
            case List(one)     => Right(one)
            case first :: rest => Left(SelectionError.RunMany(Platforms(first, rest)))
            case Nil           => Left(SelectionError.Empty)
      case None =>
        module.runPlatform match
          case Some(platform) =>
            if module.platforms.all.contains(platform) then Right(platform)
            else Left(SelectionError.Unsupported(module.name, platform))
          case None =>
            module.platforms.all.distinct match
              case List(one)     => Right(one)
              case first :: rest => Left(SelectionError.RunAmbiguous(module.name, Platforms(first, rest)))
              case Nil           => Left(SelectionError.Empty)
