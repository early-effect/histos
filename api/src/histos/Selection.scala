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

/** A series (`2.13`, `3`) or one full version (`2.13.18`). */
enum ScalaToken derives CanEqual:
  case Series2(minor: Int)
  case Series3
  case Exact(version: ScalaVersion)

  def label: String = this match
    case Series2(minor) => s"2.$minor"
    case Series3        => "3"
    case Exact(version) => version.value
end ScalaToken

/** Every declared version, or an explicitly nonempty Scala selection. */
enum ScalaSelection derives CanEqual:
  case All
  case Only(first: ScalaToken, rest: List[ScalaToken])

  def tokens: List[ScalaToken] = this match
    case All               => Nil
    case Only(first, rest) => first :: rest

  /** Every declared version the tokens match. A miss contributes nothing. */
  def filter(declared: List[ScalaVersion]): List[ScalaVersion] = this match
    case All               => declared.distinct
    case Only(first, rest) =>
      ScalaSelection
        .distinctTokens(first :: rest)
        .flatMap(token => declared.filter(version => ScalaSelection.matches(token, version)))
        .distinct

  /** A named module. Each token matches exactly one declared version. */
  def require(module: ProjectName, declared: List[ScalaVersion]): Either[SelectionError, List[ScalaVersion]] =
    this match
      case All               => Right(declared.distinct)
      case Only(first, rest) =>
        ScalaSelection
          .distinctTokens(first :: rest)
          .foldLeft[Either[SelectionError, List[ScalaVersion]]](Right(Nil)): (acc, token) =>
            acc.flatMap: found =>
              declared.filter(ScalaSelection.matches(token, _)).distinct match
                case Nil       => Left(SelectionError.UnsupportedScala(module, token))
                case List(one) => Right(if found.contains(one) then found else found :+ one)
                case many      => Left(SelectionError.AmbiguousScala(module, token, many))
end ScalaSelection

object ScalaSelection:
  private val Series = """2\.(\d+)""".r

  def matches(token: ScalaToken, version: ScalaVersion): Boolean =
    token match
      case ScalaToken.Exact(pinned)  => pinned == version
      case ScalaToken.Series3        => version.value.startsWith("3.")
      case ScalaToken.Series2(minor) => version.value.startsWith(s"2.$minor.")

  def from(values: List[String]): Either[SelectionError, ScalaSelection] =
    TextList
      .parse(values)
      .left
      .map(SelectionError.Text(_))
      .flatMap: entries =>
        entries
          .foldLeft[Either[SelectionError, List[ScalaToken]]](Right(Nil)): (acc, raw) =>
            acc.flatMap: found =>
              token(raw).map: parsed =>
                if found.contains(parsed) then found else found :+ parsed
          .flatMap:
            case first :: rest => Right(ScalaSelection.Only(first, rest))
            case Nil           => Left(SelectionError.Empty)

  def token(raw: String): Either[SelectionError, ScalaToken] =
    raw match
      case "3"            => Right(ScalaToken.Series3)
      case Series(digits) =>
        digits.toIntOption match
          case Some(minor) => Right(ScalaToken.Series2(minor))
          case None        => Left(SelectionError.UnknownScala(raw))
      case other =>
        ScalaVersion.from(other) match
          case Right(version) => Right(ScalaToken.Exact(version))
          case Left(_)        => Left(SelectionError.UnknownScala(other))

  def union(left: ScalaSelection, right: ScalaSelection): ScalaSelection =
    (left, right) match
      case (ScalaSelection.All, chosen)                                          => chosen
      case (chosen, ScalaSelection.All)                                          => chosen
      case (ScalaSelection.Only(first, rest), ScalaSelection.Only(more, others)) =>
        distinctTokens(first :: rest ++ (more :: others)) match
          case head :: tail => ScalaSelection.Only(head, tail)
          case Nil          => ScalaSelection.All

  private[histos] def distinctTokens(tokens: List[ScalaToken]): List[ScalaToken] =
    tokens.foldLeft(List.empty[ScalaToken]): (found, token) =>
      if found.contains(token) then found else found :+ token
end ScalaSelection

enum SelectionError:
  case Empty
  case Text(error: TextListError)
  case UnknownPlatform(raw: String)
  case Unsupported(module: ProjectName, platform: Platform)
  case RunMany(platforms: Platforms)
  case RunAmbiguous(module: ProjectName, platforms: Platforms)
  case UnknownScala(raw: String)
  case UnsupportedScala(module: ProjectName, token: ScalaToken)
  case AmbiguousScala(module: ProjectName, token: ScalaToken, versions: List[ScalaVersion])
  case RunScalaMany(tokens: List[ScalaToken])
  case RunScalaAmbiguous(module: ProjectName, versions: List[ScalaVersion])

  def message: String = this match
    case Empty       => "--platform needs at least one platform"
    case Text(error) =>
      error match
        case TextListError.EmptyEntry(raw)             => s"'$raw' contains an empty list entry"
        case TextListError.TrailingEscape(raw)         => s"'$raw' ends with an incomplete escape"
        case TextListError.InvalidEscape(raw, escaped) =>
          s"'$raw' escapes '$escaped'; only comma and backslash may be escaped"
    case UnknownPlatform(raw)          => s"'$raw' is not jvm, js, or native"
    case Unsupported(module, platform) => s"${module.value} does not support ${platform.token}"
    case RunMany(platforms)            =>
      s"run requires one platform, not ${platforms.all.map(_.token).mkString(",")}"
    case RunAmbiguous(module, platforms) =>
      s"run ${module.value} requires --platform ${platforms.all.map(_.token).mkString("|")}"
    case UnknownScala(raw)                       => s"'$raw' is not a Scala series or a full version"
    case UnsupportedScala(module, token)         => s"${module.value} does not build ${token.label}"
    case AmbiguousScala(module, token, versions) =>
      s"${module.value} ${token.label} matches ${versions.map(_.value).mkString(" and ")}"
    case RunScalaMany(tokens) =>
      s"run requires one Scala version, not ${tokens.map(_.label).mkString(",")}"
    case RunScalaAmbiguous(module, versions) =>
      val choices = versions.map(_.value).mkString("|")
      if choices.isEmpty then s"run ${module.value} has no Scala version"
      else s"run ${module.value} requires --scala $choices"
end SelectionError

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
    scala: ScalaSelection = ScalaSelection.All,
)

object TestSelection:
  def named(
      names: List[ProjectName],
      platforms: PlatformSelection = PlatformSelection.All,
      scala: ScalaSelection = ScalaSelection.All,
  ): TestSelection =
    val targets = names match
      case first :: rest => TestTargets.Named(first, rest)
      case Nil           => TestTargets.All
    TestSelection(targets, platforms, scala = scala)

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

/** One Scala version for a program. Several versions would start several processes. */
object RunScala:
  def resolve(module: Module, explicit: Option[ScalaSelection]): Either[SelectionError, ScalaVersion] =
    val declared = module.versions.all.distinct
    explicit match
      case Some(ScalaSelection.Only(token, Nil)) =>
        declared.filter(ScalaSelection.matches(token, _)) match
          case List(one) => Right(one)
          case Nil       => Left(SelectionError.UnsupportedScala(module.name, token))
          case many      => Left(SelectionError.RunScalaAmbiguous(module.name, many))
      case Some(ScalaSelection.Only(first, rest)) =>
        Left(SelectionError.RunScalaMany(first :: rest))
      case Some(ScalaSelection.All) | None =>
        declared match
          case List(one) => Right(one)
          case Nil       => Left(SelectionError.RunScalaAmbiguous(module.name, Nil))
          case many      => Left(SelectionError.RunScalaAmbiguous(module.name, many))
