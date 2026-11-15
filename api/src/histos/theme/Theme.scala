package histos.theme

/** What a console span is. The line builder picks it. The theme picks the style. */
enum Role:
  case Compiling, Pulse, Unchanged, Cached
  case Passed, Failed, NoTests, Unmatched
  case Warn, Error
  case TallyOk, TallyBad

  def token: String = this match
    case Compiling => "compiling"
    case Pulse     => "pulse"
    case Unchanged => "unchanged"
    case Cached    => "cached"
    case Passed    => "passed"
    case Failed    => "failed"
    case NoTests   => "no-tests"
    case Unmatched => "unmatched"
    case Warn      => "warn"
    case Error     => "error"
    case TallyOk   => "tally-ok"
    case TallyBad  => "tally-bad"
end Role

object Role:
  def fromToken(raw: String): Option[Role] =
    raw match
      case "compiling" => Some(Role.Compiling)
      case "pulse"     => Some(Role.Pulse)
      case "unchanged" => Some(Role.Unchanged)
      case "cached"    => Some(Role.Cached)
      case "passed"    => Some(Role.Passed)
      case "failed"    => Some(Role.Failed)
      case "no-tests"  => Some(Role.NoTests)
      case "unmatched" => Some(Role.Unmatched)
      case "warn"      => Some(Role.Warn)
      case "error"     => Some(Role.Error)
      case "tally-ok"  => Some(Role.TallyOk)
      case "tally-bad" => Some(Role.TallyBad)
      case _           => None

/** Weight on top of a color. Dim still has a hue. */
enum Emphasis:
  case Plain, Bold, Dim

  def token: String = this match
    case Plain => "plain"
    case Bold  => "bold"
    case Dim   => "dim"
end Emphasis

object Emphasis:
  def fromToken(raw: String): Option[Emphasis] =
    raw match
      case "plain" => Some(Emphasis.Plain)
      case "bold"  => Some(Emphasis.Bold)
      case "dim"   => Some(Emphasis.Dim)
      case _       => None

/** One role's ink. The color is required. */
final case class Style(color: Color, emphasis: Emphasis)

/** Styles for the roles a build wants to replace. An absent role keeps the base theme.
  *
  * [[Theme.empty]] is not the off switch. A partial theme is legal. The printer fills the gaps from [[Theme.standard]].
  */
final case class Theme(
    compiling: Option[Style] = None,
    pulse: Option[Style] = None,
    unchanged: Option[Style] = None,
    cached: Option[Style] = None,
    passed: Option[Style] = None,
    failed: Option[Style] = None,
    noTests: Option[Style] = None,
    unmatched: Option[Style] = None,
    warn: Option[Style] = None,
    error: Option[Style] = None,
    tallyOk: Option[Style] = None,
    tallyBad: Option[Style] = None,
):
  def apply(role: Role): Option[Style] =
    role match
      case Role.Compiling => compiling
      case Role.Pulse     => pulse
      case Role.Unchanged => unchanged
      case Role.Cached    => cached
      case Role.Passed    => passed
      case Role.Failed    => failed
      case Role.NoTests   => noTests
      case Role.Unmatched => unmatched
      case Role.Warn      => warn
      case Role.Error     => error
      case Role.TallyOk   => tallyOk
      case Role.TallyBad  => tallyBad

  def set(role: Role, style: Style): Theme =
    role match
      case Role.Compiling => copy(compiling = Some(style))
      case Role.Pulse     => copy(pulse = Some(style))
      case Role.Unchanged => copy(unchanged = Some(style))
      case Role.Cached    => copy(cached = Some(style))
      case Role.Passed    => copy(passed = Some(style))
      case Role.Failed    => copy(failed = Some(style))
      case Role.NoTests   => copy(noTests = Some(style))
      case Role.Unmatched => copy(unmatched = Some(style))
      case Role.Warn      => copy(warn = Some(style))
      case Role.Error     => copy(error = Some(style))
      case Role.TallyOk   => copy(tallyOk = Some(style))
      case Role.TallyBad  => copy(tallyBad = Some(style))

  /** Roles in this theme win. Gaps come from `base`. */
  def orElse(base: Theme): Theme =
    Theme(
      compiling = compiling.orElse(base.compiling),
      pulse = pulse.orElse(base.pulse),
      unchanged = unchanged.orElse(base.unchanged),
      cached = cached.orElse(base.cached),
      passed = passed.orElse(base.passed),
      failed = failed.orElse(base.failed),
      noTests = noTests.orElse(base.noTests),
      unmatched = unmatched.orElse(base.unmatched),
      warn = warn.orElse(base.warn),
      error = error.orElse(base.error),
      tallyOk = tallyOk.orElse(base.tallyOk),
      tallyBad = tallyBad.orElse(base.tallyBad),
    )
end Theme

object Theme:
  private val cyan      = Style(Color.Cyan, Emphasis.Plain)
  private val dimCyan   = Style(Color.Cyan, Emphasis.Dim)
  private val green     = Style(Color.Green, Emphasis.Plain)
  private val greenBold = Style(Color.Green, Emphasis.Bold)
  private val red       = Style(Color.Red, Emphasis.Plain)
  private val redBold   = Style(Color.Red, Emphasis.Bold)
  private val yellow    = Style(Color.Yellow, Emphasis.Plain)

  /** Cyan for work in progress, dim cyan for a cache hit, green for a pass, red for a failure, yellow for a warning. */
  val standard: Theme = Theme(
    compiling = Some(cyan),
    pulse = Some(cyan),
    unchanged = Some(dimCyan),
    cached = Some(dimCyan),
    passed = Some(green),
    failed = Some(red),
    noTests = Some(yellow),
    unmatched = Some(yellow),
    warn = Some(yellow),
    error = Some(red),
    tallyOk = Some(greenBold),
    tallyBad = Some(redBold),
  )

  val empty: Theme = Theme()
end Theme
