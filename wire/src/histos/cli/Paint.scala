package histos.cli

import histos.theme.{Color, Emphasis, Role, Style, Theme}

import zio.Chunk

/** One span of a console line. The role is chosen when the line is built. */
enum Part:
  case Plain(text: String)
  case Mark(text: String, role: Role)

object Part:
  extension (part: Part)
    def text: String = part match
      case Plain(text)   => text
      case Mark(text, _) => text

  def text(parts: Chunk[Part]): String = parts.map(_.text).mkString

  def join(lines: Chunk[Chunk[Part]]): String =
    lines.map(text).mkString("\n")
end Part

/** Whether this process writes ANSI. */
enum Ink:
  case Colour
  case Plain

/** How an RGB color is encoded. A named color ignores this. */
enum Depth:
  case Indexed
  case Truecolor

/** `--color` and `--no-color`. Absent is [[ColorChoice.Default]]. */
enum ColorChoice:
  case Default, On, Off

/** `--truecolor` and `--no-truecolor`. Absent is [[TruecolorChoice.Default]]. */
enum TruecolorChoice:
  case Default, On, Off

/** How far a compile log goes. `Units` is one line per finished source. `Phases` is every compiler phase. */
enum CompileLog:
  case Units
  case Phases

/** The ink, the depth, and the compile log for one process. The theme is not here. */
final case class Appearance(ink: Ink, depth: Depth, log: CompileLog = CompileLog.Units)

object Appearance:
  val plain: Appearance = Appearance(Ink.Plain, Depth.Indexed)

/** Flags peeled off an argv before the command is parsed. The last color flag of a pair wins. Tokens after `--` stay,
  * so a program argument is not a histos flag.
  */
final case class Peeled(
    args: List[String],
    color: ColorChoice,
    truecolor: TruecolorChoice,
    log: CompileLog,
)

object ColorFlag:
  def peel(args: List[String]): Peeled =
    val (ours, program) = args.span(_ != "--")
    val peeled          =
      ours.foldLeft(Peeled(Nil, ColorChoice.Default, TruecolorChoice.Default, CompileLog.Units)): (acc, token) =>
        token match
          case "--color"        => acc.copy(color = ColorChoice.On)
          case "--no-color"     => acc.copy(color = ColorChoice.Off)
          case "--truecolor"    => acc.copy(truecolor = TruecolorChoice.On)
          case "--no-truecolor" => acc.copy(truecolor = TruecolorChoice.Off)
          case "--verbose"      => acc.copy(log = CompileLog.Phases)
          case other            => acc.copy(args = acc.args :+ other)
    peeled.copy(args = peeled.args ++ program)

/** SGR for a line of parts. This is the only writer of an escape. */
object Paint:
  private val Reset = "\u001b[0m"

  def decide(choice: ColorChoice, terminal: Boolean, noColor: Option[String]): Ink =
    choice match
      case ColorChoice.Off     => Ink.Plain
      case ColorChoice.On      => Ink.Colour
      case ColorChoice.Default =>
        noColor match
          case Some(set) if set.nonEmpty => Ink.Plain
          case _                         => if terminal then Ink.Colour else Ink.Plain

  /** `COLORTERM` counts only for `truecolor` and `24bit`. Anything else, including empty, is indexed. */
  def depth(choice: TruecolorChoice, colorTerm: Option[String]): Depth =
    choice match
      case TruecolorChoice.On      => Depth.Truecolor
      case TruecolorChoice.Off     => Depth.Indexed
      case TruecolorChoice.Default =>
        colorTerm match
          case Some("truecolor") | Some("24bit") => Depth.Truecolor
          case _                                 => Depth.Indexed

  def paint(parts: Chunk[Part], theme: Theme, appearance: Appearance): String =
    appearance.ink match
      case Ink.Plain  => Part.text(parts)
      case Ink.Colour =>
        val resolved = theme.orElse(Theme.standard)
        parts.map {
          case Part.Plain(text)      => text
          case Part.Mark(text, role) =>
            resolved(role) match
              case Some(style) => wrap(text, style, appearance.depth)
              case None        => text
        }.mkString

  def paintLines(lines: Chunk[Chunk[Part]], theme: Theme, appearance: Appearance): String =
    lines.map(line => paint(line, theme, appearance)).mkString("\n")

  /** The xterm-256 index of an RGB color: the 6×6×6 cube, or the grayscale ramp when that is nearer. */
  def indexOf(red: Int, green: Int, blue: Int): Int =
    val cubeRed   = level(red)
    val cubeGreen = level(green)
    val cubeBlue  = level(blue)
    val cube      = 16 + 36 * cubeRed + 6 * cubeGreen + cubeBlue
    val gray      = grayIndex(red, green, blue)
    val cubeAt    = (channel(cubeRed), channel(cubeGreen), channel(cubeBlue))
    val grayAt    = 8 + (gray - 232) * 10
    val cubeAway  = distance(red, green, blue, cubeAt._1, cubeAt._2, cubeAt._3)
    val grayAway  = distance(red, green, blue, grayAt, grayAt, grayAt)
    if grayAway < cubeAway then gray else cube

  private def wrap(text: String, style: Style, depth: Depth): String =
    code(style.color, depth) match
      case None            => text
      case Some(colorCode) =>
        val weight = style.emphasis match
          case Emphasis.Plain => ""
          case Emphasis.Bold  => "1;"
          case Emphasis.Dim   => "2;"
        s"\u001b[$weight${colorCode}m$text$Reset"

  private def code(color: Color, depth: Depth): Option[String] =
    color match
      case Color.Black                                                                          => Some("30")
      case Color.Red                                                                            => Some("31")
      case Color.Green                                                                          => Some("32")
      case Color.Yellow                                                                         => Some("33")
      case Color.Blue                                                                           => Some("34")
      case Color.Magenta                                                                        => Some("35")
      case Color.Cyan                                                                           => Some("36")
      case Color.White                                                                          => Some("37")
      case Color.BrightBlack                                                                    => Some("90")
      case Color.BrightRed                                                                      => Some("91")
      case Color.BrightGreen                                                                    => Some("92")
      case Color.BrightYellow                                                                   => Some("93")
      case Color.BrightBlue                                                                     => Some("94")
      case Color.BrightMagenta                                                                  => Some("95")
      case Color.BrightCyan                                                                     => Some("96")
      case Color.BrightWhite                                                                    => Some("97")
      case Color.Index(value) if value >= 0 && value <= 255                                     => Some(s"38;5;$value")
      case Color.Index(_)                                                                       => None
      case Color.Rgb(red, green, blue) if channelOk(red) && channelOk(green) && channelOk(blue) =>
        depth match
          case Depth.Truecolor => Some(s"38;2;$red;$green;$blue")
          case Depth.Indexed   => Some(s"38;5;${indexOf(red, green, blue)}")
      case Color.Rgb(_, _, _) => None

  private def channelOk(value: Int): Boolean = value >= 0 && value <= 255

  private def level(value: Int): Int =
    if value < 48 then 0
    else if value < 114 then 1
    else (value - 35) / 40

  /** Cube steps are 0, 95, 135, 175, 215, 255. */
  private def channel(step: Int): Int =
    if step == 0 then 0 else 55 + step * 40

  private def grayIndex(red: Int, green: Int, blue: Int): Int =
    val first      = distance(red, green, blue, 8, 8, 8)
    val (index, _) = (1 to 23).foldLeft((232, first)):
      case ((best, away), step) =>
        val value = 8 + step * 10
        val next  = distance(red, green, blue, value, value, value)
        if next < away then (232 + step, next) else (best, away)
    index

  private def distance(red: Int, green: Int, blue: Int, toRed: Int, toGreen: Int, toBlue: Int): Int =
    val dr = red - toRed
    val dg = green - toGreen
    val db = blue - toBlue
    dr * dr + dg * dg + db * db
end Paint
