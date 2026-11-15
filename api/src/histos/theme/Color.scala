package histos.theme

/** A console color. A named color follows the terminal's palette. [[Color.indexed]] and [[Color.rgb]] check a literal.
  * [[Color.fromIndex]] and [[Color.fromRgb]] are the runtime constructors.
  */
enum Color:
  case Black, Red, Green, Yellow, Blue, Magenta, Cyan, White
  case BrightBlack, BrightRed, BrightGreen, BrightYellow, BrightBlue, BrightMagenta, BrightCyan, BrightWhite
  case Index(value: Int)
  case Rgb(red: Int, green: Int, blue: Int)

object Color:
  inline def indexed(inline n: Int): Color =
    inline if n >= 0 && n <= 255 then Index(n)
    else scala.compiletime.error("a color index is from 0 to 255")

  inline def rgb(inline red: Int, inline green: Int, inline blue: Int): Color =
    inline if red >= 0 && red <= 255 && green >= 0 && green <= 255 && blue >= 0 && blue <= 255 then
      Rgb(red, green, blue)
    else scala.compiletime.error("a color channel is from 0 to 255")

  def fromIndex(value: Int): Either[ColorError, Color] =
    if value >= 0 && value <= 255 then Right(Index(value))
    else Left(ColorError.Index(value))

  def fromRgb(red: Int, green: Int, blue: Int): Either[ColorError, Color] =
    for
      shownRed   <- channel(red)
      shownGreen <- channel(green)
      shownBlue  <- channel(blue)
    yield Rgb(shownRed, shownGreen, shownBlue)

  /** The wire token. A named color stays a name. An index is `index:n`. Truecolor is `rgb:r,g,b`. */
  def token(color: Color): String =
    color match
      case Black                 => "black"
      case Red                   => "red"
      case Green                 => "green"
      case Yellow                => "yellow"
      case Blue                  => "blue"
      case Magenta               => "magenta"
      case Cyan                  => "cyan"
      case White                 => "white"
      case BrightBlack           => "bright-black"
      case BrightRed             => "bright-red"
      case BrightGreen           => "bright-green"
      case BrightYellow          => "bright-yellow"
      case BrightBlue            => "bright-blue"
      case BrightMagenta         => "bright-magenta"
      case BrightCyan            => "bright-cyan"
      case BrightWhite           => "bright-white"
      case Index(value)          => s"index:$value"
      case Rgb(red, green, blue) => s"rgb:$red,$green,$blue"

  def fromToken(raw: String): Either[ColorError, Color] =
    raw match
      case "black"          => Right(Black)
      case "red"            => Right(Red)
      case "green"          => Right(Green)
      case "yellow"         => Right(Yellow)
      case "blue"           => Right(Blue)
      case "magenta"        => Right(Magenta)
      case "cyan"           => Right(Cyan)
      case "white"          => Right(White)
      case "bright-black"   => Right(BrightBlack)
      case "bright-red"     => Right(BrightRed)
      case "bright-green"   => Right(BrightGreen)
      case "bright-yellow"  => Right(BrightYellow)
      case "bright-blue"    => Right(BrightBlue)
      case "bright-magenta" => Right(BrightMagenta)
      case "bright-cyan"    => Right(BrightCyan)
      case "bright-white"   => Right(BrightWhite)
      case other            =>
        if other.startsWith("index:") then indexToken(other.drop("index:".length))
        else if other.startsWith("rgb:") then rgbToken(other.drop("rgb:".length))
        else Left(ColorError.Token(raw))

  private def indexToken(raw: String): Either[ColorError, Color] =
    raw.toIntOption match
      case Some(value) => fromIndex(value)
      case None        => Left(ColorError.Token(s"index:$raw"))

  private def rgbToken(raw: String): Either[ColorError, Color] =
    raw.split(",", -1).toList match
      case red :: green :: blue :: Nil =>
        (red.toIntOption, green.toIntOption, blue.toIntOption) match
          case (Some(shownRed), Some(shownGreen), Some(shownBlue)) =>
            fromRgb(shownRed, shownGreen, shownBlue)
          case _ => Left(ColorError.Token(s"rgb:$raw"))
      case _ => Left(ColorError.Token(s"rgb:$raw"))

  private def channel(value: Int): Either[ColorError, Int] =
    if value >= 0 && value <= 255 then Right(value)
    else Left(ColorError.Channel(value))
end Color

/** A rejected index or channel. The token case is a wire color this palette does not name. */
enum ColorError:
  case Index(value: Int)
  case Channel(value: Int)
  case Token(raw: String)

  def message: String = this match
    case Index(value)   => s"$value is not a color index"
    case Channel(value) => s"$value is not a color channel"
    case Token(raw)     => s"'$raw' is not a color"
end ColorError
