package histos.pack

/** A JVM binary name, such as `demo.Exit`. Empty and whitespace are not names. */
opaque type MainClass = String

object MainClass:
  given CanEqual[MainClass, MainClass] = CanEqual.derived

  private val Name = """[A-Za-z_][A-Za-z0-9_$]*(\.[A-Za-z_][A-Za-z0-9_$]*)*""".r

  def from(raw: String): Either[MainClassError, MainClass] =
    if raw.isEmpty then Left(MainClassError.Empty)
    else if Name.matches(raw) then Right(trusted(raw))
    else Left(MainClassError.Bad(raw))

  /** A name the caller already validated. */
  private[histos] def trusted(raw: String): MainClass = raw

  extension (name: MainClass) def binary: String = name

enum MainClassError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a main class is not empty"
    case Bad(raw) => s"'$raw' is not a main class"
