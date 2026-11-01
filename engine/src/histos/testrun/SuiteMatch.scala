package histos.testrun

/** Matching and the child-runner paste. The pattern value stays data. */
object SuiteMatch:
  /** Empty means every suite. Otherwise one pattern has to match. */
  def allows(patterns: List[SuitePattern], name: String): Boolean =
    patterns.isEmpty || patterns.exists(pattern => hits(pattern, name))

  /** The child runners paste this. They do not reimplement the glob. Each entry is the anchored regex of one pattern.
    */
  def embed(patterns: List[SuitePattern]): String =
    val items = patterns.map(pattern => "\"" + literal(compiled(pattern)) + "\"").mkString(", ")
    s"""private val suitePatterns = Array[String]($items)
       |private def suiteAllowed(name: String): Boolean = {
       |  val shown = if (name.endsWith("$$") && !name.endsWith("$$$$")) name.substring(0, name.length - 1) else name
       |  if (suitePatterns.length == 0) true
       |  else suitePatterns.exists(pattern => shown.matches(pattern) || name.matches(pattern))
       |}
       |""".stripMargin

  private def hits(pattern: SuitePattern, name: String): Boolean =
    val regex = compiled(pattern)
    shown(name).matches(regex) || name.matches(regex)

  private def shown(name: String): String =
    if name.endsWith("$") && !name.endsWith("$$") then name.dropRight(1) else name

  private def compiled(pattern: SuitePattern): String =
    val body = pattern.value.flatMap {
      case '*'             => ".*"
      case '?'             => "."
      case c if special(c) => "\\" + c
      case c               => c.toString
    }
    "^" + body + "$"

  private def special(char: Char): Boolean =
    "\\.+()[]{}^$|".contains(char)

  private def literal(regex: String): String =
    regex.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case char => char.toString
    }
end SuiteMatch
