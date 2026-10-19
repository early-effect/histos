package histos.plugin

import java.util.Locale

/** Shared grammars for the plugin opaques. Not a public parser. */
private[histos] object Tokens:
  def simple(raw: String): Boolean =
    raw.headOption match
      case Some(head) if head.isLetter => raw.forall(char => char.isLetterOrDigit || char == '-')
      case _                           => false

  def parseSimple[E](raw: String, empty: => E, bad: String => E): Either[E, String] =
    if simple(raw) then Right(raw)
    else if raw.isEmpty then Left(empty)
    else Left(bad(raw))

  /** Dot-separated segments, each a simple token. No empty, leading, or trailing dot. */
  def pluginId(raw: String): Boolean =
    raw.nonEmpty &&
      !raw.startsWith(".") &&
      !raw.endsWith(".") &&
      !raw.contains("..") &&
      !raw.contains('/') &&
      !raw.contains(':') &&
      !raw.exists(_.isWhitespace) &&
      raw.split("\\.", -1).forall(simple)

  def entrySegment(part: String): Boolean =
    if part.isEmpty || part.startsWith("$") || part.contains("$$") then false
    else
      val body = if part.endsWith("$") then part.dropRight(1) else part
      body.nonEmpty && body.split("\\$", -1).forall(ident)

  def entry(raw: String): Boolean =
    raw.nonEmpty &&
      !raw.startsWith(".") &&
      !raw.endsWith(".") &&
      !raw.contains("..") &&
      !raw.exists(_.isWhitespace) &&
      !raw.contains('/') &&
      !raw.contains(':') &&
      raw.split("\\.", -1).forall(entrySegment)

  def env(raw: String): Boolean =
    raw.headOption match
      case Some(head) if head.isLetter || head == '_' => raw.forall(char => char.isLetterOrDigit || char == '_')
      case _                                          => false

  /** Non-empty single line. Plugins encode richer settings without a newline. */
  def plain(raw: String): Boolean =
    raw.nonEmpty && !raw.exists(_.isControl)

  def inputKey(raw: String): Boolean =
    raw.nonEmpty && !raw.exists(_.isWhitespace)

  def repoSegment(part: String): Boolean =
    part.headOption match
      case Some(head) if head.isLetter =>
        part.forall(char => char.isLetterOrDigit || char == '.' || char == '_' || char == '-')
      case _ => false

  /** Lowercase sha256, or None when the text is not 64 hex digits. */
  def sha256(raw: String): Option[String] =
    val hex = raw.toLowerCase(Locale.ROOT)
    if raw.length == 64 && hex.forall(isHex) then Some(hex) else None

  private def ident(part: String): Boolean =
    part.headOption match
      case Some(head) if head.isLetter || head == '_' => part.forall(char => char.isLetterOrDigit || char == '_')
      case _                                          => false

  private def isHex(char: Char): Boolean =
    char.isDigit || (char >= 'a' && char <= 'f')
