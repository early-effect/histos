package histos.splice

import java.nio.file.Path

/** Indexed source maps for prepended wrappers. Offset is exact emitted line count. */
object SourceMaps:

  final case class Section(line: Int, url: String, column: Int = 0)

  /** 0-based line where content after `prefix` begins. Count every `\n`, including blanks. Wrappers we emit end in
    * `\n`, so the next chunk starts at column 0 of that line.
    */
  def lineOffset(prefix: String): Int =
    prefix.count(_ == '\n')

  def indexed(file: String, sections: List[Section]): String =
    val body = sections
      .map { s =>
        s"""{"offset":{"line":${s.line},"column":${s.column}},"url":${JsonText.encode(s.url)}}"""
      }
      .mkString(",")
    s"""{"version":3,"file":${JsonText.encode(file)},"sections":[$body]}"""

  def annotate(js: String, mapFile: String): String =
    val base = if js.endsWith("\n") then js else js + "\n"
    base + s"//# sourceMappingURL=$mapFile\n"

  /** Rewrite a map's `sources` so each path is relative to `toMap`. esbuild records them relative to `fromDir`, the
    * directory of the temporary outfile.
    */
  def retarget(mapJson: String, fromDir: Path, toMap: Path): String =
    JsonText.mapStringArray(mapJson, "sources") { raw =>
      val path = Path.of(raw)
      val abs  = if path.isAbsolute then path.normalize else fromDir.resolve(raw).normalize
      val base = toMap.toAbsolutePath.normalize
      val from = Option(base.getParent).getOrElse(base)
      from.relativize(abs).toString.replace('\\', '/')
    }

  def relativeUrl(fromMap: Path, toMap: Path): String =
    val from = Option(fromMap.toAbsolutePath.normalize.getParent).getOrElse(fromMap.toAbsolutePath.normalize)
    from.relativize(toMap.toAbsolutePath.normalize).toString.replace('\\', '/')

  def mapPath(js: Path): Path =
    js.resolveSibling(js.getFileName.toString + ".map")

  def mapFileName(js: Path): String =
    js.getFileName.toString + ".map"

  def sectionsFor(
      prefix: String,
      linker: List[LinkerFile],
      fromMap: Path,
  ): List[Section] =
    linker.zipWithIndex.flatMap { (file, i) =>
      file.sourceMap.map { mapPath =>
        val before =
          prefix + linker.take(i).map(_.contents).mkString("\n") + (if i > 0 then "\n" else "")
        Section(lineOffset(before), relativeUrl(fromMap, mapPath))
      }
    }

end SourceMaps

/** The bits of JSON this plugin reads. Iterative on purpose: an esbuild metafile for a large link is not a recursion.
  */
private[splice] object JsonText:

  def encode(s: String): String =
    val b = new StringBuilder(s.length + 2)
    b.append('"')
    s.foreach {
      case '"'          => b.append("\\\"")
      case '\\'         => b.append("\\\\")
      case c if c < ' ' =>
        b.append(f"\\u${c.toInt}%04x")
      case c => b.append(c)
    }
    b.append('"')
    b.toString

  /** Keys of the top-level object member `member`. Nested objects of the same name are not that member. */
  def objectKeys(text: String, member: String): List[String] =
    val found     = List.newBuilder[String]
    var i         = 0
    var depth     = 0
    var captureAt = -1
    var arm       = false
    val n         = text.length
    while i < n do
      val c = text.charAt(i)
      if isWs(c) then i += 1
      else if c == '"' then
        readString(text, i) match
          case None            => i = n
          case Some((s, next)) =>
            val after = skipWs(text, next)
            val isKey = after < n && text.charAt(after) == ':'
            if captureAt == depth && isKey then found += s
            else if captureAt < 0 && depth == 1 && isKey && s == member then arm = true
            i = next
      else if c == ':' then i += 1
      else if c == '{' then
        if arm && depth == 1 then
          captureAt = depth + 1
          arm = false
        depth += 1
        i += 1
      else if c == '}' then
        depth -= 1
        if captureAt >= 0 && depth < captureAt then captureAt = -1
        if depth < 0 then i = n else i += 1
      else
        if arm && depth == 1 then arm = false
        i += 1
    found.result()

  /** Rewrite the string elements of the top-level array `member`. The rest of `text` stays. */
  def mapStringArray(text: String, member: String)(f: String => String): String =
    var i     = 0
    var depth = 0
    var arm   = false
    var done  = Option.empty[String]
    val n     = text.length
    while i < n && done.isEmpty do
      val c = text.charAt(i)
      if isWs(c) then i += 1
      else if c == '"' then
        readString(text, i) match
          case None            => done = Some(text)
          case Some((s, next)) =>
            val after = skipWs(text, next)
            val isKey = after < n && text.charAt(after) == ':'
            if depth == 1 && isKey && s == member then arm = true
            i = next
      else if c == ':' then i += 1
      else if c == '[' && arm && depth == 1 then done = Some(rewriteArray(text, i, f))
      else if c == '{' then
        if arm && depth == 1 then arm = false
        depth += 1
        i += 1
      else if c == '}' then
        depth -= 1
        if depth < 0 then done = Some(text) else i += 1
      else
        if arm && depth == 1 then arm = false
        i += 1
    done.getOrElse(text)

  /** Decoded string and the index just after its closing quote. `open` points at the opening quote. */
  def readString(text: String, open: Int): Option[(String, Int)] =
    if open < 0 || open >= text.length || text.charAt(open) != '"' then None
    else
      val b      = new StringBuilder
      var i      = open + 1
      var closed = false
      var bad    = false
      val n      = text.length
      while i < n && !closed && !bad do
        text.charAt(i) match
          case '"' =>
            closed = true
            i += 1
          case '\\' =>
            if i + 1 >= n then bad = true
            else
              text.charAt(i + 1) match
                case '"'  => b.append('"'); i += 2
                case '\\' => b.append('\\'); i += 2
                case '/'  => b.append('/'); i += 2
                case 'n'  => b.append('\n'); i += 2
                case 'r'  => b.append('\r'); i += 2
                case 't'  => b.append('\t'); i += 2
                case 'b'  => b.append('\b'); i += 2
                case 'f'  => b.append('\f'); i += 2
                case 'u'  =>
                  if i + 6 > n then bad = true
                  else
                    hex4(text.substring(i + 2, i + 6)) match
                      case Some(ch) => b.append(ch); i += 6
                      case None     => bad = true
                case _ => bad = true
          case ch =>
            b.append(ch)
            i += 1
      if closed && !bad then Some(b.toString -> i) else None

  private def rewriteArray(text: String, open: Int, f: String => String): String =
    readStringArray(text, open) match
      case None                 => text
      case Some((strings, end)) =>
        val body = strings.map(s => encode(f(s))).mkString("[", ",", "]")
        text.substring(0, open) + body + text.substring(end)

  private def readStringArray(text: String, open: Int): Option[(List[String], Int)] =
    if open >= text.length || text.charAt(open) != '[' then None
    else
      val buf    = List.newBuilder[String]
      var i      = open + 1
      var ok     = true
      var closed = false
      val n      = text.length
      while i < n && ok && !closed do
        val c = text.charAt(i)
        if isWs(c) || c == ',' then i += 1
        else if c == ']' then
          closed = true
          i += 1
        else if c == '"' then
          readString(text, i) match
            case Some((s, next)) =>
              buf += s
              i = next
            case None => ok = false
        else ok = false
      if ok && closed then Some(buf.result() -> i) else None

  private def hex4(s: String): Option[Char] =
    if s.length != 4 then None
    else
      var n  = 0
      var ok = true
      var i  = 0
      while i < 4 && ok do
        val d = s.charAt(i) match
          case c if c >= '0' && c <= '9' => c - '0'
          case c if c >= 'a' && c <= 'f' => c - 'a' + 10
          case c if c >= 'A' && c <= 'F' => c - 'A' + 10
          case _                         => -1
        if d < 0 then ok = false
        else n = n * 16 + d
        i += 1
      if ok then Some(n.toChar) else None

  private def isWs(c: Char): Boolean =
    c == ' ' || c == '\n' || c == '\r' || c == '\t'

  private def skipWs(text: String, from: Int): Int =
    var i = from
    while i < text.length && isWs(text.charAt(i)) do i += 1
    i
end JsonText
