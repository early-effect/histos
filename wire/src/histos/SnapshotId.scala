package histos

import histos.version.{CommitSha, ShipNumber, SnapshotCoordinate}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** The revision a ship publishes to Coursier: `<ship>-<12 hex>-SNAPSHOT`. Assembly and every artifact of that publish
  * use this string. A checkout with no `app` ship, or no commit, has none.
  */
object SnapshotId:
  def current(repo: Path): UIO[Option[String]] =
    ZIO
      .attemptBlocking {
        val file = repo.resolve("histos.scala")
        if !Files.isRegularFile(file) then None
        else
          val text = Files.readString(file, StandardCharsets.UTF_8)
          publishing(text).flatMap(number => commit(repo).map(sha => render(number, sha)))
      }
      .orElseSucceed(None)

  def render(number: ShipNumber, sha: CommitSha): String =
    SnapshotCoordinate(number, sha.abbreviate).render

  /** The ship whose members include `app`. That is the assembly this checkout launches. */
  private[histos] def publishing(text: String): Option[ShipNumber] =
    scan(text, 0)

  private def scan(text: String, from: Int): Option[ShipNumber] =
    val at = text.indexOf("Ship(\"", from)
    if at < 0 then None
    else
      row(text, at) match
        case Some(number) => Some(number)
        case None         => scan(text, at + 6)

  private def row(text: String, at: Int): Option[ShipNumber] =
    val open  = at + 6
    val close = text.indexOf('"', open)
    if close < 0 then None
    else
      val closeName = skip(text, close + 1)
      if closeName >= text.length || text.charAt(closeName) != ')' then None
      else
        val after = skip(text, closeName + 1)
        if !text.startsWith(".next", after) then None
        else
          val paren = skip(text, after + 5)
          if paren >= text.length || text.charAt(paren) != '(' then None
          else
            triple(text, paren + 1).flatMap: (number, end) =>
              val closeNumber = skip(text, end)
              if closeNumber >= text.length || text.charAt(closeNumber) != ')' then None
              else
                val openMembers = skip(text, closeNumber + 1)
                if openMembers >= text.length || text.charAt(openMembers) != '(' then None
                else
                  val membersEnd = text.indexOf(')', openMembers + 1)
                  if membersEnd < 0 then None
                  else
                    val members = text.substring(openMembers + 1, membersEnd).split(',').map(_.trim).toList
                    if members.contains("app") then Some(number) else None

  private def triple(text: String, from: Int): Option[(ShipNumber, Int)] =
    component(text, from).flatMap: (major, afterMajor) =>
      comma(text, afterMajor).flatMap: minorAt =>
        component(text, minorAt).flatMap: (minor, afterMinor) =>
          comma(text, afterMinor).flatMap: patchAt =>
            component(text, patchAt).flatMap: (patch, afterPatch) =>
              ShipNumber.from(s"$major.$minor.$patch").toOption.map(number => (number, afterPatch))

  private def comma(text: String, from: Int): Option[Int] =
    val at = skip(text, from)
    if at < text.length && text.charAt(at) == ',' then Some(skip(text, at + 1)) else None

  private def component(text: String, from: Int): Option[(String, Int)] =
    val start       = skip(text, from)
    val (raw, next) = digits(text, start, "")
    if raw.isEmpty then None else Some((raw, next))

  private def digits(text: String, at: Int, acc: String): (String, Int) =
    if at < text.length && text.charAt(at).isDigit then digits(text, at + 1, acc + text.charAt(at))
    else (acc, at)

  private def skip(text: String, at: Int): Int =
    if at < text.length && text.charAt(at).isWhitespace then skip(text, at + 1) else at

  private def commit(repo: Path): Option[CommitSha] =
    val process = new ProcessBuilder("git", "-C", repo.toString, "rev-parse", "HEAD")
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
    if process.waitFor() == 0 then CommitSha.from(text).toOption else None
end SnapshotId
