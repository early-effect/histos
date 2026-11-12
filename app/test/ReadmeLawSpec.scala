package histos.cli

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** The README teaches commands that exist. A concrete invocation parses. A usage form (`<...>`, `[...]`, `|`, or `...`)
  * names a known command. A relative link names a file in this checkout. Inline code and shell blocks are commands.
  * Output belongs in a `text` block, which is not read.
  */
object ReadmeLawSpec extends ZIOSpecDefault:
  def spec = suite("readme laws")(
    test("every command the README shows parses, and every link it makes exists"):
      for
        root     <- checkout
        text     <- ZIO.attemptBlocking(Files.readString(root.resolve("README.md"))).orDie
        findings <- ReadmeCheck.findings(text, link => Files.exists(root.resolve(link)))
      yield assertTrue(findings == Nil)
    ,
    test("the check names an unparseable command, an unknown usage, and a missing link"):
      val sample =
        """Run `histos compile` and `./histos test --affected '["api"]' api`.
          |`histos update --stable` is the shim. `histos help publish` prints help.
          |`histos compil` is a typo. `histos nope [project ...]` is not a command.
          |See [plugins](docs/plugins.md) and [gone](docs/gone.md) and [web](https://example.com).
          |
          |```sh
          |./histos publish snapshots
          |histos testOnly
          |```
          |
          |```mermaid
          |flowchart TD
          |histos nope
          |```
          |
          |```text
          |histos 0.2.1 is a stable release. histos update --stable switches.
          |```
          |""".stripMargin
      ReadmeCheck
        .findings(sample, link => link == "docs/plugins.md")
        .map: findings =>
          assertTrue(
            findings.map(_.shown) == List(
              "histos compil",
              "histos nope [project ...]",
              "histos testOnly",
              "docs/gone.md",
            )
          ),
  )

  private def checkout: UIO[Path] =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("histos.scala")) && Files.isRegularFile(path.resolve("README.md")) then
        Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    ZIO
      .attemptBlocking(walk(Path.of("").toAbsolutePath.normalize, 8))
      .orDie
      .someOrElseZIO(ZIO.dieMessage("no checkout above the working directory"))
end ReadmeLawSpec

/** What the README check found. */
enum ReadmeFinding:
  case Unparsed(command: String, error: CliParseError)
  case UnknownUsage(command: String)
  case MissingLink(path: String)

  def shown: String = this match
    case Unparsed(command, _)  => command
    case UnknownUsage(command) => command
    case MissingLink(path)     => path
end ReadmeFinding

object ReadmeCheck:
  /** Words the installed shim answers before the jar starts. */
  private val shimWords: List[List[String]] =
    List(List("update"), List("self", "update"), List("toolchain", "install"), List("toolchain", "list"))

  private val Fence    = "^\\s*```\\s*([A-Za-z0-9_-]*)\\s*$".r
  private val Span     = "`([^`\\n]+)`".r
  private val Link     = "\\]\\(([^)\\s]+)\\)".r
  private val Launcher = "^(?:\\$\\s+)?(?:\\./)?histos(?:\\s+(.*))?$".r

  def findings(markdown: String, exists: String => Boolean): UIO[List[ReadmeFinding]] =
    val (prose, blocks) = split(markdown)
    val commands        = (spans(prose) ++ blocks).flatMap(invocation)
    ZIO
      .foreach(commands)(judge)
      .map(found => found.flatten ++ links(prose).filterNot(exists).map(ReadmeFinding.MissingLink(_)))

  private val shells: Set[String] = Set("", "sh", "bash", "shell", "console", "zsh")

  /** Prose with fenced blocks removed, and the lines of every shell block. */
  private def split(markdown: String): (String, List[String]) =
    val start             = (List.empty[String], List.empty[String], Option.empty[String])
    val (prose, lines, _) = markdown.linesIterator.foldLeft(start):
      case ((prose, lines, None), Fence(language))                           => (prose, lines, Some(language))
      case ((prose, lines, None), line)                                      => (line :: prose, lines, None)
      case ((prose, lines, Some(_)), Fence(_))                               => (prose, lines, None)
      case ((prose, lines, open @ Some(language)), line) if shells(language) => (prose, line :: lines, open)
      case ((prose, lines, open @ Some(_)), _)                               => (prose, lines, open)
    (prose.reverse.mkString("\n"), lines.reverse)

  private def spans(prose: String): List[String] =
    Span.findAllMatchIn(prose).map(_.group(1)).toList

  private def links(prose: String): List[String] =
    Link
      .findAllMatchIn(prose)
      .map(_.group(1).takeWhile(_ != '#'))
      .filter(target => target.nonEmpty && !target.contains("://") && !target.startsWith("mailto:"))
      .toList

  /** The text when it is a `histos` or `./histos` invocation. */
  private def invocation(text: String): Option[String] =
    text.trim match
      case found @ Launcher(_) => Some(found)
      case _                   => None

  private def judge(command: String): UIO[Option[ReadmeFinding]] =
    val words = tokens(command).drop(1)
    if words.isEmpty || shim(words) then ZIO.none
    else if words.exists(usage) then
      words match
        case first :: _ if Help.names.contains(first) || shimWords.exists(_.headOption.contains(first)) => ZIO.none
        case _ => ZIO.some(ReadmeFinding.UnknownUsage(command))
    else
      Grammar
        .parse(words)
        .as(Option.empty[ReadmeFinding])
        .catchAll(error => ZIO.some(ReadmeFinding.Unparsed(command, error)))

  private def shim(words: List[String]): Boolean =
    shimWords.exists(prefix => words.startsWith(prefix))

  private def usage(word: String): Boolean =
    word.contains('<') || word.contains('[') || word.contains('|') || word.contains("...")

  /** Whitespace splits words. Single and double quotes group a word and are dropped. */
  private def tokens(text: String): List[String] =
    final case class Walk(done: List[String], word: Option[String], quote: Option[Char])
    val end = text.foldLeft(Walk(Nil, None, None)): (walk, char) =>
      walk.quote match
        case Some(open) if char == open          => walk.copy(quote = None)
        case Some(_)                             => walk.copy(word = Some(walk.word.getOrElse("") + char))
        case None if char == '\'' || char == '"' =>
          walk.copy(word = Some(walk.word.getOrElse("")), quote = Some(char))
        case None if char.isWhitespace => Walk(walk.word.toList ++ walk.done, None, None)
        case None                      => walk.copy(word = Some(walk.word.getOrElse("") + char))
    (end.word.toList ++ end.done).reverse
end ReadmeCheck
