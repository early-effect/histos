package histos.docs

import histos.cli.Help

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.test.*

/** A command or a flag in `histos help` that no reference page names. */
object HelpCoverageSpec extends ZIOSpecDefault:

  /** The repo root, walking up from the process directory. A test starts in the module directory. */
  private def root: Either[String, Path] =
    val here  = Path.of("").toAbsolutePath.normalize
    val found = Iterator
      .iterate(Option(here))(_.flatMap(dir => Option(dir.getParent)))
      .takeWhile(_.isDefined)
      .flatten
      .find(dir => Files.exists(dir.resolve("histos.scala")))
    found.toRight(s"no histos.scala above $here")

  def spec = suite("help coverage")(
    test("every help entry is named on a reference page") {
      body.map(text => assertTrue(Help.entries.map(_.name).filterNot(mentioned(text, _)).isEmpty))
    },
    test("every help flag is named on a reference page") {
      body.map(text => assertTrue(Help.flags.filterNot(mentioned(text, _)).isEmpty))
    },
  )

  /** The command pages, read as source. The site HTML is not the check. */
  private def body: zio.IO[String, String] =
    zio.ZIO
      .fromEither(root)
      .flatMap: repo =>
        zio.ZIO
          .attemptBlocking {
            Files
              .list(repo.resolve("docs/src/main/scalajvm/histos/docs"))
              .toList
              .asScala
              .filter(_.getFileName.toString.endsWith(".scala"))
              .map(path => Files.readString(path))
              .mkString("\n")
          }
          .mapError(_.toString)

  /** A token counts when a page writes it as a command, a flag, or a heading. */
  private def mentioned(body: String, token: String): Boolean =
    val asFlag = body.contains(s"--$token")
    val asWord = raw"(?i)(^|[^A-Za-z0-9-])${java.util.regex.Pattern.quote(token)}([^A-Za-z0-9-]|$$)".r
      .findFirstIn(body)
      .isDefined
    asFlag || asWord
end HelpCoverageSpec
