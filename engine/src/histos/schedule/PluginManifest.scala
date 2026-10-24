package histos.schedule

import histos.jdk.Sha256
import histos.plugin.RepoPath

import java.nio.file.{Files, LinkOption, Path}

import zio.{Chunk, UIO, ZIO}

/** What a plugin task stores. No declared outputs stores `PluginOutput.text`
  * unchanged, and that text is a legal hit. Declared outputs store a header a
  * task's own text does not have to avoid, then one sha256 line per file. The
  * output bytes stay in the work tree. The blob is this text.
  */
object PluginManifest:
  val Header: String = "histos-manifest v1"

  def store(text: String, files: List[(RepoPath, Sha256)]): String =
    if files.isEmpty then text
    else
      val lines = files.map((path, sha) => s"${sha.value} ${path.value}")
      (Header :: lines.size.toString :: lines ::: List(text)).mkString("\n")

  /** The task text under a file manifest. A text hit returns the stored string.
    */
  def text(stored: String): String =
    read(stored) match
      case Some(listed) => listed.text
      case None         => stored

  /** A plugin hit still has to have every declared file, with the sha256 the
    * manifest recorded. No declared outputs skips the check. A manifest that
    * does not parse is a miss.
    */
  def accept(
      stored: String,
      root: Path,
      outputs: List[RepoPath]
  ): UIO[Boolean] =
    if outputs.isEmpty then ZIO.succeed(true)
    else
      ZIO.attemptBlocking(matches(stored, root, outputs)).orElseSucceed(false)

  private final case class Listed(files: List[(String, String)], text: String)

  private def matches(
      stored: String,
      root: Path,
      outputs: List[RepoPath]
  ): Boolean =
    read(stored) match
      case None         => false
      case Some(listed) =>
        val expected = outputs.map(_.value).toSet
        val found = listed.files.map(_._2).toSet
        expected == found && listed.files.forall((sha, path) =>
          same(root, sha, path)
        )

  private def same(root: Path, sha: String, path: String): Boolean =
    val file = root.resolve(path)
    Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) &&
    Sha256.of(Chunk.fromArray(Files.readAllBytes(file))).value == sha

  private def read(stored: String): Option[Listed] =
    stored.split("\n", -1).toList match
      case Header :: countText :: rest =>
        countText.toIntOption match
          case Some(count) if count >= 0 && rest.length >= count =>
            val (listed, body) = rest.splitAt(count)
            val files =
              listed.foldLeft[Option[List[(String, String)]]](Some(Nil)):
                (acc, line) =>
                  acc.flatMap: soFar =>
                    line.split(" ", 2).toList match
                      case sha :: path :: Nil =>
                        Sha256
                          .from(sha)
                          .toOption
                          .flatMap: parsed =>
                            RepoPath
                              .from(path)
                              .toOption
                              .map(ok => soFar :+ (parsed.value -> ok.value))
                      case _ => None
            files.map(parsed => Listed(parsed, body.mkString("\n")))
          case _ => None
      case _ => None
