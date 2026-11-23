package histos.workflow

import histos.version.CommitSha
import histos.CiSetup
import histos.ProjectGraph

/** The commit of the ship that is rendering the workflow. Tests supply it. A command reads it from the launcher jar. */
opaque type RenderingShip = CommitSha

object RenderingShip:
  def apply(commit: CommitSha): RenderingShip = commit

  /** A commit that is not 40 hex is the same absence as a jar with no toolchain identity. */
  def from(raw: String): Either[CiError, RenderingShip] =
    CommitSha.from(raw).left.map(_ => CiError.NoShip)

  extension (ship: RenderingShip) def commit: CommitSha = ship
end RenderingShip

/** The install steps a job runs before `./histos`. Remote names the rendering ship's action. Wrapper caches, then the
  * job's own `./histos` command.
  */
object CiInstall:
  val Repository: String = "early-effect/histos"
  val ActionPath: String = ".github/actions/setup"

  /** `early-effect/histos/.github/actions/setup@<commit> # <commit>`. The pin scan requires a comment. This action has
    * no release tag, so the comment is the same commit.
    */
  def remote(ship: RenderingShip): String =
    val commit = ship.commit.value
    s"$Repository/$ActionPath@$commit # $commit"

  /** After a full checkout, before a step that renders a workflow. A fetch does not write this ref. */
  val originHead: List[String] =
    List("      - run: git remote set-head origin --auto")

  /** The first job installs the toolchain and saves both caches. */
  def prepare(graph: ProjectGraph)(using ship: RenderingShip): List[String] =
    jdk(graph.runnerTools.jdk) ++ caches(graph, save = true)

  /** A later job restores what [[prepare]] saved and installs only on a miss. */
  def restore(graph: ProjectGraph)(using ship: RenderingShip): List[String] =
    caches(graph, save = false)

  def steps(graph: ProjectGraph)(using ship: RenderingShip): List[String] =
    jdk(graph.runnerTools.jdk) ++ restore(graph)

  def jdk(floor: Option[histos.JdkFloor]): List[String] =
    floor.toList.flatMap: version =>
      List(
        s"      - uses: ${ActionPins.setupJava.line}",
        "        with:",
        "          distribution: temurin",
        s"          java-version: ${quote(version.value.toString)}",
      )

  private def caches(graph: ProjectGraph, save: Boolean)(using ship: RenderingShip): List[String] =
    graph.ciSetup match
      case CiSetup.Remote  => List(s"      - uses: ${remote(ship)}")
      case CiSetup.Wrapper =>
        cache(Toolchains, ToolchainKey, save) ++ cache(Coursier, CoursierKey, save)

  private val Toolchains: String = "~/.histos/toolchains"
  private val Coursier: String   = "~/.cache/coursier/v1"

  private val ToolchainKey: String =
    "histos-toolchains-${{ hashFiles('build.properties') }}-${{ runner.os }}-${{ runner.arch }}"

  private val CoursierKey: String =
    "coursier-${{ hashFiles('dependencies.lock', 'histos.scala') }}-${{ runner.os }}-${{ runner.arch }}"

  private def cache(path: String, key: String, save: Boolean): List[String] =
    val action = if save then ActionPins.cache.line else ActionPins.cacheRestore.line
    List(
      s"      - uses: $action",
      "        with:",
      s"          path: ${quote(path)}",
      s"          key: ${quote(key)}",
    )

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
end CiInstall
