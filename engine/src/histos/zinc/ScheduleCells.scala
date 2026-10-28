package histos.zinc

import histos.jdk.JdkRuntime
import histos.schedule.Histos
import histos.step.{Input, InputName, InputValue, Need, Phase, Step, StepId, StepInfo}
import histos.{Module, Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.{Chunk, UIO, ZIO}

import Histos.given

/** Shared inputs for every cell in one compile. */
final case class CompilePlan(
    repo: Path,
    modules: List[Module],
    lock: histos.resolve.DependencyLock,
    jdk: JdkRuntime,
    native: histos.native.NativeOrigin,
    repositories: histos.resolve.Repositories,
    pool: CompilerPool,
    tools: histos.resolve.LockedTools,
)

/** One cell, the class directories it may use, and where its feeds go. */
private[zinc] final case class CellRun(
    plan: CompilePlan,
    module: Module,
    version: ScalaVersion,
    platform: Platform,
    upstreams: Map[ScheduleCells.CellKey, Path],
    hold: ProjectName => UIO[Unit],
    offer: CompileFeed => Unit,
)

private[zinc] object ScheduleCells:
  private[zinc] type CellKey = (ProjectName, ScalaVersion, Platform)
  private type CellStep      =
    histos.step.HermeticStep[GraphCompile.Fed, GraphCompileError, String]

  /** One step per cell. A cell's upstreams are the cells it depends on, at its own version and platform.
    */
  def goal(
      plan: CompilePlan,
      selected: List[Module],
      only: Option[Set[String]],
      platform: Option[Platform],
      hold: ProjectName => UIO[Unit],
      offer: CompileFeed => Unit,
  ): CellStep =
    val names                        = selected.map(_.name).toSet
    val byName                       = selected.map(module => module.name -> module).toMap
    val keys                         = selected.flatMap(module => keysOf(module, only, platform))
    val declared                     = keys.toSet
    val built                        = scala.collection.mutable.Map.empty[CellKey, CellStep]
    def cell(key: CellKey): CellStep =
      built.get(key) match
        case Some(found) => found
        case None        =>
          val made = make(key)
          built.update(key, made)
          made
    def make(key: CellKey): CellStep =
      val (name, version, chosen) = key
      val info                    = StepInfo(
        StepId.Cell(name, version, chosen, Phase.Compile),
        Chunk(
          Input(
            InputName("cell"),
            InputValue.Text(s"${name.value} ${version.value} ${chosen.token}"),
          )
        ) ++ Chunk(
          Input(
            InputName("jdk"),
            InputValue.Text(
              histos.toolchain.Runtimes.identityToken(plan.jdk.identity)
            ),
          ),
          Input(
            InputName("tools"),
            InputValue.Text(
              byName
                .get(name)
                .toList
                .flatMap(module =>
                  plan.tools
                    .declaredCompilation(module, version, chosen, plan.native)
                )
                .mkString("\n")
            ),
          ),
        ),
        Set(Need.Cpu),
      )
      val body = (packed: String) =>
        byName.get(name) match
          case None         => ZIO.fail(GraphCompileError.UnknownCell(name, version))
          case Some(module) =>
            GraphCompile.compileCell(
              CellRun(plan, module, version, chosen, parse(packed), hold, offer)
            )
      dependencies(key, byName, names, declared) match
        case Nil  => Step.hermetic[String](info)(body(""))
        case deps => combine(deps).andThenHermetic[String](info)(body)
    def combine(deps: List[CellKey]): CellStep =
      deps match
        case head :: Nil  => cell(head)
        case head :: tail =>
          cell(head).zip(combine(tail)).map { case (left, right) =>
            join(left, right)
          }
        case Nil =>
          Step.hermetic[String](
            StepInfo(StepId.Work("none"), Chunk.empty, Set(Need.Cpu))
          )(ZIO.succeed(""))
    keys match
      case Nil =>
        Step.hermetic[String](
          StepInfo(StepId.Work("none"), Chunk.empty, Set(Need.Cpu))
        )(ZIO.succeed(""))
      case head :: tail =>
        tail.foldLeft(cell(head))((acc, key) => acc.zip(cell(key)).map { case (left, right) => join(left, right) })

  /** Class directories accumulated for one cell, one line per upstream. */
  private[zinc] def render(done: Map[CellKey, Path]): String =
    done.toList
      .sortBy { case ((name, version, chosen), _) =>
        (name.value, version.value, chosen.token)
      }
      .map { case ((name, version, chosen), path) =>
        List(name.value, version.value, chosen.token, path.toString).mkString(
          "\t"
        )
      }
      .mkString("\n")

  private def join(left: String, right: String): String =
    if left.isEmpty then right
    else if right.isEmpty then left
    else s"$left\n$right"

  /** A dependency that does not declare this version and platform is not a step. The cell reports it missing.
    */
  private def dependencies(
      key: CellKey,
      byName: Map[ProjectName, Module],
      names: Set[ProjectName],
      declared: Set[CellKey],
  ): List[CellKey] =
    val (name, version, chosen) = key
    byName
      .get(name)
      .toList
      .flatMap(_.upstream(chosen))
      .filter(names.contains)
      .flatMap: dependency =>
        val next = (dependency, version, chosen)
        if declared.contains(next) then List(next) else Nil

  private def keysOf(
      module: Module,
      only: Option[Set[String]],
      platform: Option[Platform],
  ): List[CellKey] =
    val versions = only match
      case None       => module.versions.all
      case Some(keep) =>
        module.versions.all.filter(version => keep.contains(version.value))
    val built = List(Platform.Jvm, Platform.Js, Platform.Native).filter(
      module.platforms.all.contains
    )
    val targets = platform match
      case None         => built
      case Some(chosen) => built.filter(_ == chosen)
    versions
      .sortBy(_.value)
      .flatMap(version => targets.map(chosen => (module.name, version, chosen)))

  private def parse(packed: String): Map[CellKey, Path] =
    if packed.isEmpty then Map.empty
    else packed.split('\n').toList.flatMap(decode).toMap

  private def decode(line: String): Option[(CellKey, Path)] =
    line.split("\t", 4).toList match
      case name :: version :: token :: path :: Nil =>
        (
          ProjectName.from(name),
          ScalaVersion.from(version),
          Platform.from(token),
        ) match
          case (Right(module), Right(scalaVersion), Right(chosen)) =>
            Some(((module, scalaVersion, chosen), Path.of(path)))
          case _ => None
      case _ => None
end ScheduleCells
