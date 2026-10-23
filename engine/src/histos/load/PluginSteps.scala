package histos.load

import histos.plugin.{DeclaredTask, PluginRunError, PluginTaskId, TaskDep}
import histos.schedule.Histos
import histos.step.{Input, InputName, InputValue, Need, Phase, Step, StepId, StepInfo}

import zio.{Chunk, ZIO}

import Histos.given

/** The reachable plugin tasks, with each `TaskDep.Compile` as a compile step. */
private[load] object PluginSteps:
  private type R = histos.schedule.ActionCache & histos.schedule.RemoteCache & histos.resolve.Resolve & histos.schedule.Scheduler
  private type CellStep = histos.step.HermeticStep[R, PluginFault, String]

  def goal(
      root: PluginTaskId,
      taskOf: PluginTaskId => Option[DeclaredTask],
      cellsOf: DeclaredTask => Either[PluginRunError, List[PluginCells.Cell]],
      compile: PluginCells.Cell => ZIO[histos.resolve.Resolve, PluginFault, Unit],
      runOne: (PluginTaskId, DeclaredTask, String) => ZIO[R, PluginFault, String],
  ): Either[PluginFault, CellStep] =
    val built = scala.collection.mutable.Map.empty[PluginTaskId, CellStep]
    def task(id: PluginTaskId, stack: List[PluginTaskId]): Either[PluginFault, CellStep] =
      built.get(id) match
        case Some(found) => Right(found)
        case None =>
          if stack.contains(id) then Left(PluginFault.Task(PluginRunError.Cycle((id :: stack).reverse.dropWhile(_ != id))))
          else
            val made = make(id, id :: stack)
            made.foreach(step => built.update(id, step))
            made
    def make(id: PluginTaskId, stack: List[PluginTaskId]): Either[PluginFault, CellStep] =
      taskOf(id) match
        case None => Left(PluginFault.Task(PluginRunError.Unknown(id)))
        case Some(declared) =>
          for
            cells <- cellsOf(declared).left.map(PluginFault.Task(_))
            upstreams <- cells.distinct.foldLeft[Either[PluginFault, List[CellStep]]](Right(Nil)): (acc, cell) =>
              acc.map(_ :+ compileStep(cell, compile))
            same = declared.dependsOn.collect:
              case TaskDep.Same(name) => PluginTaskId(id.plugin, name)
            deps <- same.foldLeft[Either[PluginFault, List[CellStep]]](Right(upstreams)): (acc, dep) =>
              acc.flatMap(soFar => task(dep, stack).map(soFar :+ _))
          yield wire(id, declared, deps, runOne)
    task(root, Nil)

  private def compileStep(
      cell: PluginCells.Cell,
      compile: PluginCells.Cell => ZIO[histos.resolve.Resolve, PluginFault, Unit],
  ): CellStep =
    val info = StepInfo(
      StepId.Cell(cell.module.name, cell.version, cell.platform, Phase.Compile),
      Chunk(
        Input(
          InputName("cell"),
          InputValue.Text(s"${cell.module.name.value} ${cell.version.value} ${cell.platform.token}"),
        )
      ),
      Set(Need.Cpu),
    )
    Step.hermetic[String](info)(compile(cell).as(""))

  private def wire(
      id: PluginTaskId,
      task: DeclaredTask,
      upstreams: List[CellStep],
      runOne: (PluginTaskId, DeclaredTask, String) => ZIO[R, PluginFault, String],
  ): CellStep =
    val info = StepInfo(
      StepId.Plugin(id),
      Chunk(Input(InputName("task"), InputValue.Text(id.render))),
      Set(Need.Cpu),
    )
    val body = (upstream: String) => runOne(id, task, upstream)
    val step = upstreams match
      case Nil => Step.hermetic[String](info)(body(""))
      case head :: tail =>
        val joined = tail.foldLeft[CellStep](head)((acc, next) => acc.zip(next).map((left, right) => join(left, right)))
        joined.andThenHermetic[String](info)(body)
    step

  private def join(left: String, right: String): String =
    if left.isEmpty then right
    else if right.isEmpty then left
    else s"$left\n$right"
