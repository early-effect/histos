package histos.load

import histos.cache.ContentStore
import histos.plugin.{DeclaredTask, PluginId, PluginRunError, PluginTaskId, Seal, TaskDep, TaskName}
import histos.resolve.Resolve
import histos.step.{InputName, InputValue, Need as StepNeed, Phase, Step, StepId, StepInfo}
import histos.{Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import slayer.stubbed

import zio.{Ref, ZIO}
import zio.test.*

object PluginStepsLawSpec extends ZIOSpecDefault:
  private val plugin    = PluginId("demo")
  private val packName  = TaskName("pack")
  private val midName   = TaskName("mid")
  private val childName = TaskName("child")
  private val packId    = PluginTaskId(plugin, packName)
  private val midId     = PluginTaskId(plugin, midName)
  private val childId   = PluginTaskId(plugin, childName)
  private val scala37   = ScalaVersion("3.7.0")
  private val appName   = ProjectName("app")
  private val coreName  = ProjectName("core")

  def spec = suite("plugin step laws")(
    test("an unknown task, a cycle, and a cell error stop before the next lookup"):
      val pack    = declared(packName, List(TaskDep.Same(midName)))
      val mid     = declared(midName, List(TaskDep.Same(childName)))
      val child   = declared(childName, List(TaskDep.Same(packName)))
      val known   = Map(packId -> pack, midId -> mid, childId -> child)
      val cycle   = plan(packId, known.get, _ => Right(Nil), _ => Right(Nil))
      val unknown = plan(packId, _ => None, _ => Left(PluginRunError.Cycle(List(childId))), _ => Right(Nil))
      val cells   = plan(
        packId,
        _ => Some(declared(packName, Nil)),
        _ => Left(PluginRunError.Unknown(childId)),
        _ => Left(PluginRunError.Cycle(List(packId))),
      )
      val missed = plan(
        packId,
        Map(packId -> declared(packName, List(TaskDep.Same(childName)))).get,
        _ => Right(Nil),
        _ => Right(Nil),
      )
      assertTrue(
        cycle == Left(PluginFault.Task(PluginRunError.Cycle(List(packId, midId, childId, packId)))),
        unknown == Left(PluginFault.Task(PluginRunError.Unknown(packId))),
        cells == Left(PluginFault.Task(PluginRunError.Unknown(childId))),
        missed == Left(PluginFault.Task(PluginRunError.Unknown(childId))),
      )
    ,
    test("a copied link cell is compiled once, then the task sees the script and the child"):
      val core                      = PluginCells.Cell(module(coreName), scala37, Platform.Jvm)
      val app                       = PluginCells.Cell(module(appName), scala37, Platform.Jvm)
      val appCopy                   = PluginCells.Cell(module(appName), scala37, Platform.Jvm)
      val pack                      = declared(packName, List(TaskDep.Same(childName)))
      val child                     = declared(childName, Nil)
      val known                     = Map(packId -> pack, childId -> child)
      def cells(task: DeclaredTask) =
        if task.name == packName then Right(List(core, app)) else Right(Nil)
      def links(task: DeclaredTask) =
        if task.name == packName then Right(List(appCopy)) else Right(Nil)
      val program =
        for
          marks <- Ref.make(List.empty[String])
          seen  <- Ref.make("unset")
          compile = (cell: PluginCells.Cell) =>
            marks.update(found => found :+ s"${cell.module.name.value}:${cell.version.value}:${cell.platform.token}")
          link   = (_: PluginCells.Cell) => ZIO.service[Resolve] *> ZIO.service[ContentStore] *> ZIO.succeed("script")
          runOne = (id: PluginTaskId, _: DeclaredTask, text: String) =>
            if id == childId then seen.set(text).as("child") else ZIO.succeed(text)
          built = PluginSteps.goal(packId, known.get, cells, links, compile, link, runOne)
          step     <- ZIO.fromEither(built)
          result   <- Step.run(step)
          compiled <- marks.get
          upstream <- seen.get
        yield assertTrue(
          app == appCopy,
          app ne appCopy,
          result == "script\nchild",
          upstream == "",
          compiled.toSet == Set(s"core:${scala37.value}:jvm", s"app:${scala37.value}:jvm"),
          compiled.count(_ == s"app:${scala37.value}:jvm") == 1,
          waits(step) == List(
            cell(appName, Phase.Link) -> List(cell(appName, Phase.Compile)),
            StepId
              .Plugin(packId) -> List(cell(coreName, Phase.Compile), cell(appName, Phase.Link), StepId.Plugin(childId)),
          ),
          texts(info(step, cell(coreName, Phase.Compile))) == List("cell" -> s"core ${scala37.value} jvm"),
          texts(info(step, cell(appName, Phase.Link))) == List("link" -> s"app ${scala37.value} jvm"),
          texts(info(step, StepId.Plugin(packId))) == List("task" -> packId.render),
          info(step, StepId.Plugin(childId)).needs == Set(StepNeed.Cpu),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Resolve] ++ stubbed[ContentStore]),
  )

  private def plan(
      root: PluginTaskId,
      taskOf: PluginTaskId => Option[DeclaredTask],
      cellsOf: DeclaredTask => Either[PluginRunError, List[PluginCells.Cell]],
      linksOf: DeclaredTask => Either[PluginRunError, List[PluginCells.Cell]],
  ) =
    PluginSteps.goal(root, taskOf, cellsOf, linksOf, _ => ZIO.unit, _ => ZIO.succeed(""), (_, _, _) => ZIO.succeed(""))

  private def declared(name: TaskName, deps: List[TaskDep]): DeclaredTask =
    DeclaredTask(name, Seal.Hermetic, Set.empty, Nil, Nil, deps)

  private def module(name: ProjectName): Module =
    Module(name, Platforms(Platform.Jvm, Nil), ScalaVersions(scala37, Nil), Nil, Nil, SourcePos("histos.scala", 1, 1))

  private def cell(name: ProjectName, phase: Phase): StepId =
    StepId.Cell(name, scala37, Platform.Jvm, phase)

  private def waits(step: Step.Of[?, ?, ?, ?]): List[(StepId, List[StepId])] =
    step match
      case next: Step.Then[?, ?, ?, ?]        => waits(next.upstream) :+ (next.info.id -> upstreamIds(next.upstream))
      case zip: Step.Zipped[?, ?, ?, ?, ?]    => waits(zip.left) ++ waits(zip.right)
      case mapped: Step.Mapped[?, ?, ?, ?, ?] => waits(mapped.inner)
      case _: Step.Leaf[?, ?, ?, ?]           => Nil

  private def upstreamIds(step: Step.Of[?, ?, ?, ?]): List[StepId] =
    step match
      case leaf: Step.Leaf[?, ?, ?, ?]        => List(leaf.info.id)
      case zip: Step.Zipped[?, ?, ?, ?, ?]    => upstreamIds(zip.left) ++ upstreamIds(zip.right)
      case mapped: Step.Mapped[?, ?, ?, ?, ?] => upstreamIds(mapped.inner)
      case next: Step.Then[?, ?, ?, ?]        => List(next.info.id)

  private def info(step: Step.Of[?, ?, ?, ?], id: StepId): StepInfo =
    step match
      case leaf: Step.Leaf[?, ?, ?, ?] if leaf.info.id == id => leaf.info
      case next: Step.Then[?, ?, ?, ?] if next.info.id == id => next.info
      case zip: Step.Zipped[?, ?, ?, ?, ?]                   =>
        val left = infos(zip.left)
        if left.exists(_.id == id) then info(zip.left, id) else info(zip.right, id)
      case mapped: Step.Mapped[?, ?, ?, ?, ?] => info(mapped.inner, id)
      case next: Step.Then[?, ?, ?, ?]        =>
        val below = infos(next.upstream)
        if below.exists(_.id == id) then info(next.upstream, id) else next.info
      case leaf: Step.Leaf[?, ?, ?, ?] => leaf.info

  private def infos(step: Step.Of[?, ?, ?, ?]): List[StepInfo] =
    step match
      case leaf: Step.Leaf[?, ?, ?, ?]        => List(leaf.info)
      case zip: Step.Zipped[?, ?, ?, ?, ?]    => infos(zip.left) ++ infos(zip.right)
      case mapped: Step.Mapped[?, ?, ?, ?, ?] => infos(mapped.inner)
      case next: Step.Then[?, ?, ?, ?]        => infos(next.upstream) :+ next.info

  private def texts(found: StepInfo): List[(String, String)] =
    found.inputs.toList.flatMap: input =>
      input.value match
        case InputValue.Text(text) => List(input.name.value -> text)
        case InputValue.Digest(_)  => Nil
end PluginStepsLawSpec
