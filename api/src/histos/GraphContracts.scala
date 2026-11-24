package histos

import histos.plugin.*

/** Declaration validation only. Filesystem links and publication are checked by the runtime. */
private[histos] object GraphContracts:
  def validate(projects: List[Project], plugins: List[RegisteredPlugin]): Either[BuildError, Unit] =
    val modules = projects.collect { case module: Module => module }
    val tasks   =
      plugins.flatMap(plugin => plugin.contribution.tasks.map(task => PluginTaskId(plugin.id, task.name) -> task))
    for
      _ <- moduleShapes(modules)
      _ <- ownership(tasks)
      _ <- emissions(tasks)
      _ <- acyclic(modules, tasks)
    yield ()

  def verify(graph: ProjectGraph): Either[BuildError, Unit] =
    val modules = graph.projects.collect { case module: Module => module }
    val problem = graph.verify.flatMap: task =>
      task.run match
        case VerifyRun.BuiltIn(_) | VerifyRun.Plugin(_) => Nil
        case VerifyRun.Tests(selection)                 =>
          selection.targets match
            case TestTargets.All | TestTargets.Filtered(_) => Nil
            case TestTargets.Named(first, rest)            =>
              (first :: rest).flatMap: name =>
                modules.find(_.name == name) match
                  case None         => List(ContractFault.TestModule(name))
                  case Some(module) =>
                    selection.platforms.validate(name, module.platforms) match
                      case Left(SelectionError.Unsupported(_, platform)) =>
                        List(ContractFault.TestPlatform(name, platform))
                      case Left(error) => List(ContractFault.TestSelection(error))
                      case Right(_)    => Nil
    problem.headOption.toLeft(()).left.map(BuildError.Contract(_))

  private def moduleShapes(modules: List[Module]): Either[BuildError, Unit] =
    val defaults = modules.flatMap: module =>
      module.runPlatform
        .filterNot(module.platforms.all.contains)
        .map(ContractFault.RunDefault(module.name, _))
        .toList ++
        module.roots.flatMap: root =>
          root.platforms match
            case PlatformSelection.All             => Nil
            case PlatformSelection.Only(platforms) =>
              platforms.all.filterNot(module.platforms.all.contains).map(ContractFault.RootPlatform(module.name, _))
    val groups   = modules.filter(_.testAggregates.nonEmpty)
    val overlaps = groups.zipWithIndex.flatMap: (first, index) =>
      groups
        .drop(index + 1)
        .flatMap: second =>
          (first.name :: first.testAggregates)
            .find((second.name :: second.testAggregates).contains)
            .map(ContractFault.AggregateOverlap(first.name, second.name, _))
            .toList
    (defaults ++ overlaps).headOption.toLeft(()).left.map(BuildError.Contract(_))

  private def ownership(tasks: List[(PluginTaskId, DeclaredTask)]): Either[BuildError, Unit] =
    val duplicates = tasks.flatMap: (id, task) =>
      task.outputs
        .groupBy(_.name)
        .collect:
          case (name, outputs) if outputs.size > 1 => ContractFault.DuplicateOutput(id, name)
    val outputs  = tasks.flatMap((id, task) => task.outputs.map(id -> _))
    val overlaps = outputs.zipWithIndex.flatMap: (entry, index) =>
      val (id, output) = entry
      outputs
        .drop(index + 1)
        .collect:
          case (otherId, other) if overlapsPath(output.path, other.path) =>
            ContractFault.OutputOverlap(id, otherId, output.path)
    (duplicates ++ overlaps).headOption.toLeft(()).left.map(BuildError.Contract(_))

  private def overlapsPath(first: RepoPath, second: RepoPath): Boolean =
    first == second || first.value.startsWith(second.value + "/") || second.value.startsWith(first.value + "/")

  private def emissions(tasks: List[(PluginTaskId, DeclaredTask)]): Either[BuildError, Unit] =
    val problem = tasks.flatMap: (id, task) =>
      task.dependsOn.flatMap:
        case TaskDep.EmitScoped(_, _, _, content, name) =>
          task.outputs.find(_.name == name) match
            case None                                              => List(ContractFault.EmittedOutput(id, name))
            case Some(output) if output.kind != content.outputKind =>
              List(ContractFault.EmittedOutputKind(id, name, content.outputKind, output.kind))
            case Some(_) => Nil
        case TaskDep.Compile(_, _, _) | TaskDep.CompileScoped(_, _, _, _) | TaskDep.Link(_, _, _) | TaskDep.Same(_) |
            TaskDep.Generate(_) | TaskDep.Emit(_) =>
          Nil
    problem.headOption.toLeft(()).left.map(BuildError.Contract(_))

  private def acyclic(modules: List[Module], tasks: List[(PluginTaskId, DeclaredTask)]): Either[BuildError, Unit] =
    def cells(name: ProjectName, version: Option[ScalaVersion], selection: PlatformSelection): List[Cell] =
      modules
        .filter(_.name == name)
        .flatMap: module =>
          for
            scala    <- module.versions.all.filter(found => version.forall(_ == found))
            platform <- selection.selected(module.platforms)
          yield Cell(name, scala, platform)

    def chosen(platform: Option[Platform]): PlatformSelection =
      platform.fold(PlatformSelection.All)(PlatformSelection.only(_))

    def stage(cell: Cell, scope: SourceSet): BuildNode = BuildNode.Compile(CompilationStage(cell, scope))

    def prerequisites(id: PluginTaskId, task: DeclaredTask): List[BuildNode] =
      task.dependsOn.flatMap:
        case TaskDep.Compile(name, version, platform) =>
          cells(name, version, chosen(platform)).map(stage(_, SourceSet.Test))
        case TaskDep.CompileScoped(name, version, platform, scope) =>
          cells(name, version, chosen(platform)).map(stage(_, scope))
        case TaskDep.Link(name, version, _) =>
          cells(name, version, PlatformSelection.only(Platform.Js)).map(BuildNode.Link(_))
        case TaskDep.Same(name)                => List(BuildNode.Task(PluginTaskId(id.plugin, name)))
        case TaskDep.Generate(name)            => List(BuildNode.Task(PluginTaskId(id.plugin, name)))
        case TaskDep.Emit(_)                   => Nil
        case TaskDep.EmitScoped(_, _, _, _, _) => Nil

    def producers(cell: Cell, scope: SourceSet): List[BuildNode] =
      tasks.collect:
        case (id, task) if task.dependsOn.exists {
              case TaskDep.Emit(name)                             => name == cell.module && scope == SourceSet.Main
              case TaskDep.EmitScoped(name, set, platforms, _, _) =>
                name == cell.module && set == scope && platforms.contains(cell.platform)
              case TaskDep.Compile(_, _, _) | TaskDep.CompileScoped(_, _, _, _) | TaskDep.Link(_, _, _) |
                  TaskDep.Same(_) | TaskDep.Generate(_) =>
                false
            } =>
          BuildNode.Task(id)

    def upstreamStages(names: List[ProjectName], cell: Cell, scope: SourceSet): List[BuildNode] =
      names.flatMap: name =>
        val available  = cells(name, None, PlatformSelection.only(cell.platform))
        val exact      = available.filter(_.version == cell.version)
        val candidates =
          if exact.nonEmpty then exact
          else
            available.filter: upstream =>
              (cell.version.value.startsWith("3.") && upstream.version.value.startsWith("3.")) ||
                (cell.version.value.startsWith("2.13.") && upstream.version.value.startsWith("2.13."))
        candidates.map(stage(_, scope))

    val compilation = modules.flatMap: module =>
      cells(module.name, None, PlatformSelection.All).flatMap: cell =>
        val main     = stage(cell, SourceSet.Main)
        val test     = stage(cell, SourceSet.Test)
        val mainDeps = upstreamStages(module.upstream(cell.platform), cell, SourceSet.Main)
        val testDeps = upstreamStages(module.upstream(cell.platform, SourceSet.Test), cell, SourceSet.Test)
        List(
          main                 -> (mainDeps ++ producers(cell, SourceSet.Main)),
          test                 -> ((main :: testDeps) ++ producers(cell, SourceSet.Test)),
          BuildNode.Link(cell) -> List(main),
        )
    val edges = (compilation ++ tasks.map((id, task) => BuildNode.Task(id) -> prerequisites(id, task))).toMap

    def walk(node: BuildNode, path: List[BuildNode], done: Set[BuildNode]): (Option[List[BuildNode]], Set[BuildNode]) =
      if path.contains(node) then (Some((node :: path).reverse.dropWhile(_ != node)), done)
      else if done.contains(node) then (None, done)
      else
        val (cycle, seen) = edges
          .getOrElse(node, Nil)
          .foldLeft((Option.empty[List[BuildNode]], done)):
            case ((Some(found), checked), _) => (Some(found), checked)
            case ((None, checked), child)    => walk(child, node :: path, checked)
        (cycle, seen + node)

    val (cycle, _) = edges.keys.toList.foldLeft((Option.empty[List[BuildNode]], Set.empty[BuildNode])):
      case ((Some(found), checked), _) => (Some(found), checked)
      case ((None, checked), node)     => walk(node, Nil, checked)
    cycle.map(ContractFault.Cycle(_)).toLeft(()).left.map(BuildError.Contract(_))
  end acyclic
end GraphContracts
