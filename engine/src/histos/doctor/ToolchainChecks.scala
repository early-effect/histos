package histos.doctor

import histos.jdk.{JdkRead, Machine, Toolchain}
import histos.native.{ClangProbe, NativeDeclaration, NativeProject}
import histos.node.NodeToolchain
import histos.toolchain.Survey
import histos.{
  ApiClock,
  ClangMajor,
  ClangPin,
  Module,
  NodeFloor,
  Platform,
  ProjectGraph,
  RunnerTools,
  ToolRevision,
  ToolchainIdentity,
}

import java.nio.file.Path

import zio.{UIO, ZIO}

/** JDK, Node, clang, and the runnerTools the generated jobs install. */
object ToolchainChecks:
  val jdkId: CheckId       = CheckId.trusted("jdk.source")
  val nodeFloorId: CheckId = CheckId.trusted("node.floor")
  val shadowId: CheckId    = CheckId.trusted("node.shadowed")
  val clangId: CheckId     = CheckId.trusted("clang.source")
  val coverId: CheckId     = CheckId.trusted("runner-tools.cover")
  val agreeId: CheckId     = CheckId.trusted("runner-tools.agree")
  val localId: CheckId     = CheckId.trusted("toolchain.local")

  def checks(machine: Machine, graph: ProjectGraph, launch: List[Module]): List[DoctorCheck] =
    List(
      check(jdkId)(jdk(machine)),
      check(nodeFloorId)(nodeFloor(machine, graph, launch)),
      check(shadowId)(nodeShadow(machine, graph, launch)),
      check(clangId)(clang(machine, launch)),
      check(coverId)(ZIO.succeed(List(cover(graph)))),
      check(agreeId)(ZIO.succeed(agree(graph))),
      check(localId)(local(machine.repo)),
    )

  /** The warning `doctor` reports for a stale local toolchain. The same sentence `ApiClock` prints. */
  def reported(pointed: Boolean, identity: Option[ToolchainIdentity], head: Option[String]): Outcome =
    ApiClock.decide(pointed, identity, head) match
      case ApiClock.Admit.Warn(line) => Outcome.Found(ToolchainDiagnostic.Local(line))
      case ApiClock.Admit.Go         =>
        Outcome.Ok(localId, "the local toolchain matches this checkout, or this command runs the pin")

  def local(repo: Path): UIO[List[Outcome]] =
    ApiClock
      .admit(repo)
      .map:
        case ApiClock.Admit.Warn(line) => List(Outcome.Found(ToolchainDiagnostic.Local(line)))
        case ApiClock.Admit.Go         => List(reported(false, None, None))

  def jdk(machine: Machine): UIO[List[Outcome]] =
    Toolchain
      .read(machine)
      .map:
        case JdkRead.Chosen(runtime, source) =>
          List(Outcome.Ok(jdkId, s"jdk ${runtime.version.value} at ${runtime.home} from ${source.label}"))
        case JdkRead.Failed(error, source) =>
          List(Outcome.Found(ToolchainDiagnostic.Jdk(Toolchain.failure(error, source))))

  /** Empty `launch` is a doctor run that starts no module, so the check does not probe. A non-empty list is the modules
    * that run asked to launch. Only a Js module in that list probes, and a miss names those modules.
    */
  def nodeFloor(machine: Machine, graph: ProjectGraph, launch: List[Module]): UIO[List[Outcome]] =
    val js = launch.filter(_.platforms.all.contains(Platform.Js))
    if launch.isEmpty then ZIO.succeed(List(Outcome.Ok(nodeFloorId, "this run does not launch Node")))
    else if js.isEmpty then ZIO.succeed(List(Outcome.Ok(nodeFloorId, "no Js module")))
    else
      NodeToolchain
        .choose(machine, graph.runnerTools.nodePath)
        .either
        .map:
          case Right(choice) =>
            val runtime = choice.runtime
            List(
              Outcome.Ok(
                nodeFloorId,
                s"node ${runtime.version.value} at ${runtime.binary.path} from ${NodeToolchain.origin(choice.source)}",
              )
            )
          case Left(error) =>
            List(Outcome.Found(ToolchainDiagnostic.NodeFloor(s"${launched(js)}. ${NodeToolchain.failure(error)}")))

  private def launched(modules: List[Module]): String =
    modules.map(_.name.value) match
      case Nil       => "no module launches on Node"
      case List(one) => s"$one launches on Node"
      case many      => s"${many.mkString(", ")} launch on Node"

  /** Empty `launch` does not walk `PATH`. A named binary (`HISTOS_NODE`, `runnerTools.nodePath`, or `node.local`) does
    * not read `PATH`, so a warning about a later binary would be a lie. A Js launch that falls through to `PATH` warns
    * when the first `node` is below the floor and a later one is newer.
    */
  def nodeShadow(machine: Machine, graph: ProjectGraph, launch: List[Module]): UIO[List[Outcome]] =
    val js = launch.filter(_.platforms.all.contains(Platform.Js))
    if launch.isEmpty then ZIO.succeed(List(Outcome.Ok(shadowId, "this run does not launch Node")))
    else if js.isEmpty then ZIO.succeed(List(Outcome.Ok(shadowId, "no Js module")))
    else
      NodeToolchain
        .abovePath(machine, graph.runnerTools.nodePath)
        .flatMap:
          case Some(source) =>
            ZIO.succeed(
              List(Outcome.Ok(shadowId, s"the launch uses ${NodeToolchain.origin(source)} and does not read PATH"))
            )
          case None =>
            NodeToolchain
              .sights(machine)
              .map: found =>
                NodeToolchain.shadow(found) match
                  case None =>
                    List(Outcome.Ok(shadowId, s"${launched(js)}. PATH does not hide a newer node"))
                  case Some(hidden) =>
                    val line    = exportLine(hidden.newer.binary)
                    val summary =
                      s"${launched(js)}. node ${hidden.first.version.value} at ${hidden.first.binary} is first on PATH and hides node ${hidden.newer.version.value} at ${hidden.newer.binary}."
                    List(Outcome.Found(ToolchainDiagnostic.NodeShadowed(line, summary)))

  /** Empty `launch` links no module, so the check does not probe. A non-empty list is the modules that run asked to
    * link. Only a Native module in that list probes, and a miss names those modules.
    */
  def clang(machine: Machine, launch: List[Module]): UIO[List[Outcome]] =
    val native = launch.filter(_.platforms.all.contains(Platform.Native))
    if launch.isEmpty then ZIO.succeed(List(Outcome.Ok(clangId, "this run does not link Native")))
    else if native.isEmpty then ZIO.succeed(List(Outcome.Ok(clangId, "no Native module")))
    else
      NativeProject.declaration(ProjectGraph.trusted(native)) match
        case NativeDeclaration.NotNative =>
          ZIO.succeed(List(Outcome.Ok(clangId, "no Native module")))
        case NativeDeclaration.Missing(name) =>
          clangFound(native, s"${NativeDeclaration.Missing(name).message} clang is resolved from PATH.")
        case NativeDeclaration.Disagree(left, right) =>
          clangFound(native, s"${NativeDeclaration.Disagree(left, right).message} clang is resolved from PATH.")
        case NativeDeclaration.Major(major) =>
          ClangProbe
            .resolve(major, machine.env)
            .either
            .map:
              case Right(resolved) =>
                List(Outcome.Ok(clangId, s"clang ${resolved.version.value} at ${resolved.clang} from PATH"))
              case Left(error) =>
                List(Outcome.Found(ToolchainDiagnostic.Clang(told(native, Survey.clangProblem(error)))))

  private def clangFound(modules: List[Module], detail: String): UIO[List[Outcome]] =
    ZIO.succeed(List(Outcome.Found(ToolchainDiagnostic.Clang(told(modules, detail)))))

  private def told(modules: List[Module], detail: String): String =
    s"${linked(modules)}. $detail"

  private def linked(modules: List[Module]): String =
    modules.map(_.name.value) match
      case Nil       => "no module links with clang"
      case List(one) => s"$one links with clang"
      case many      => s"${many.mkString(", ")} link with clang"

  def cover(graph: ProjectGraph): Outcome =
    val native   = nativeModules(graph)
    val js       = hasJs(graph)
    val majors   = clangMajors(native)
    val needNode = js && graph.runnerTools.node.isEmpty
    val needLlvm = native.nonEmpty && graph.runnerTools.llvm.isEmpty
    if !needNode && !needLlvm then Outcome.Ok(coverId, "runnerTools covers the modules")
    else
      val suggested = suggest(graph.runnerTools, needNode, majors)
      val shape     = RunnerCall.show(suggested)
      Outcome.Found(ToolchainDiagnostic.Cover(coverSummary(needNode, needLlvm, majors, shape), shape))

  def agree(graph: ProjectGraph): List[Outcome] =
    val found = nodeAgree(graph).toList ++ llvmAgree(graph).toList
    if found.isEmpty then List(Outcome.Ok(agreeId, "runnerTools agree with the modules"))
    else found

  /** `export HISTOS_NODE=<path>`. A path with spaces is quoted. */
  def exportLine(path: Path): String =
    val text  = path.toString
    val quote = text.exists(char => char.isWhitespace || char == '\'' || char == '"')
    val shown = if quote then "'" + text.replace("'", "'\\''") + "'" else text
    s"export HISTOS_NODE=$shown"

  private def nodeAgree(graph: ProjectGraph): Option[Outcome] =
    graph.runnerTools.node
      .filter(_.value < 25)
      .map: floor =>
        val raised  = graph.runnerTools.copy(node = Some(NodeFloor.trusted(25)))
        val shape   = RunnerCall.show(raised)
        val summary = s"NodeFloor(${floor.value}) is below 25. Raise it to NodeFloor(25). Add $shape."
        Outcome.Found(ToolchainDiagnostic.Agree(summary, shape))

  private def llvmAgree(graph: ProjectGraph): Option[Outcome] =
    graph.runnerTools.llvm.flatMap: revision =>
      clangMajors(nativeModules(graph)).distinct match
        case Nil =>
          None
        case List(major) if majorOf(revision).contains(major.value) =>
          None
        case List(major) =>
          val raised  = graph.runnerTools.copy(llvm = Some(raise(revision, major.value)))
          val shape   = RunnerCall.show(raised)
          val have    = majorOf(revision).getOrElse(0)
          val summary =
            s"LLVM ${revision.value} is major $have, and the modules name ClangMajor(${major.value}). Raise the revision. Add $shape."
          Some(Outcome.Found(ToolchainDiagnostic.Agree(summary, shape)))
        case many =>
          val shape   = RunnerCall.show(graph.runnerTools)
          val summary =
            s"LLVM ${revision.value} cannot match ClangMajor ${many.map(_.value).mkString(" and ")}. Make the Native modules name one clang major, then set the ToolRevision major to that number. Add $shape."
          Some(Outcome.Found(ToolchainDiagnostic.Agree(summary, shape)))

  private def suggest(tools: RunnerTools, needNode: Boolean, majors: List[ClangMajor]): RunnerTools =
    val node =
      if needNode then Some(NodeFloor.trusted(25)) else tools.node
    val llvm =
      if tools.llvm.isDefined then tools.llvm
      else
        majors.distinct match
          case List(major) => Some(ToolRevision.trusted(s"${major.value}.0.0"))
          case _           => tools.llvm
    tools.copy(node = node, llvm = llvm)

  private def coverSummary(needNode: Boolean, needLlvm: Boolean, majors: List[ClangMajor], shape: String): String =
    val missing = List(
      Option.when(needNode)("Js modules have no runnerTools.node, so the test job does not install Node"),
      Option.when(needLlvm)("Native modules have no runnerTools.llvm, so the test job does not install clang"),
    ).flatten.mkString(". ")
    val majorNote =
      if !needLlvm then ""
      else
        majors.distinct match
          case List(major) => s" The LLVM revision major is ${major.value}."
          case Nil         => " Name a ClangMajor on each Native module, then set llvm to that major."
          case many        =>
            s" Native modules name clang ${many.map(_.value).mkString(" and ")}. Pick one ClangMajor before setting llvm."
    s"$missing.$majorNote Add $shape."

  private def raise(revision: ToolRevision, major: Int): ToolRevision =
    val next = s"$major${revision.value.dropWhile(_.isDigit)}"
    ToolRevision.from(next) match
      case Right(found) => found
      case Left(_)      => ToolRevision.trusted(s"$major.0.0")

  private def majorOf(revision: ToolRevision): Option[Int] =
    revision.value.takeWhile(_.isDigit).toIntOption.filter(_ > 0)

  private def hasJs(graph: ProjectGraph): Boolean =
    modules(graph).exists(_.platforms.all.contains(Platform.Js))

  private def nativeModules(graph: ProjectGraph): List[Module] =
    modules(graph).filter(_.platforms.all.contains(Platform.Native))

  private def modules(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case module: Module => module }

  private def clangMajors(native: List[Module]): List[ClangMajor] =
    native.flatMap: module =>
      module.clang match
        case ClangPin.Major(major) => List(major)
        case ClangPin.Unused       => Nil

  private def check(checkId: CheckId)(body: UIO[List[Outcome]]): DoctorCheck =
    new DoctorCheck:
      def id                      = checkId
      def area                    = Area.Toolchain
      def needs                   = Set(Probe.Local)
      def run(ctx: DoctorContext) = body
end ToolchainChecks
