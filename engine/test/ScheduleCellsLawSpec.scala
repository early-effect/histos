package histos.zinc

import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.native.NativeOrigin
import histos.resolve.{DependencyLock, LockedTools, Repositories, Resolve}
import histos.step.{InputName, InputValue, Phase, Step, StepId, StepInfo}
import histos.toolchain.{RuntimeIdentity, Runtimes}
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  ToolCatalog,
}

import java.nio.file.Path

import slayer.stubbed

import zio.ZIO
import zio.test.*

import InputName.*

object ScheduleCellsLawSpec extends ZIOSpecDefault:
  private val scala37    = ScalaVersion.trusted("3.7.0")
  private val scala38    = ScalaVersion.trusted("3.8.0")
  private val scala39    = ScalaVersion.trusted("3.9.0")
  private val scala310   = ScalaVersion.trusted("3.10.0")
  private val scala21315 = ScalaVersion.trusted("2.13.15")
  private val scala21316 = ScalaVersion.trusted("2.13.16")
  private val scala21317 = ScalaVersion.trusted("2.13.17")
  private val scala21318 = ScalaVersion.trusted("2.13.18")
  private val appName    = ProjectName.trusted("app")
  private val coreName   = ProjectName.trusted("core")

  def spec = suite("schedule cell laws")(
    test("cells run in version string order then jvm, js, native, and a filter or a missing platform is none"):
      val program =
        for
          plan <- planned
          ordered = module(appName, scala310, List(scala38), List(Platform.Js), Nil)
          step    = goal(plan, List(ordered), None, None)
          only38  = goal(plan, List(ordered), Some(Set(scala38.value)), None)
          js      = goal(plan, List(ordered), None, Some(Platform.Js))
          native  = goal(plan, List(module(appName, scala38, Nil, Nil, Nil)), None, Some(Platform.Native))
          empty   = goal(plan, Nil, None, None)
          missed  = goal(plan, List(ordered), Some(Set("9.9.9")), None)
        yield assertTrue(
          runs(step) == List(
            cell(appName, scala310, Platform.Jvm),
            cell(appName, scala310, Platform.Js),
            cell(appName, scala38, Platform.Jvm),
            cell(appName, scala38, Platform.Js),
          ),
          waits(step).isEmpty,
          runs(only38) == List(cell(appName, scala38, Platform.Jvm), cell(appName, scala38, Platform.Js)),
          runs(js) == List(cell(appName, scala310, Platform.Js), cell(appName, scala38, Platform.Js)),
          runs(native) == List(StepId.Work("none")),
          runs(empty) == List(StepId.Work("none")),
          runs(missed) == List(StepId.Work("none")),
          texts(info(step, cell(appName, scala310, Platform.Jvm))) == List(
            "cell"  -> s"app ${scala310.value} jvm",
            "jdk"   -> Runtimes.identityToken(plan.jdk.identity),
            "tools" -> "",
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Resolve])
    ,
    test("an exact cell is the only wait, scala 3 waits on every scala 3 cell, and 2.13 waits on the newest patch"):
      val program =
        for
          plan <- planned
          core3     = module(coreName, scala39, List(scala38), Nil, Nil)
          wide      = module(appName, scala37, Nil, Nil, List(ProjectDep(coreName, None)))
          exact     = module(appName, scala39, Nil, Nil, List(ProjectDep(coreName, None)))
          alone     = module(appName, scala39, Nil, Nil, List(ProjectDep(coreName, None)))
          old       = module(coreName, scala21316, List(scala21318, scala21317), Nil, Nil)
          miss      = module(appName, scala21315, Nil, Nil, List(ProjectDep(coreName, None)))
          hit       = module(appName, scala21317, Nil, Nil, List(ProjectDep(coreName, None)))
          wideStep  = goal(plan, List(wide, core3), None, None)
          exactStep = goal(plan, List(exact, core3), None, None)
          aloneStep = goal(plan, List(alone), None, None)
          missStep  = goal(plan, List(miss, old), None, None)
          hitStep   = goal(plan, List(hit, old), None, None)
        yield assertTrue(
          waits(wideStep) == List(
            cell(appName, scala37, Platform.Jvm) -> List(
              cell(coreName, scala39, Platform.Jvm),
              cell(coreName, scala38, Platform.Jvm),
            )
          ),
          runs(wideStep).toSet == Set(
            cell(appName, scala37, Platform.Jvm),
            cell(coreName, scala38, Platform.Jvm),
            cell(coreName, scala39, Platform.Jvm),
          ),
          waits(exactStep) == List(
            cell(appName, scala39, Platform.Jvm) -> List(cell(coreName, scala39, Platform.Jvm))
          ),
          runs(exactStep).contains(cell(coreName, scala38, Platform.Jvm)),
          waits(aloneStep).isEmpty,
          runs(aloneStep) == List(cell(appName, scala39, Platform.Jvm)),
          waits(missStep) == List(
            cell(appName, scala21315, Platform.Jvm) -> List(cell(coreName, scala21318, Platform.Jvm))
          ),
          waits(hitStep) == List(
            cell(appName, scala21317, Platform.Jvm) -> List(cell(coreName, scala21317, Platform.Jvm))
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Resolve])
    ,
    test("a packed classpath is sorted by module, version, then platform"):
      val app  = (appName, scala310, Platform.Js)
      val core = (coreName, scala38, Platform.Jvm)
      val text = ScheduleCells.render(
        Map(core -> Path.of("/tmp/core"), app -> Path.of("/tmp/app"))
      )
      assertTrue(
        text == s"app\t${scala310.value}\tjs\t/tmp/app\ncore\t${scala38.value}\tjvm\t/tmp/core",
        ScheduleCells.render(Map.empty) == "",
      ),
  )

  private def goal(
      plan: CompilePlan,
      selected: List[Module],
      only: Option[Set[String]],
      platform: Option[Platform],
  ) =
    ScheduleCells.goal(plan, selected, only, platform, (_: ProjectName) => ZIO.unit, (_: CompileFeed) => ())

  private def planned: ZIO[Resolve, Nothing, CompilePlan] =
    ZIO.serviceWith[Resolve]: resolve =>
      val jdk = JdkRuntime(
        Path.of("/jdk"),
        JdkVersion.trusted("21", JdkMajor.trusted(21)),
        RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
      )
      CompilePlan(
        Path.of("repo"),
        Nil,
        DependencyLock(Nil),
        jdk,
        NativeOrigin.Central,
        Repositories.central,
        LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve),
        LockStamp("t"),
      )

  private def module(
      name: ProjectName,
      first: ScalaVersion,
      rest: List[ScalaVersion],
      extraPlatforms: List[Platform],
      upstream: List[ProjectDep],
  ): Module =
    Module(
      name,
      Platforms(Platform.Jvm, extraPlatforms),
      ScalaVersions(first, rest),
      Nil,
      upstream,
      SourcePos("histos.scala", 1, 1),
    )

  private def cell(name: ProjectName, version: ScalaVersion, platform: Platform): StepId =
    StepId.Cell(name, version, platform, Phase.Compile)

  private def runs(step: Step.Of[?, ?, ?, ?]): List[StepId] =
    step match
      case leaf: Step.Leaf[?, ?, ?, ?]        => List(leaf.info.id)
      case zip: Step.Zipped[?, ?, ?, ?, ?]    => runs(zip.left) ++ runs(zip.right)
      case mapped: Step.Mapped[?, ?, ?, ?, ?] => runs(mapped.inner)
      case next: Step.Then[?, ?, ?, ?]        => runs(next.upstream) :+ next.info.id

  private def waits(step: Step.Of[?, ?, ?, ?]): List[(StepId, List[StepId])] =
    step match
      case next: Step.Then[?, ?, ?, ?] =>
        waits(next.upstream) :+ (next.info.id -> upstreamIds(next.upstream))
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
end ScheduleCellsLawSpec
