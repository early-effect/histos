package histos.zinc

import histos.cache.ContentStore
import histos.jdk.JdkRuntime
import histos.native.NativeOrigin
import histos.resolve.{DependencyLock, LockedCell, LockedTools, Resolve}
import histos.toolchain.BuildCompiler
import histos.workspace.Workspace
import histos.{Jvm, Module, ProjectGraph, ProjectName, Projects, module}

import java.nio.file.Path

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Chunk, Promise, Ref, ZIO, ZLayer}
import zio.test.{assertTrue, suite, test}

/** Scheduling. The compiler is a stub. No scalac and no child.
  */
object ScheduleLaw:
  private val core = ProjectName("core")
  private val app  = ProjectName("app")
  private val side = ProjectName("side")

  def laws =
    suite("cells")(upstream, overlap)

  private def upstream =
    test("The console finishes an upstream cell before the module that depends on it."):
      BuildCompiler.version.flatMap: version =>
        open(version).flatMap: (jdk, root, graph) =>
          traced:
            ready *> console(jdk, root, graph).runCollect.either
          .map:
            case Left(error)  => assertTrue(error.message.isEmpty)
            case Right(feeds) =>
              val seen  = marks(feeds)
              val order = seen.collect:
                case ("done", name) if name == core => "done-core"
                case ("began", name) if name == app => "began-app"
                case ("done", name) if name == app  => "done-app"
              val sideDone = seen.exists((kind, name) => kind == "done" && name == side)
              assertTrue(order == List("done-core", "began-app", "done-app"), sideDone)

  private def overlap =
    test("A cell may start while an unrelated upstream is still compiling."):
      val cpus = java.lang.Runtime.getRuntime.availableProcessors
      // Parallelism.default gives one CPU permit per processor. One permit cannot overlap two cells.
      if cpus < 2 then ZIO.succeed(assertTrue(cpus >= 2))
      else
        BuildCompiler.version.flatMap: version =>
          open(version).flatMap: (jdk, root, graph) =>
            for
              release <- Promise.make[Nothing, Unit]
              early   <- Ref.make(Option.empty[Boolean])
              begun   <- Ref.make(Set.empty[ProjectName])
              result  <- traced:
                pausing(release) *>
                  console(jdk, root, graph)
                    .tap(feed => note(begun, early, release, feed))
                    .runCollect
                    .either
              seen <- early.get
            yield result match
              case Left(error)  => assertTrue(error.message.isEmpty)
              case Right(feeds) =>
                val done = marks(feeds).collect { case ("done", name) => name }.toSet
                assertTrue(seen == Some(true), done == Set(core, app, side))

  private def note(
      begun: Ref[Set[ProjectName]],
      early: Ref[Option[Boolean]],
      release: Promise[Nothing, Unit],
      feed: CompileFeed,
  ) =
    feed match
      case CompileFeed.Began(name, _, _) =>
        begun.update(_ + name) *>
          (if name == side then
             begun.get.flatMap: names =>
               early.set(Some(!names.contains(app))) *> release.succeed(()).unit
           else ZIO.unit)
      case _ => ZIO.unit

  private def ready =
    stub[Compiler](_.compile(any[CompileCall]))((call: CompileCall) => finish(call))

  private def pausing(release: Promise[Nothing, Unit]) =
    stub[Compiler](_.compile(any[CompileCall]))((call: CompileCall) =>
      val wait =
        if moduleName(call).contains(core.value) then release.await
        else ZIO.unit
      wait *> finish(call)
    )

  private def finish(call: CompileCall) =
    ZIO.fromEither(ApiDigest.writeEmpty(call.request.analysis)).as(output(call))

  private def output(call: CompileCall): CompileOutput =
    CompileOutput(
      call.request.classes,
      0,
      call.key,
      changed = true,
      sources = Some(SourceCount(call.request.sources.size, 0)),
      classesWritten = Some(0),
      miss = None,
    )

  private def console(jdk: JdkRuntime, root: Path, graph: ProjectGraph) =
    GraphCompile.consoleCounted(root, graph, List(app, side), jdk, NativeOrigin.Central, None)

  private def marks(feeds: Chunk[CompileFeed]): List[(String, ProjectName)] =
    feeds.toList.flatMap:
      case CompileFeed.Began(name, _, _) => List("began" -> name)
      case CompileFeed.Done(cell)        => List("done" -> cell.module)
      case _                             => Nil

  private def moduleName(call: CompileCall): Option[String] =
    Option(call.request.classes.getParent).flatMap: versionDir =>
      Option(versionDir.getParent).flatMap: moduleDir =>
        Option(moduleDir.getFileName).map(_.toString)

  private def open(version: histos.ScalaVersion) =
    val coreDef = module("core", Jvm)(version)
    val sideDef = module("side", Jvm)(version)
    val appDef  = module("app", Jvm)(version).dependsOn(coreDef)
    val files   = List(
      "core/src/main/scala/Core.scala" -> "object Core\n",
      "app/src/main/scala/App.scala"   -> "object App\n",
      "side/src/main/scala/Side.scala" -> "object Side\n",
    )
    for
      jdk   <- GraphCompile.jdkHere
      root  <- Workspace.work("histos-schedule")
      graph <- ZIO.fromEither(Projects(coreDef, appDef, sideDef))
      _     <- ZIO.foreachDiscard(files): (name, body) =>
        Workspace.writeString(root.resolve(name), body)
      _ <- Workspace.writeString(Resolve.lockPath(root), lockText(graph))
      _ <- histos.ToolFixture.seed(root, graph)
    yield (jdk, root, graph)

  private def traced[A](
      use: ZIO[LockedTools & Stubbed[Compiler] & Resolve & ContentStore & Compiler, Any, A]
  ) =
    Workspace
      .work("histos-schedule-store")
      .flatMap: store =>
        use.provideSomeLayer[LockedTools](
          stubbedTraced[Compiler] ++ ZLayer.succeed(ContentStore.in(store)) ++ resolveLayer
        )

  private val resolveLayer =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve))

  private def lockText(graph: ProjectGraph): String =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          LockedCell(item.name, version, platform, Nil, Nil, Nil)
    DependencyLock.render(DependencyLock(cells))

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }
end ScheduleLaw
