package histos.testrun

import histos.*
import histos.build.{BuildNode, ExecutionError, GeneratedTask}
import histos.lock.ExecutionKind
import histos.plugin.*
import histos.workspace.{IoFailure, Workspace}
import histos.zinc.{GeneratedLocation, GeneratedRootId}

import java.nio.file.Files

import zio.{Ref, Scope, ZIO, durationInt}
import zio.test.*

object ScopedTestResourcesSpec extends ZIOSpecDefault:
  private val tests = suite("generated Test resource inventory")(
    test(
      "the Test leaf and child consume leased File/tree resources at logical names, not file parents or publication aliases"
    ) {
      for
        runs <- Ref.make(0)
        emitter = ResourcesEmitter(runs)
        graph <- ZIO.fromEither(
          Projects(module("consumer", Jvm)(TestExecutionFixtures.version).test(TestMain("demo.Check")))
        )
        linked = graph.withPlugins(List(RegisteredPlugin(emitter.id, emitter.contribute)(emitter)))
        checked <- TestExecutionFixtures.withGraph(linked): fixture =>
          val cell  = Cell(ProjectName("consumer"), TestExecutionFixtures.version, Jvm)
          val stage = CompilationStage(cell, SourceSet.Test)
          val asked =
            TestRun(fixture.repo, linked, List(cell.module), fixture.session.jdk, fixture.runtimes, TestMode.Run)
          for
            _        <- Workspace.writeString(fixture.repo.resolve("consumer/test/demo/Check.scala"), source)
            products <- fixture.ensureGenerated(List(BuildNode.Compile(stage))) { (request, id, _) =>
              val lease        = request.session.repo.resolve(".histos/generations/test-resources")
              val destinations = OutputDestinations(
                List(
                  OutputDestination.File(emitter.file, lease.resolve("single.dat")),
                  OutputDestination.Directory(emitter.tree, lease.resolve("tree")),
                )
              )
              for
                scope  <- ZIO.scope
                output <- emitter
                  .run(
                    RunRequest(
                      id,
                      request.session.repo,
                      Nil,
                      PluginArgs.empty,
                      Nil,
                      InvocationContext(
                        InvocationId.derive("test-resource"),
                        ClientId("cli"),
                        CommandIdentity.Task(id),
                        ExecutionKind.Local,
                        Interaction.Unattended,
                        OfflineSelection.Online,
                      ),
                      PluginEvents.closedInvocation,
                      Nil,
                      Nil,
                      destinations,
                    )
                  )
                  .provideSomeLayer(PluginScopes.live(scope))
                  .mapError(error => ExecutionError.Plugin(histos.load.PluginFault.Task(error)))
              yield GeneratedTask(
                output,
                ZIO.acquireRelease(
                  ZIO.succeed(
                    List(
                      GeneratedLocation(
                        GeneratedRootId(id, emitter.file),
                        OutputKind.File,
                        lease.resolve("single.dat"),
                      ),
                      GeneratedLocation(GeneratedRootId(id, emitter.tree), OutputKind.Directory, lease.resolve("tree")),
                    )
                  )
                )(_ => Workspace.delete(lease).ignore),
              )
            }
            test  <- ZIO.fromEither(products.require(stage))
            plan  <- ZIO.fromEither(TestCells.plan(linked, asked.selection))
            feeds <- ModuleTest.execute(asked, plan, products).runCollect
            file  <- Workspace.readString(test.classes.resolve("named/file.txt"))
            tree  <- Workspace.readString(test.classes.resolve("tree/value.txt"))
            alias <- ZIO
              .attemptBlocking(Files.exists(fixture.repo.resolve("target/logical/single.dat")))
              .mapError(_ => IoFailure(fixture.repo, histos.workspace.IoFault.Stat))
            leak <- ZIO
              .attemptBlocking(Files.exists(test.classes.resolve("leak.txt")))
              .mapError(_ => IoFailure(test.classes, histos.workspace.IoFault.Stat))
            calls <- runs.get
          yield assertTrue(
            file == "file",
            tree == "tree",
            !alias,
            !leak,
            calls == 1,
            feeds.exists { case TestFeed.Out(_, _, _, text) => text == "file:tree"; case _ => false },
            feeds.exists { case TestFeed.Done(cell) => cell.status == TestStatus.Passed; case _ => false },
          )
      yield checked
    }
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private final case class ResourcesEmitter(runs: Ref[Int]) extends HistosPlugin:
    val id               = PluginId("test-resources")
    val file             = OutputName("single")
    val tree             = OutputName("tree")
    private val filePath = RepoPath("target/logical/single.dat")
    private val treePath = RepoPath("target/logical/tree")
    def contribute       = Contribution(
      List(
        DeclaredTask(
          TaskName("emit"),
          Seal.Open,
          Set.empty,
          Nil,
          List(DeclaredOutput.File(file, filePath), DeclaredOutput.Directory(tree, treePath)),
          List(
            TaskDep.EmitScoped(
              ProjectName("consumer"),
              SourceSet.Test,
              PlatformSelection.only(Jvm),
              EmittedContent.ResourceFile(ResourceName("named/file.txt")),
              file,
            ),
            TaskDep.EmitScoped(
              ProjectName("consumer"),
              SourceSet.Test,
              PlatformSelection.only(Jvm),
              EmittedContent.ResourceTree,
              tree,
            ),
          ),
        )
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      for
        _         <- runs.update(_ + 1)
        single    <- ZIO.fromEither(request.file(file))
        directory <- ZIO.fromEither(request.directory(tree))
        _         <- Workspace.writeString(single, "file").mapError(_ => PluginRunError.MissingOutput(file, filePath))
        _         <- Workspace
          .writeString(single.resolveSibling("leak.txt"), "not declared")
          .mapError(_ => PluginRunError.MissingOutput(file, filePath))
        _ <- Workspace
          .writeString(directory.resolve("tree/value.txt"), "tree")
          .mapError(_ => PluginRunError.MissingOutput(tree, treePath))
      yield PluginOutput("resources", List(WrittenFile(file, filePath)), List(WrittenDirectory(tree, treePath)))

  private val source =
    """package demo
      |object Check:
      |  private def read(name: String): String =
      |    Option(getClass.getResourceAsStream(name)).fold("missing"): input =>
      |      try new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      |      finally input.close()
      |  def main(args: Array[String]): Unit = println(read("/named/file.txt") + ":" + read("/tree/value.txt"))
      |""".stripMargin
end ScopedTestResourcesSpec
