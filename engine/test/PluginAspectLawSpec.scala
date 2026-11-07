package histos.load

import histos.plugin.{DeclaredOutput, DeclaredTask, OutputName, PluginOutput, PluginRunError, RepoPath, Seal, TaskName, WrittenFile}
import histos.step.{Need, Phase, Step, StepId, StepInfo}
import histos.workspace.Workspace
import histos.{Jvm, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, ZIO}
import zio.test.*

/** A declared output is a file on disk. An extra path fails before that check, and the loader runs the effect.
  */
object PluginAspectLawSpec extends ZIOSpecDefault:
  private val jar  = OutputName("jar")
  private val path = RepoPath("out/app.jar")
  private val info = StepInfo(
    StepId.Cell(ProjectName.trusted("core"), ScalaVersion.trusted("3.10.0"), Jvm, Phase.Run),
    Chunk.empty,
    Set(Need.Cpu),
  )

  def spec = suite("plugin aspects")(
    test("no recorded output succeeds, an extra path is refused, and a declared file has to be on disk"):
      ZIO.scoped {
        Workspace.work("histos-plugin-aspect").flatMap { repo =>
          val task = declared(List(DeclaredOutput(jar, path)))
          for
            empty  <- run(repo, task, None)
            extra  <- run(repo, task, Some(PluginOutput("ok", List(WrittenFile(jar, RepoPath("nope/out.txt"))))))
            absent <- run(repo, task, Some(PluginOutput("ok", List(WrittenFile(jar, path)))))
            _      <- write(repo.resolve(path.value))
            link   <- ZIO.attemptBlocking {
              Files.createSymbolicLink(repo.resolve("out").resolve("link.jar"), repo.resolve(path.value))
            }.orDie
            linked <- run(
              repo,
              declared(List(DeclaredOutput(OutputName("link"), RepoPath("out/link.jar")))),
              Some(PluginOutput("ok", List(WrittenFile(OutputName("link"), RepoPath("out/link.jar"))))),
            )
            wrote <- run(repo, task, Some(PluginOutput("ok", List(WrittenFile(jar, path)))))
          yield
            val symlink = Files.isSymbolicLink(link)
            assertTrue(
              empty == Right(()),
              extra == Left(PluginFault.Task(PluginRunError.ExtraOutput("nope/out.txt"))),
              absent == Left(PluginFault.Task(PluginRunError.MissingOutput(jar))),
              linked == Left(PluginFault.Task(PluginRunError.MissingOutput(OutputName("link")))),
              wrote == Right(()),
              symlink,
            )
        }
      }
    ,
    test("the loader aspect returns what the effect returned"):
      ZIO.scoped {
        PluginThreads.make(getClass.getClassLoader).flatMap { threads =>
          val step = Step.local[Int](info)(ZIO.succeed(7)) @@ PluginAspects.loader(threads)
          Step.run(step).map(value => assertTrue(value == 7))
        }
      }
  )

  private def declared(outputs: List[DeclaredOutput]): DeclaredTask =
    DeclaredTask(TaskName("pack"), Seal.Hermetic, Set.empty, Nil, outputs, Nil)

  private def run(
      repo: Path,
      task: DeclaredTask,
      output: Option[PluginOutput],
  ) =
    for
      slot <- Ref.make(output)
      step = Step.local[Unit](info)(ZIO.unit) @@ PluginAspects.declaredOutputs(repo, task, slot)
      ran <- Step.run(step).either
    yield ran

  private def write(path: Path) =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, "jar")
      }
      .orDie
end PluginAspectLawSpec
