package histos.preview

import histos.scala
import histos.workspace.Workspace
import histos.{Jvm, Module, ProjectGraph, Projects, module}

import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}
import zio.test.*

/** Quick preview names suites from the files on disk and does not skip one it cannot cover. */
object QuickAnswerLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("quick answer")(
    test("a missing class directory is uncompiled"):
      inRepo: (repo, graph, mod) =>
        QuickAnswer.read(repo, mod, List(mod), scala3, Jvm, graph).map: answer =>
          assertTrue(answer == QuickAnswer.Uncompiled)
    ,
    test("an empty class directory with no records is uncompiled"):
      inRepo: (repo, graph, mod) =>
        for
          _      <- emptyDir(classes(repo))
          answer <- QuickAnswer.read(repo, mod, List(mod), scala3, Jvm, graph)
        yield assertTrue(answer == QuickAnswer.Uncompiled)
    ,
    test("a class file names its suite, and an inner or anonymous class does not"):
      inRepo: (repo, graph, mod) =>
        val root = classes(repo)
        for
          _      <- touch(root.resolve("demo").resolve("Hello.class"))
          _      <- touch(root.resolve("demo").resolve("Hello$.class"))
          _      <- touch(root.resolve("demo").resolve("Hello$Inner.class"))
          _      <- touch(root.resolve("demo").resolve("Hello$$anon.class"))
          _      <- touch(root.resolve("other").resolve("World.class"))
          answer <- QuickAnswer.read(repo, mod, List(mod), scala3, Jvm, graph)
        yield assertTrue(answer == QuickAnswer.Named(Nil, Nil, List("demo.Hello", "other.World")))
    ,
    test("only an anonymous class is partial, because no suite could be named"):
      inRepo: (repo, graph, mod) =>
        for
          _      <- touch(classes(repo).resolve("demo").resolve("Hello$$anon.class"))
          answer <- QuickAnswer.read(repo, mod, List(mod), scala3, Jvm, graph)
        yield assertTrue(answer == QuickAnswer.Partial(Nil))
    ,
    test("a record file is a name, and the same dollars in a class file are not"):
      inRepo: (repo, graph, mod) =>
        for
          _      <- emptyDir(classes(repo))
          _      <- touch(records(repo).resolve("demo").resolve("Hello$$anon"))
          answer <- QuickAnswer.read(repo, mod, List(mod), scala3, Jvm, graph)
        yield assertTrue(answer == QuickAnswer.Named(Nil, Nil, List("demo.Hello$$anon")))
  )

  private def inRepo(
      use: (Path, ProjectGraph, Module) => UIO[TestResult]
  ) =
    ZIO.scoped {
      Workspace.work("histos-quick-answer").flatMap { repo =>
        val defined = module("quick", Jvm)(scala3)
        for
          graph  <- ZIO.fromEither(Projects(defined))
          mod    <- only(graph)
          result <- use(repo, graph, mod)
        yield result
      }
    }

  private def only(graph: ProjectGraph): UIO[Module] =
    graph.projects.collect { case item: Module => item } match
      case item :: Nil => ZIO.succeed(item)
      case _           => ZIO.dieMessage("quick answer fixture is one module")

  private def classes(repo: Path): Path =
    repo.resolve(".histos").resolve("test-classes").resolve("quick").resolve(scala3.value).resolve(Jvm.token)

  private def records(repo: Path): Path =
    repo.resolve(".histos").resolve("test-quick").resolve("quick").resolve(scala3.value).resolve(Jvm.token)

  private def touch(path: Path): UIO[Unit] =
    Workspace.writeString(path, "").catchAll(error => ZIO.dieMessage(error.message))

  private def emptyDir(path: Path): UIO[Unit] =
    val marker = path.resolve(".keep")
    touch(marker) *> ZIO.attemptBlocking(Files.delete(marker)).orDie
end QuickAnswerLawSpec
