package histos.cli

import histos.preview.Plan
import histos.server.{Calls, Feed, Say}
import histos.{Jvm, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, ZIO}
import zio.test.*

object PreviewSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  private val preview = suite("preview compile")(
    test("preview compile is one command"):
      for
        named <- Cli.parse(List("preview", "compile", "app"))
        all <- Cli.parse(List("preview", "compile"))
        bare <- Cli.parse(List("preview")).either
        other <- Cli.parse(List("preview", "test")).either
      yield assertTrue(
        named == Command.PreviewCompile(List("app")),
        all == Command.PreviewCompile(Nil),
        bare == Left(CliParseError.Help),
        other == Left(CliParseError.UnknownCommand)
      )
    ,
    test("the handler prints the plan and creates no classes directory"):
      ZIO.scoped {
        for
          repo <- keep
          _ <- write(repo, "core/src/main/scala/Core.scala", "object Core")
          _ <- write(repo, "app/src/main/scala/App.scala", "object App")
          _ <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          core = module("core", Jvm)(scala3)
          app = module("app", Jvm)(scala3).dependsOn(core)
          graph <- ZIO.fromEither(Projects(core, app))
          text <- Plan.show(repo, graph, List("app"))
          feeds <- Ref.make(Chunk.empty[Feed])
          _ <- Calls.previewCompile(repo, graph, List("app"), Say.record(feeds))
          heard <- feeds.get
          outs = heard.collect { case Feed.Out(line) => line }
          exit = heard.collect { case Feed.Exit(code, err) => (code, err) }
        yield assertTrue(
          outs == Chunk(text),
          exit == Chunk((CliExit.Success.code, "")),
          text.startsWith("file-level staleness: unknown"),
          text.contains("core 3.10.0 jvm upstream"),
          text.contains("app 3.10.0 jvm selected"),
          text.contains("upstreams core"),
          text.contains("sources core/src/main/scala/Core.scala"),
          text.contains("sources app/src/main/scala/App.scala"),
          !text.contains("AppSpec"),
          !text.contains("duration"),
          !Files.exists(repo.resolve(".histos"))
        )
      }
    ,
    test("an unknown project fails the plan and creates no classes directory"):
      ZIO.scoped {
        for
          repo <- keep
          core = module("core", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(core))
          feeds <- Ref.make(Chunk.empty[Feed])
          _ <- Calls.previewCompile(
            repo,
            graph,
            List("missing"),
            Say.record(feeds)
          )
          heard <- feeds.get
        yield assertTrue(
          heard.collect { case Feed.Exit(code, err) => (code, err) } == Chunk(
            (CliExit.CompileFailed.code, "'missing' is not a project")
          ),
          !Files.exists(repo.resolve(".histos").resolve("classes"))
        )
      }
  )

  private def keep: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-preview-cli"))
    )(delete)

  private def write(
      root: Path,
      name: String,
      text: String
  ): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.writeString(path, text)
    }

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = preview
