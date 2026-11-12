package histos.scaffold

import histos.workspace.Workspace

import java.nio.file.Files

import zio.ZIO
import zio.test.*

object ScaffoldLawSpec extends ZIOSpecDefault:
  private val pin = "9.9.9"

  def spec = suite("scaffold laws")(
    test("a version argument is the pin, and histos alone is refused"):
      assertTrue(
        Skeleton.versionOf(Some(pin)) == Right(pin),
        Skeleton.versionOf(Some(s"histos $pin")) == Right(pin),
        Skeleton.versionOf(Some(s"  $pin  ")) == Right(pin),
        Skeleton.versionOf(None) == Left(SkeletonError.Version),
        Skeleton.versionOf(Some("histos")) == Left(SkeletonError.Version),
        Skeleton.versionOf(Some("")) == Left(SkeletonError.Version),
        ImportSource.from("sbt") == Some(ImportSource.Sbt),
        ImportSource.from("zipx") == Some(ImportSource.Zipx),
        ImportSource.from("mill") == None,
      )
    ,
    test("a full project writes the wrapper and the ignore file, and a bare project does not"):
      Workspace.work("histos-scaffold").flatMap { parent =>
        for
          demo       <- Skeleton.create(parent, "demo", pin, ScaffoldKind.Full)
          bare       <- Skeleton.create(parent, "bare", pin, ScaffoldKind.Bare)
          build      <- ZIO.attemptBlocking(Files.readString(demo.resolve("histos.scala"))).orDie
          props      <- ZIO.attemptBlocking(Files.readString(demo.resolve("build.properties"))).orDie
          ignore     <- ZIO.attemptBlocking(Files.readString(demo.resolve(".gitignore"))).orDie
          script     <- ZIO.attemptBlocking(Files.readString(demo.resolve("histos"))).orDie
          exec       <- ZIO.attemptBlocking(Files.isExecutable(demo.resolve("histos"))).orDie
          bareBuild  <- ZIO.attemptBlocking(Files.exists(bare.resolve("histos.scala"))).orDie
          bareIgnore <- ZIO.attemptBlocking(Files.exists(bare.resolve(".gitignore"))).orDie
          bareScript <- ZIO.attemptBlocking(Files.exists(bare.resolve("histos"))).orDie
        yield assertTrue(
          build.contains("""module("app", Jvm)"""),
          build.contains(s"""scala"${Skeleton.scalaVersion}""""),
          build.contains(s"""dep"dev.zio::zio:${Skeleton.zioVersion}""""),
          build.contains("""MainClass("demo.Main")"""),
          props == s"histos.version=$pin\n",
          Skeleton.ignoreLines.forall(ignore.contains),
          script == Skeleton.wrapper,
          exec,
          bareBuild,
          !bareIgnore,
          !bareScript,
        )
      }
    ,
    test("a bad name is refused, and a second create does not overwrite"):
      Workspace.work("histos-scaffold-name").flatMap { parent =>
        val occupied = parent.resolve("demo")
        for
          bad    <- Skeleton.create(parent, "1bad", pin).either
          _      <- Skeleton.create(parent, "demo", pin, ScaffoldKind.Bare)
          second <- Skeleton.create(parent, "demo", pin, ScaffoldKind.Bare).either
        yield assertTrue(
          bad == Left(SkeletonError.Name("1bad")),
          second == Left(SkeletonError.NotEmpty(occupied)),
        )
      }
    ,
    test("init writes the draft it was given, and an existing build file is refused"):
      Workspace.work("histos-scaffold-init").flatMap { parent =>
        val demo     = parent.resolve("demo")
        val bad      = parent.resolve("1bad")
        val occupied = parent.resolve("taken")
        val draft    = "object build extends HistosBuild:\n"
        for
          _ <- ZIO.attemptBlocking {
            Files.createDirectories(demo)
            Files.createDirectories(bad)
            Files.createDirectories(occupied)
            Files.writeString(occupied.resolve("histos.scala"), "already\n")
            ()
          }.orDie
          made    <- Skeleton.init(demo, pin, ScaffoldKind.Full, Some(draft))
          text    <- ZIO.attemptBlocking(Files.readString(made.resolve("histos.scala"))).orDie
          props   <- ZIO.attemptBlocking(Files.readString(made.resolve("build.properties"))).orDie
          sources <- ZIO.attemptBlocking(Files.exists(made.resolve("app/src/main/scala/demo/Main.scala"))).orDie
          named   <- Skeleton.init(bad, pin).either
          again   <- Skeleton.init(occupied, pin).either
        yield assertTrue(
          text == draft,
          props == s"histos.version=$pin\n",
          !sources,
          named == Left(SkeletonError.Name("1bad")),
          again == Left(SkeletonError.Exists(occupied.resolve("histos.scala"))),
        )
      },
  )
end ScaffoldLawSpec
