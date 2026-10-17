package histos.version

import histos.*
import histos.task.{TaskError, TaskKind}

import java.nio.file.Files

import zio.test.*
import zio.*

object VersionTaskSpec extends ZIOSpecDefault:
  private val number = ShipNumber("1.4.2")
  private val sha = CommitSha("1234567890abcdef1234567890abcdef12345678")

  private enum FixtureError:
    case Ship(error: ShipNameError)
    case Project(error: ProjectNameError)
    case Rows(error: CatalogError)

    def message: String = this match
      case Ship(error)    => error.message
      case Project(error) => error.message
      case Rows(error)    => error.message

  private val opened =
    for
      libs    <- ShipName.from("libs").left.map(FixtureError.Ship(_))
      core    <- ProjectName.from("core").left.map(FixtureError.Project(_))
      catalog <- Catalog.from(List(Ship.Line(libs, number, ::(core, Nil)))).left.map(FixtureError.Rows(_))
    yield (libs, catalog)

  private def use(f: (ShipName, Catalog) => ZIO[Any, Any, TestResult]): ZIO[Any, Any, TestResult] =
    opened match
      case Right(libs, catalog) => f(libs, catalog)
      case Left(error)          => ZIO.succeed(assertTrue(error.message == "fixture"))

  private val task = suite("versions task")(
    test("development names -ci and says whether the registry was read"):
      use: (_, catalog) =>
        for
          checked <- VersionTask.effect(catalog, Map.empty, Evidence.Checked(Registry.empty))
          missed  <- VersionTask.effect(catalog, Map.empty, Evidence.Unchecked)
        yield assertTrue(
          checked.kind == TaskKind.Versions && checked.text.contains("1.4.2-ci") && checked.text.contains("drift clear"),
          missed.text.contains("drift unchecked"),
        )
    ,
    test("a manual ship publishes its stated version without a registry file"):
      val core = module("core", Jvm)(scala"3.9.0")
      Projects(ships(manual("libs", manual"1.2.3-SNAPSHOT")(core)), core) match
        case Left(error) => ZIO.succeed(assertTrue(error.message == "manual"))
        case Right(graph) =>
          ZIO.scoped:
            for
              dir <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-manual"))): path =>
                ZIO.attempt(Files.deleteIfExists(path)).ignore
              done <- VersionTask.fromRepo(graph, dir, Map(Session.Key -> "snapshot"))
            yield assertTrue(
              done.text.contains("session manual"),
              done.text.contains("compile 1.2.3-SNAPSHOT publish 1.2.3-SNAPSHOT"),
              !done.text.contains("-ci"),
            )
    ,
    test("a snapshot stores the commit, and a missing registry read publishes nothing"):
      use: (_, catalog) =>
        val env = Map(Session.Key -> "snapshot", Session.ShaKey -> sha.value, Session.BranchKey -> "default")
        for
          published <- VersionTask.effect(catalog, env, Evidence.Checked(Registry.empty))
          refused   <- VersionTask.effect(catalog, env, Evidence.Unchecked).either
        yield assertTrue(
          published.text.contains("publish 1.4.2-1234567890ab-SNAPSHOT") && published.text.contains("pointer 1.4.2-SNAPSHOT"),
          refused match
            case Left(TaskError.Version(VersionError.Refused(Refusal.Unchecked :: _))) => true
            case other => other.fold(_.message, _.text) == "unchecked"
          ,
        )
    ,
    test("a moved release refuses the task, and a missing file is unchecked"):
      use: (libs, catalog) =>
        val env = Map(Session.Key -> "release")
        Registry.parse(List(s"${libs.value} moved")) match
          case Left(error) => ZIO.succeed(assertTrue(error.message == "moved"))
          case Right(registry) =>
            val core = module("core", Jvm)(scala"3.9.0")
            Projects(ships(ship("libs", ship"1.4.2")(core)), core) match
              case Left(error) => ZIO.succeed(assertTrue(error.message == "libs"))
              case Right(graph) =>
                ZIO.scoped:
                  for
                    dir <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-versions"))): path =>
                      ZIO.attempt(Files.deleteIfExists(path)).ignore
                    moved <- VersionTask.effect(catalog, env, Evidence.Checked(registry)).either
                    missed <- VersionTask.fromRepo(graph, dir, env).either
                  yield assertTrue(
                    moved match
                      case Left(TaskError.Version(VersionError.Refused(Refusal.Shadowed(shipName, released) :: Nil))) =>
                        shipName == libs && released == number
                      case other => other.fold(_.message, _.text) == "shadowed"
                    ,
                    missed match
                      case Left(TaskError.Version(VersionError.Refused(Refusal.Unchecked :: Nil))) => true
                      case other => other.fold(_.message, _.text) == "unchecked"
                    ,
                  )
    ,
  )

  def spec = task
