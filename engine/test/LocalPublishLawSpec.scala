package histos.publish

import histos.cache.ContentStore
import histos.central.Publisher
import histos.jdk.{JdkRuntime, JdkVersion, Sha256}
import histos.resolve.Resolve
import histos.toolchain.RuntimeIdentity
import histos.version.{ShipName, VersionTask}
import histos.workspace.Workspace
import histos.{
  Jvm,
  License,
  PomFact,
  ProjectGraph,
  ProjectName,
  Projects,
  Ship,
  license,
  module,
  organization,
  ship,
  ships,
  url,
}
import histos.`scala`

import java.nio.file.{Files, Path}

import slayer.stubbed

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object LocalPublishLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val number = ship"1.2.3"
  private val group  = organization"example.com"
  private val app    = module("app", Jvm)(scala3)
  private val core   = module("core", Jvm)(scala3)
  private val extra  = module("extra", Jvm)(scala3)

  private val stock = stubbed[Resolve] ++ stubbed[Publisher] ++ stubbed[ContentStore]

  def spec = suite("local publish laws")(
    test("a development session is staged locally until the organization is missing, and no classes are written"):
      ZIO
        .scoped {
          Workspace.work("histos-local-org").flatMap { repo =>
            for
              graph <- ZIO.fromEither(Projects(ships(Ship("demo").next(1, 2, 3)(app)), app)).orDieWith(blow)
              found <- publish(repo, graph, Nil, Map.empty, PublishWrite.Local).either
              wrote <- touched(repo)
            yield assertTrue(
              found == Left(LocalPublishError.NoOrganization),
              LocalPublishError.NoOrganization.message ==
                "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")",
              !wrote,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("a release with no license, developer, or scm is refused before classes exist"):
      ZIO
        .scoped {
          Workspace.work("histos-local-facts").flatMap { repo =>
            val env = Map("HISTOS_SESSION" -> "release")
            for
              plain    <- ZIO.fromEither(organized(app)).orDieWith(blow)
              licensed <- ZIO.fromEither(withLicense(app)).orDieWith(blow)
              _        <- Workspace.writeString(VersionTask.path(repo), "")
              missing  <- publish(repo, plain, Nil, env, PublishWrite.Release).either
              partial  <- publish(repo, licensed, Nil, env, PublishWrite.Release).either
              wrote    <- touched(repo)
            yield assertTrue(
              missing == Left(LocalPublishError.Facts(::(PomFact.License, PomFact.Developer :: PomFact.Scm :: Nil))),
              missing.fold(_.message == "a release needs a license, a developer, an scm", _ => false),
              partial == Left(LocalPublishError.Facts(::(PomFact.Developer, PomFact.Scm :: Nil))),
              partial.fold(_.message == "a release needs a developer, an scm", _ => false),
              !wrote,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("naming a held ship writes that hold and does not compile the ship that would release"):
      ZIO
        .scoped {
          Workspace.work("histos-local-hold").flatMap { repo =>
            val built = Projects.organization(group)(
              ships(Ship("kept").next(1, 2, 3)(core), Ship("open").next(1, 2, 3)(extra)),
              core,
              extra,
            )
            for
              graph <- ZIO.fromEither(built).orDieWith(blow)
              _     <- Workspace.writeString(VersionTask.path(repo), "kept matches\n")
              found <- publish(
                repo,
                graph,
                List(PublishName.Ship(ShipName("kept"))),
                Map("HISTOS_SESSION" -> "release"),
                PublishWrite.Release,
              )
              wrote <- touched(repo)
            yield assertTrue(
              found == PublishOutput(Nil, Nil, List(s"kept compile ${number.render} hold already-released"), Nil),
              !wrote,
            )
          }
        }
        .provideSomeLayer[TestEnvironment & Scope](stock)
    ,
    test("a rendered module prefers its unique coordinate, otherwise the jar under the root"):
      ZIO.scoped {
        Workspace.work("histos-local-rendered").flatMap { repo =>
          val jar      = repo.resolve("org").resolve("example").resolve("app.jar")
          val outside  = Path.of("/var/histos-outside.jar").toAbsolutePath.normalize
          val rootName = Option(repo.getFileName).map(_.toString).getOrElse("")
          for
            _ <- Workspace.writeString(jar, "jar")
            placed = row(Some(jar), None, number.render).rendered(Some(repo))
            unique = row(Some(jar), Some(s"${number.render}-abcdefabcdef-SNAPSHOT"), number.render)
              .rendered(Some(repo))
            bare    = row(None, None, number.render).rendered(Some(repo))
            escaped = row(Some(outside), None, number.render).rendered(Some(repo))
          yield assertTrue(
            placed == s"${group.value} app ${number.render} org/example/app.jar",
            rootName.nonEmpty,
            !placed.contains(rootName),
            unique == s"${group.value} app ${number.render} ${number.render}-abcdefabcdef-SNAPSHOT",
            !unique.contains("org/example"),
            bare == s"${group.value} app ${number.render} ${number.render}",
            escaped == s"${group.value} app ${number.render} ${outside.toString.replace('\\', '/')}",
          )
        }
      },
  )

  private def organized(member: histos.ProjectDef): Either[histos.BuildError, ProjectGraph] =
    Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(member)), member)

  private def withLicense(member: histos.ProjectDef): Either[histos.BuildError, ProjectGraph] =
    Projects
      .organization(group)
      .license(License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt"))(
        ships(Ship("demo").next(1, 2, 3)(member)),
        member,
      )

  private def publish(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      env: Map[String, String],
      write: PublishWrite,
  ): ZIO[Resolve & Publisher & ContentStore, LocalPublishError, PublishOutput] =
    ZIO.fromEither(JdkVersion.from("21")).orDieWith(blow).flatMap { version =>
      val runtime = JdkRuntime(Path.of("jdk"), version, RuntimeIdentity.Local(Path.of("jdk"), Sha256.of(Chunk.empty)))
      LocalPublish.run(repo, graph, names, runtime, env, write)
    }

  private def row(jar: Option[Path], unique: Option[String], revision: String): PublishedModule =
    PublishedModule(
      ProjectName("app"),
      group,
      "app",
      revision,
      jar,
      None,
      None,
      None,
      ShipName("demo"),
      Sha256.of(Chunk.empty),
      unique,
      Nil,
    )

  private def touched(repo: Path): ZIO[Any, Nothing, Boolean] =
    ZIO.attemptBlocking(Files.exists(repo.resolve(".histos"))).orDie

  private def blow(error: Any): IllegalStateException =
    new IllegalStateException(error.toString)
end LocalPublishLawSpec
