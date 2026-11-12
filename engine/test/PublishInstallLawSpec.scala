package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.plugin.ApiDigest
import histos.resolve.{DependencyLock, Resolve}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{CompilerClosure, HistosBuild, Jvm, ProjectName, Projects, Ship, module, organization, ships}

import java.nio.file.{Files, Path}

import zio.{Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** `publish local` compiles the module and installs the snapshot coordinate into the law's own ivy root.
  */
object PublishInstallLawSpec extends ZIOSpecDefault:
  private val hostApi = ApiDigest.trusted("ab" * 32)
  private val group   = organization"histos.runlaw"

  def spec = suite("publish install laws")(
    test("a development publish installs the snapshot jar into ivy2 local and names that coordinate"):
      ZIO.serviceWithZIO[CompilerClosure] { closure =>
        val app = module("runlaw", Jvm)(closure.scalaVersion)
        Workspace.work("histos-publish-install").flatMap { repo =>
          val ivy    = repo.resolve("ivy")
          val source = repo.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
          val run    =
            for
              machine <- Machine.load(repo)
              jdk     <- GraphCompile.jdk(machine)
              sha     <- LawGit.ready(repo)
              revision = s"1.2.3-${sha.abbreviate.value}-SNAPSHOT"
              graph <- ZIO.fromEither(Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(app)), app))
              _     <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
              _     <- Workspace.writeString(
                repo.resolve("dependencies.lock"),
                DependencyLock.render(closure.pinning(ProjectName("runlaw"))),
              )
              notes <- Ref.make(List.empty[String])
              jar = placed(ivy, revision, "jars", "jar")
              pom = placed(ivy, revision, "poms", "pom")
              found <- PublishRun
                .local(
                  repo,
                  graph,
                  HistosBuild.fixed(graph),
                  Nil,
                  jdk,
                  Map.empty,
                  hostApi,
                  _ => ZIO.unit,
                  line => notes.update(_ :+ line),
                )
                .either
              said   <- notes.get
              onDisk <- present(jar, pom)
            yield assertTrue(
              found.map(_.lines) == Right(List(s"${group.value} runlaw_3 $revision")),
              said.contains(s"installing runlaw_3 $revision"),
              !said.exists(_.contains("-ci")),
              onDisk,
            )
          run.provideSomeLayer[Resolve & Scope](
            ZLayer.succeed(ContentStore.in(repo.resolve("store"))) ++ ZLayer.succeed(IvyRoot.at(ivy))
          )
        }
      }
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private def present(jar: Path, pom: Path) =
    ZIO.attemptBlocking(Files.isRegularFile(jar) && Files.isRegularFile(pom)).orDie

  /** `Pattern.default`: `[organisation]/[module]/[revision]/[type]s/[artifact].[ext]`. */
  private def placed(root: Path, revision: String, kind: String, ext: String): Path =
    root.resolve(group.value).resolve("runlaw_3").resolve(revision).resolve(kind).resolve(s"runlaw_3.$ext")
end PublishInstallLawSpec
