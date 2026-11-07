package histos.publish

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{HistosBuild, Jvm, Platform, ProjectName, Projects, Ship, module, organization, ships}
import histos.scala

import java.nio.file.{Files, Path}

import zio.{Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** `publish local` compiles the module and installs that `-ci` coordinate where Coursier reads ivy2 local.
  */
object PublishInstallLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val group  = organization"histos.runlaw"
  private val app    = module("runlaw", Jvm)(scala3)

  def spec = suite("publish install laws")(
    test("a development publish installs the ci jar into ivy2 local and names that coordinate"):
      ZIO.scoped {
        Workspace.work("histos-publish-install").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val ivy = localRoot
                val source = repo.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                val store  = repo.resolve("store")
                val built  = Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(app)), app)
                val jar    = placed(ivy, "jars", "jar")
                val pom    = placed(ivy, "poms", "pom")
                val run =
                  for
                    graph <- ZIO.fromEither(built).catchAll(error => ZIO.dieMessage(error.toString))
                    _     <- Workspace
                      .writeString(source, "object Hello:\n  def ready: Int = 1\n")
                      .catchAll(error => ZIO.dieMessage(error.message))
                    _ <- Workspace
                      .writeString(
                        repo.resolve("dependencies.lock"),
                        DependencyLock.render(pinned(lock, ProjectName("runlaw"))),
                      )
                      .catchAll(error => ZIO.dieMessage(error.message))
                    notes <- Ref.make(List.empty[String])
                    found <- PublishRun
                      .local(
                        repo,
                        graph,
                        HistosBuild.fixed(graph),
                        Nil,
                        jdk,
                        Map.empty,
                        _ => ZIO.unit,
                        line => notes.update(_ :+ line),
                      )
                      .either
                    said    <- notes.get
                    onDisk  <- present(jar, pom)
                    _       <- wipe(ivy)
                  yield assertTrue(
                    found.map(_.lines) == Right(List(s"${group.value} runlaw_3 1.2.3-ci")),
                    said.contains(s"installing runlaw_3 1.2.3-ci"),
                    onDisk,
                  )
                run
                  .provideSomeLayer[Resolve](ZLayer.succeed(ContentStore.in(store)))
                  .ensuring(wipe(ivy))
              }
            }
          }
        }
      }
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def present(jar: Path, pom: Path) =
    ZIO.attemptBlocking(Files.isRegularFile(jar) && Files.isRegularFile(pom)).orDie

  private def wipe(root: Path) =
    Workspace.delete(root.resolve(group.value)).catchAll(error => ZIO.dieMessage(error.message))

  /** `Pattern.default`: `[organisation]/[module]/[revision]/[type]s/[artifact].[ext]`. */
  private def placed(root: Path, kind: String, ext: String): Path =
    root.resolve(group.value).resolve("runlaw_3").resolve("1.2.3-ci").resolve(kind).resolve(s"runlaw_3.$ext")

  private def localRoot: Path =
    val configured =
      Option(System.getProperty("coursier.ivy.home"))
        .filter(_.nonEmpty)
        .orElse(Option(System.getProperty("ivy.home")).filter(_.nonEmpty))
    val base = configured
      .orElse(Option(System.getProperty("user.home")).filter(_.nonEmpty).map(home => s"$home/.ivy2"))
      .getOrElse("")
    Path.of(base).resolve("local")

  private def pinned(lock: DependencyLock, names: ProjectName*): DependencyLock =
    val added = names.toList.map: name =>
      LockedCell(name, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    lock.copy(cells = added ++ lock.cells)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end PublishInstallLawSpec
