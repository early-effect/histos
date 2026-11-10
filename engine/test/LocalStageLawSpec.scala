package histos.publish

import histos.cache.ContentStore
import histos.central.Publisher
import histos.jdk.Machine
import histos.plugin.ApiDigest
import histos.PluginSources
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.version.ShipName
import histos.{
  GeneratedAsset,
  Jvm,
  MainClass,
  Platform,
  ProjectName,
  Projects,
  ResourceName,
  Ship,
  module,
  organization,
  ships,
}
import histos.scala

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** A development publish compiles one library and writes the snapshot coordinate of HEAD. The compiler is the locked
  * one, read offline.
  */
object LocalStageLawSpec extends ZIOSpecDefault:
  private val scala3  = scala"3.10.0"
  private val group   = organization"example.com"
  private val app     = module("stage", Jvm)(scala3)
  private val hostApi = ApiDigest.trusted("ab" * 32)

  def spec = suite("local stage")(
    test(
      "a development publish writes the snapshot jar, its sources, and its javadoc, and the pom has no dependencies"
    ):
      ZIO.scoped {
        Workspace.work("histos-local-stage").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val source = repo.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                val maven  = repo.resolve("maven")
                val store  = repo.resolve("store")
                val built  = Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(app)), app)
                for
                  graph <- ZIO.fromEither(built)
                  sha   <- LawGit.ready(repo)
                  _     <- Workspace.writeString(source, "object Hello:\n  def ready: Int = 1\n")
                  _     <- Workspace.writeString(
                    repo.resolve("dependencies.lock"),
                    DependencyLock.render(pinned(lock, ProjectName("stage"))),
                  )
                  found <- LocalPublish
                    .run(repo, graph, Nil, jdk, Map.empty, PublishWrite.Local, hostApi)
                    .provideSomeLayer[Resolve](
                      Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store))
                    )
                  text          <- pomOf(found)
                  jars          <- onDisk(maven, found.modules.flatMap(_.jar.toList))
                  sources       <- onDisk(maven, found.modules.flatMap(_.sources.toList))
                  javadoc       <- onDisk(maven, found.modules.flatMap(_.javadoc.toList))
                  binaryVersion <- zipTextOf(found.modules.flatMap(_.jar.toList), ApiDigest.resource)
                  sourceVersion <- zipTextOf(found.modules.flatMap(_.sources.toList), ApiDigest.resource)
                  revision = coordinate(sha)
                yield assertTrue(
                  found.skipped.isEmpty,
                  found.held.isEmpty,
                  found.refused.isEmpty,
                  found.modules.map(line =>
                    (line.group, line.artifact, line.revision, line.ship, line.unique, line.dependencies.isEmpty)
                  ) == List((group, "stage_3", revision, ShipName("demo"), None, true)),
                  jars == List(OnDisk(true, true, true)),
                  sources == List(OnDisk(true, true, true)),
                  javadoc == List(OnDisk(true, true, true)),
                  text.contains("<groupId>example.com</groupId>"),
                  text.contains("<artifactId>stage_3</artifactId>"),
                  text.contains(s"<version>$revision</version>"),
                  !text.contains("-ci"),
                  !text.contains("<dependencies>"),
                  binaryVersion.isEmpty,
                  sourceVersion.isEmpty,
                )
              }
            }
          }
        }
      }
    ,
    test("a dependent module's pom names the upstream at the same snapshot revision"):
      ZIO.scoped {
        Workspace.work("histos-local-edge").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val base  = module("base", Jvm)(scala3)
                val edge  = module("edge", Jvm)(scala3).dependsOn(base)
                val maven = repo.resolve("maven")
                val store = repo.resolve("store")
                val built = Projects
                  .organization(group)
                  .locked(ships(Ship("demo").next(1, 2, 3)(base, edge)), base, edge)
                for
                  graph <- ZIO.fromEither(built)
                  sha   <- LawGit.ready(repo)
                  _     <- Workspace.writeString(
                    repo.resolve("base").resolve("src").resolve("main").resolve("scala").resolve("Base.scala"),
                    "object Base:\n  def ready: Int = 1\n",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("edge").resolve("src").resolve("main").resolve("scala").resolve("Edge.scala"),
                    "object Edge:\n  def ready: Int = Base.ready\n",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("dependencies.lock"),
                    DependencyLock.render(pinned(lock, ProjectName("base"), ProjectName("edge"))),
                  )
                  found <- LocalPublish
                    .run(repo, graph, Nil, jdk, Map.empty, PublishWrite.Local, hostApi)
                    .provideSomeLayer[Resolve](
                      Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store))
                    )
                  disks <- ZIO.foreach(found.modules) { line =>
                    onDisk(maven, line.jar.toList ++ line.sources.toList ++ line.javadoc.toList)
                  }
                  poms <- ZIO.foreach(found.modules)(pomText)
                  revision = coordinate(sha)
                yield
                  val block =
                    s"""    <dependency>
                       |      <groupId>example.com</groupId>
                       |      <artifactId>base_3</artifactId>
                       |      <version>$revision</version>
                       |    </dependency>""".stripMargin
                  val coords = found.modules.map { line =>
                    val deps = line.dependencies.map(dep => (dep.group.value, dep.name.value, dep.version.value))
                    (line.artifact, line.revision, deps)
                  }
                  val byPom = found.modules.map(_.artifact).zip(poms).toMap
                  assertTrue(
                    found.skipped.isEmpty,
                    found.held.isEmpty,
                    found.refused.isEmpty,
                    coords == List(
                      ("base_3", revision, Nil),
                      ("edge_3", revision, List(("example.com", "base_3", revision))),
                    ),
                    disks == List.fill(2)(List.fill(3)(OnDisk(true, true, true))),
                    byPom.get("base_3").exists(text => !text.contains("<dependencies>")),
                    byPom.get("edge_3").exists(_.contains(block)),
                  )
              }
            }
          }
        }
      }
    ,
    test("an assembly publishes the unsuffixed jar and its manifest names the main class"):
      ZIO.scoped {
        Workspace.work("histos-local-assembly").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val pack  = module("pack", Jvm)(scala3).assembly(MainClass("Hello"))
                val maven = repo.resolve("maven")
                val store = repo.resolve("store")
                val built = Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(pack)), pack)
                for
                  graph <- ZIO.fromEither(built)
                  sha   <- LawGit.ready(repo)
                  _     <- Workspace.writeString(
                    repo.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala"),
                    "object Hello:\n  def main(args: Array[String]): Unit = ()\n",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("dependencies.lock"),
                    DependencyLock.render(pinned(lock, ProjectName("pack"))),
                  )
                  found <- LocalPublish
                    .run(repo, graph, Nil, jdk, Map.empty, PublishWrite.Local, hostApi)
                    .provideSomeLayer[Resolve](
                      Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store))
                    )
                  jars    <- onDisk(maven, found.modules.flatMap(_.jar.toList))
                  sources <- onDisk(maven, found.modules.flatMap(_.sources.toList))
                  javadoc <- onDisk(maven, found.modules.flatMap(_.javadoc.toList))
                  text    <- manifestOf(found)
                yield assertTrue(
                  found.skipped.isEmpty,
                  found.held.isEmpty,
                  found.refused.isEmpty,
                  found.modules.map(line => (line.artifact, line.revision, line.dependencies.isEmpty)) == List(
                    ("pack", coordinate(sha), true)
                  ),
                  jars == List(OnDisk(true, true, true)),
                  sources == List(OnDisk(true, true, true)),
                  javadoc == List(OnDisk(true, true, true)),
                  text.contains("Main-Class: Hello\r\n"),
                )
              }
            }
          }
        }
      }
    ,
    test("a library jar keeps unmanaged resources, and generated text of the same name wins"):
      ZIO.scoped {
        Workspace.work("histos-local-resources").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val noted = module("noted", Jvm)(scala3).resources(
                  GeneratedAsset.Text(ResourceName("note.txt"), "generated")
                )
                val maven = repo.resolve("maven")
                val store = repo.resolve("store")
                val built = Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(noted)), noted)
                for
                  graph <- ZIO.fromEither(built)
                  _     <- LawGit.ready(repo)
                  _     <- Workspace.writeString(
                    repo.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala"),
                    "object Hello:\n  def ready: Int = 1\n",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("src").resolve("main").resolve("resources").resolve("note.txt"),
                    "plain",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("src").resolve("main").resolve("resources").resolve("other.txt"),
                    "other",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("dependencies.lock"),
                    DependencyLock.render(pinned(lock, ProjectName("noted"))),
                  )
                  found <- LocalPublish
                    .run(repo, graph, Nil, jdk, Map.empty, PublishWrite.Local, hostApi)
                    .provideSomeLayer[Resolve](
                      Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store))
                    )
                  note          <- zipTextOf(found.modules.flatMap(_.jar.toList), "note.txt")
                  other         <- zipTextOf(found.modules.flatMap(_.jar.toList), "other.txt")
                  version       <- zipTextOf(found.modules.flatMap(_.jar.toList), ApiDigest.resource)
                  sources       <- zipTextOf(found.modules.flatMap(_.sources.toList), "note.txt")
                  sourceVersion <- zipTextOf(found.modules.flatMap(_.sources.toList), ApiDigest.resource)
                  classNote     <- Workspace.readIfPresent(
                    GraphCompile.classesDir(repo, ProjectName("noted"), scala3).resolve("note.txt")
                  )
                  refused = found.refused.isEmpty && found.skipped.isEmpty && found.held.isEmpty
                  count   = found.modules.length
                yield assertTrue(
                  refused,
                  count == 1,
                  note.contains("generated"),
                  other.contains("other"),
                  version.isEmpty,
                  sources.isEmpty,
                  sourceVersion.isEmpty,
                  classNote.contains("generated"),
                )
              }
            }
          }
        }
      }
    ,
    test("a committed plugin entry fails the publish and names that file"):
      ZIO.scoped {
        Workspace.work("histos-local-plugin").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val plug  = module("plug", Jvm)(scala3)
                val maven = repo.resolve("maven")
                val store = repo.resolve("store")
                val built = Projects.organization(group).locked(ships(Ship("demo").next(1, 2, 3)(plug)), plug)
                val entry = histos.AssetError.refusedName
                  .split("/")
                  .foldLeft(
                    repo.resolve("src").resolve("main").resolve("resources")
                  )(_.resolve(_))
                for
                  graph <- ZIO.fromEither(built)
                  _     <- LawGit.ready(repo)
                  _     <- Workspace.writeString(
                    repo.resolve("src").resolve("main").resolve("scala").resolve("Plug.scala"),
                    "object Plug:\n  def ready: Int = 1\n",
                  )
                  _ <- Workspace.writeString(entry, "plug\n")
                  _ <- Workspace.writeString(
                    repo.resolve("dependencies.lock"),
                    DependencyLock.render(pinned(lock, ProjectName("plug"))),
                  )
                  found <- LocalPublish
                    .run(repo, graph, Nil, jdk, Map.empty, PublishWrite.Local, hostApi)
                    .either
                    .provideSomeLayer[Resolve](
                      Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store))
                    )
                  text = found.fold(_.message, _ => "")
                yield assertTrue(
                  text.contains(entry.toString),
                  text.contains("delete it. A plugin is a layer, not a file."),
                )
              }
            }
          }
        }
      }
    ,
    test("a plugin coordinate's binary jar carries the api digest, and the sources jar and a sibling do not"):
      ZIO.scoped {
        Workspace.work("histos-local-stamp").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[DependencyLock] { lock =>
                val plugins = module("plugins", Jvm)(scala3)
                val sibling = module("sibling", Jvm)(scala3).dependsOn(plugins)
                val maven   = repo.resolve("maven")
                val store   = repo.resolve("store")
                val built   = Projects
                  .organization(group)
                  .plugins(PluginSources.module(plugins))
                  .locked(ships(Ship("demo").next(1, 2, 3)(plugins, sibling)), plugins, sibling)
                val stamp = ApiDigest.trusted("cd" * 32)
                for
                  graph <- ZIO.fromEither(built)
                  _     <- LawGit.ready(repo)
                  _     <- Workspace.writeString(
                    repo.resolve("plugins").resolve("src").resolve("main").resolve("scala").resolve("Mark.scala"),
                    "object Mark:\n  def ready: Int = 1\n",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("sibling").resolve("src").resolve("main").resolve("scala").resolve("Use.scala"),
                    "object Use:\n  def ready: Int = Mark.ready\n",
                  )
                  _ <- Workspace.writeString(
                    repo.resolve("dependencies.lock"),
                    DependencyLock.render(pinned(lock, ProjectName("plugins"), ProjectName("sibling"))),
                  )
                  found <- LocalPublish
                    .run(repo, graph, Nil, jdk, Map.empty, PublishWrite.Local, stamp)
                    .provideSomeLayer[Resolve](
                      Publisher.local(maven) ++ ZLayer.succeed(ContentStore.in(store))
                    )
                  pluginLine     <- lineOf(found, ProjectName("plugins"))
                  siblingLine    <- lineOf(found, ProjectName("sibling"))
                  pluginVersion  <- zipTextOf(pluginLine.jar.toList, ApiDigest.resource)
                  pluginSources  <- zipTextOf(pluginLine.sources.toList, ApiDigest.resource)
                  siblingVersion <- zipTextOf(siblingLine.jar.toList, ApiDigest.resource)
                  siblingSources <- zipTextOf(siblingLine.sources.toList, ApiDigest.resource)
                yield assertTrue(
                  found.skipped.isEmpty,
                  found.held.isEmpty,
                  found.refused.isEmpty,
                  pluginVersion.contains(s"${stamp.value}\n"),
                  pluginSources.isEmpty,
                  siblingVersion.isEmpty,
                  siblingSources.isEmpty,
                )
              }
            }
          }
        }
      },
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def coordinate(sha: histos.version.CommitSha): String =
    s"1.2.3-${sha.abbreviate.value}-SNAPSHOT"

  /** Captured before the workspace scope closes. `assertTrue` runs its arrow after that scope releases the directory.
    */
  private final case class OnDisk(underMaven: Boolean, zip: Boolean, sha256: Boolean)

  private def onDisk(maven: Path, paths: List[Path]): UIO[List[OnDisk]] =
    ZIO.attemptBlocking(paths.map(path => readOne(maven, path))).orDie

  private def readOne(maven: Path, path: Path): OnDisk =
    val bytes  = Files.readAllBytes(path)
    val zip    = bytes.length >= 2 && bytes(0) == 0x50.toByte && bytes(1) == 0x4b.toByte
    val name   = Option(path.getFileName).map(_.toString).getOrElse("")
    val side   = path.resolveSibling(s"$name.sha256")
    val sha256 =
      if Files.isRegularFile(side) then
        val text = Files.readString(side)
        text.endsWith("\n") && text.trim.nonEmpty
      else false
    OnDisk(path.startsWith(maven), zip, sha256)

  private def pomOf(found: PublishOutput): UIO[String] =
    found.modules.flatMap(_.pom.toList) match
      case path :: Nil =>
        ZIO.attemptBlocking(Files.readString(path)).orDie
      case _ => ZIO.succeed("")

  private def zipTextOf(paths: List[Path], name: String): UIO[Option[String]] =
    paths match
      case path :: Nil =>
        ZIO.attemptBlocking {
          val zip = new java.util.zip.ZipFile(path.toFile)
          try
            Option(zip.getEntry(name)).map: entry =>
              val stream = zip.getInputStream(entry)
              try new String(stream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
              finally stream.close()
          finally zip.close()
        }.orDie
      case _ => ZIO.succeed(None)

  private def manifestOf(found: PublishOutput): UIO[String] =
    found.modules.flatMap(_.jar.toList) match
      case path :: Nil =>
        ZIO.attemptBlocking {
          val zip = new java.util.zip.ZipFile(path.toFile)
          try
            Option(zip.getEntry("META-INF/MANIFEST.MF")) match
              case Some(entry) =>
                val stream = zip.getInputStream(entry)
                try new String(stream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
                finally stream.close()
              case None => ""
          finally zip.close()
        }.orDie
      case _ => ZIO.succeed("")

  private def pomText(line: PublishedModule): UIO[String] =
    line.pom match
      case Some(path) => ZIO.attemptBlocking(Files.readString(path)).orDie
      case None       => ZIO.succeed("")

  private def lineOf(found: PublishOutput, name: ProjectName): UIO[PublishedModule] =
    found.modules.find(_.module == name) match
      case Some(line) => ZIO.succeed(line)
      case None       => ZIO.dieMessage(s"${name.value} was not published")

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
end LocalStageLawSpec
