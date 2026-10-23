package histos.load

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.plugin.*
import histos.resolve.*
import histos.schedule.{ActionCache, Hold, RemoteCache, Runner, Sendable, Verdict}
import histos.workflow.Workflow
import histos.{Dep, Module}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.durationInt
import zio.test.*

/** A temp-repo stand-in for a Specular plugin. Not the Specular repository, and not a root build plugin. */
object SpecularFixtureSpec extends ZIOSpecDefault:
  private val coordinate = Dep("dev.earlyeffect::specular:0.2.0")

  private val marker = "not-a-workflow: fixture"

  private val siteText =
    """package dev.earlyeffect.specular.text
      |
      |object SiteText:
      |  def render(root: String): String = s"site:$root"
      |""".stripMargin

  private val pluginSource =
    s"""package dev.earlyeffect.specular
       |
       |import dev.earlyeffect.specular.text.SiteText
       |import histos.ModuleDef
       |import histos.ProjectName
       |import histos.plugin.*
       |import zio.{Scope, ZIO}
       |
       |// $marker
       |object Specular:
       |  val plugin: HistosPlugin = SpecularPlugin
       |
       |  def site(module: ModuleDef): ModuleDef =
       |    module.note(plugin.id, NoteKey("role"), NoteText("site"))
       |
       |object SpecularPlugin extends HistosPlugin:
       |  val id: PluginId = PluginId("dev.earlyeffect.specular")
       |
       |  def contribute: Contribution =
       |    ProjectName.from("docs") match
       |      case Right(docs) => Contribution(
       |      tasks = List(DeclaredTask(
       |        name = TaskName("site"),
       |        seal = Seal.Open,
       |        needs = Set.empty[Need],
       |        inputs = List(
       |          DeclaredInput.Directory(RepoPath("docs")),
       |          DeclaredInput.Note(docs, NoteKey("role")),
       |        ),
       |        outputs = List(DeclaredOutput(OutputName("site"), RepoPath("docs/index.html"))),
       |        dependsOn = Nil,
       |      )),
       |      commands = List(CommandSpec(CommandName("site"), Nil, TaskName("site"), PluginDetail("write the site"))),
       |      jobs = List(CiJob(JobName("docs"), TaskName("site"), JobWhen.Always)),
       |    )
       |      case Left(_) => Contribution(Nil, Nil, Nil)
       |
       |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
       |    val text = SiteText.render("docs")
       |    ZIO.succeed(PluginOutput(text, List(WrittenFile(OutputName("site"), RepoPath("docs/index.html")))))
       |""".stripMargin

  private val buildFile =
    """plugin(dep"dev.earlyeffect::specular:0.2.0")
      |
      |import histos.*
      |import dev.earlyeffect.specular.Specular
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val docs =
      |      module("docs", Jvm)(scala"3.9.0")
      |        .pipe(Specular.site)
      |    Projects(use(Specular.plugin), docs)
      |""".stripMargin

  def spec =
    suite("specular fixture")(
      test("both jars load, SiteText runs, and the open site job stays local"):
        ZIO.scoped:
          for
            api <- apiJar
            hex <- apiHex(api)
            work <- ZIO.attempt(Files.createTempDirectory("histos-specular")).mapError(_ => LoadError.Io(LoadIo.Temp))
            _ <- ZIO.addFinalizer(deleteTree(work))
            maven <- publish(work, api, hex)
            project <- ZIO.attempt {
                         val dir = work.resolve("project")
                         Files.createDirectories(dir.resolve(".histos"))
                         Files.createDirectories(dir.resolve("docs"))
                         Files.createDirectories(work.resolve("cache"))
                         Files.createDirectories(work.resolve("store"))
                         Files.writeString(dir.resolve("histos.scala"), buildFile)
                         Files.writeString(dir.resolve("docs").resolve("index.html"), "placeholder\n")
                         dir
                       }.mapError(_ => LoadError.Io(LoadIo.Temp))
            _ <- Resolve.updatePlugins(
                   project,
                   List(PluginRequest.Dep(coordinate, LinkStrategy.BinaryOrSource)),
                   Repositories(Repository.Maven(RepoUrl.file(maven)), Nil),
                 ).provide(Resolve.live(work.resolve("cache"))).mapError(LoadError.Resolve(_))
            loaded <- ZIO.serviceWithZIO[Loader](_.load(project, api)).provideSome[Loader & Scope](
                        Resolve.live(work.resolve("cache")),
                        ZLayer.succeed(ContentStore.in(work.resolve("store"))),
                      )
            id = PluginTaskId(PluginId("dev.earlyeffect.specular"), TaskName("site"))
            ran <- loaded.plugins.run(id, PluginArgs.empty).provide(ZLayer.succeed(ActionCache.miss), RemoteCache.off, IdleResolve.layer)
            lockText <- ZIO.attempt(Files.readString(Resolve.lockPath(project))).mapError(_ => LoadError.Io(LoadIo.Stat))
          yield
            val linked = loaded.plugins.links match
              case PluginLink(pluginId, PluginLinked.FromBinary(digest), artifacts) :: Nil =>
                s"${pluginId.value} binary ${digest.value} ${artifacts.map(fileName).sorted.mkString(" ")}"
              case other => other.mkString(" | ")
            val stayed = loaded.graph.plugins match
              case plugin :: Nil =>
                plugin.contribution.tasks match
                  case task :: Nil => Sendable.decide(Runner.Plugin(task.seal), task.needs) == Verdict.Stay(Hold.Plugin)
                  case _           => false
              case _ => false
            val rendered = Workflow.render(loaded.graph) match
              case Right(text) =>
                text.contains("./histos run dev.earlyeffect.specular/site") && !text.contains(marker)
              case Left(_) => false
            val locked = DependencyLock.parse(lockText) match
              case Right(lock) =>
                lock.plugins.map: plugin =>
                  val names = plugin.artifacts.map(_.name).sorted.mkString(" ")
                  val shas = plugin.artifacts.map(_.sha256.value).distinct.length
                  s"${plugin.organization} ${plugin.name} ${plugin.revision} ${plugin.link} $names shas=$shas source=${plugin.source.isDefined}"
                .mkString(";")
              case Left(err) => err.message
            val contributed = loaded.graph.plugins match
              case plugin :: Nil =>
                plugin.entry.value == "dev.earlyeffect.specular.SpecularPlugin$" &&
                  (plugin.contribution.commands match
                    case CommandSpec(name, params, task, summary) :: Nil =>
                      name == CommandName("site") &&
                        params.isEmpty &&
                        task == TaskName("site") &&
                        summary.value == "write the site"
                    case _ => false) &&
                  (plugin.contribution.tasks match
                    case task :: Nil =>
                      task.name == TaskName("site") &&
                        task.outputs == List(DeclaredOutput(OutputName("site"), RepoPath("docs/index.html")))
                    case _ => false) &&
                  (plugin.contribution.jobs match
                    case CiJob(name, task, JobWhen.Always, _) :: Nil =>
                      name == JobName("docs") && task == TaskName("site")
                    case _ => false)
              case _ => false
            val noted = loaded.graph.projects.collect { case module: Module => module }.find(_.name.value == "docs") match
              case Some(module) =>
                module.notes == List(ModuleNote(PluginId("dev.earlyeffect.specular"), NoteKey("role"), NoteText("site")))
              case None => false
            assertTrue(
              linked == s"dev.earlyeffect.specular binary $hex specular-text_3-0.2.0.jar specular_3-0.2.0.jar",
              ran == PluginOutput("site:docs", List(WrittenFile(OutputName("site"), RepoPath("docs/index.html")))),
              stayed,
              rendered,
              locked == "dev.earlyeffect specular 0.2.0 BinaryOrSource specular-text_3 specular_3 shas=2 source=true",
              contributed,
              noted,
            )
    ).provideShared(Loader.live) @@ TestAspect.timeout(3.minutes)

  private def publish(work: Path, api: Path, hex: String): IO[LoadError, Path] =
    val textClasses = work.resolve("text-classes")
    val pluginClasses = work.resolve("plugin-classes")
    val maven = work.resolve("maven")
    for
      boundary <- Boundary.jars(api)
      _ <- ZIO.attempt {
             Files.createDirectories(textClasses)
             Files.createDirectories(pluginClasses)
             Files.writeString(work.resolve("SiteText.scala"), siteText)
             Files.writeString(work.resolve("Specular.scala"), pluginSource)
             ()
           }.mapError(_ => LoadError.Io(LoadIo.Temp))
      _ <- compile(List(work.resolve("SiteText.scala")), boundary, textClasses)
      textPacked <- pack(work.resolve("specular-text.jar"), Nil, Some(textClasses))
      _ <- compile(List(work.resolve("Specular.scala")), textPacked :: boundary, pluginClasses)
      pluginPacked <- pack(
                        work.resolve("specular.jar"),
                        List(
                          HistosPlugin.entryResource -> "dev.earlyeffect.specular.SpecularPlugin$\n",
                          ApiDigest.resource -> s"$hex\n",
                        ),
                        Some(pluginClasses),
                      )
      _ <- ZIO.attempt {
             val textBytes = Files.readAllBytes(textPacked)
             val pluginBytes = Files.readAllBytes(pluginPacked)
             writeMaven(
               maven,
               "dev.earlyeffect",
               "specular-text_3",
               "0.2.0",
               textBytes,
               pom("dev.earlyeffect", "specular-text_3", "0.2.0", None),
             )
             writeMaven(
               maven,
               "dev.earlyeffect",
               "specular_3",
               "0.2.0",
               pluginBytes,
               pom("dev.earlyeffect", "specular_3", "0.2.0", Some(("dev.earlyeffect", "specular-text_3", "0.2.0"))),
             )
             val dir = maven.resolve("dev").resolve("earlyeffect").resolve("specular_3").resolve("0.2.0")
             val sourcesName = "specular_3-0.2.0-sources.jar"
             val sources = emptyJar()
             Files.write(dir.resolve(sourcesName), sources)
             Files.writeString(dir.resolve(s"$sourcesName.sha1"), sha1(sources) + "\n")
             ()
           }.mapError(_ => LoadError.Io(LoadIo.Temp))
    yield maven

  private def compile(sources: List[Path], classpath: List[Path], out: Path): IO[LoadError, Unit] =
    ZIO.attempt {
      val reporter = new StoreReporter(null)
      val args = Array(
        "-classpath",
        classpath.map(_.toString).mkString(java.io.File.pathSeparator),
        "-d",
        out.toString,
        "-color:never",
      ) ++ sources.map(_.toString)
      new Driver().process(args, reporter)
      if reporter.hasErrors then Some(reporter.allErrors.map(_.message).mkString("\n")) else None
    }.mapError(_ => LoadError.Io(LoadIo.Compile)).flatMap:
      case Some(text) => ZIO.fail(LoadError.CompileFailed(List(CompilerMessage.fromCompiler(text))))
      case None       => ZIO.unit

  private def pack(path: Path, entries: List[(String, String)], classes: Option[Path]): IO[LoadError, Path] =
    ZIO.attempt {
      val output = new JarOutputStream(Files.newOutputStream(path))
      try
        classes.foreach(root => writeTree(output, root))
        entries.foreach: (name, text) =>
          output.putNextEntry(new JarEntry(name))
          output.write(text.getBytes(StandardCharsets.UTF_8))
          output.closeEntry()
      finally output.close()
      path
    }.mapError(_ => LoadError.Io(LoadIo.Temp))

  private def writeTree(output: JarOutputStream, root: Path): Unit =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.foreach: path =>
        if Files.isRegularFile(path) then
          val name = root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
          output.putNextEntry(new JarEntry(name))
          output.write(Files.readAllBytes(path))
          output.closeEntry()
    finally walk.close()

  private def writeMaven(root: Path, group: String, name: String, revision: String, jar: Array[Byte], pomText: String): Unit =
    val dir = (group.split("\\.").toList :+ name :+ revision).foldLeft(root)(_.resolve(_))
    Files.createDirectories(dir)
    val jarName = s"$name-$revision.jar"
    val pomName = s"$name-$revision.pom"
    val pomBytes = pomText.getBytes(StandardCharsets.UTF_8)
    Files.write(dir.resolve(jarName), jar)
    Files.write(dir.resolve(pomName), pomBytes)
    Files.writeString(dir.resolve(s"$jarName.sha1"), sha1(jar) + "\n")
    Files.writeString(dir.resolve(s"$pomName.sha1"), sha1(pomBytes) + "\n")
    ()

  private def pom(group: String, name: String, revision: String, dependency: Option[(String, String, String)]): String =
    val deps = dependency match
      case None => ""
      case Some((depGroup, artifact, version)) =>
        s"""  <dependencies>
           |    <dependency>
           |      <groupId>$depGroup</groupId>
           |      <artifactId>$artifact</artifactId>
           |      <version>$version</version>
           |    </dependency>
           |  </dependencies>
           |""".stripMargin
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project>
       |  <modelVersion>4.0.0</modelVersion>
       |  <groupId>$group</groupId>
       |  <artifactId>$name</artifactId>
       |  <version>$revision</version>
       |$deps</project>
       |""".stripMargin

  private def emptyJar(): Array[Byte] =
    val buffer = new ByteArrayOutputStream
    val output = new JarOutputStream(buffer)
    output.close()
    buffer.toByteArray

  private def sha1(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-1").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  private def fileName(path: String): String =
    Option(Path.of(path).getFileName).map(_.toString).getOrElse(path)

  private val apiJar: IO[LoadError, Path] =
    ApiJar.locate

  private def apiHex(jar: Path): IO[LoadError, String] =
    ZIO.attempt(Sha256.of(Chunk.fromArray(Files.readAllBytes(jar))).value).mapError(_ => LoadError.Io(LoadIo.Stat))

  private def deleteTree(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore
