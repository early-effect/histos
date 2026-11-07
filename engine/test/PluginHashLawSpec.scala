package histos.load

import histos.jdk.Sha256
import histos.plugin.{
  DeclaredInput,
  DeclaredTask,
  EnvName,
  InputKey,
  MaterialInput,
  ModuleNote,
  NoteKey,
  NoteText,
  PluginDetail,
  PluginId,
  PluginRunError,
  PluginTaskId,
  RepoPath,
  Seal,
  TaskName,
}
import histos.schedule.{Input, InputValue}
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, ZIO}
import zio.test.*

object PluginHashLawSpec extends ZIOSpecDefault:
  private val plugin = PluginId("demo")
  private val core   = ProjectName.trusted("core")
  private val v3     = ScalaVersion.trusted("3.10.0")
  private val key    = NoteKey("owner")

  def spec = suite("plugin input hash laws")(
    test("a file, a directory, a text, an env, and a note are those inputs, and a symlink is missing"):
      Workspace
        .work("histos-plugin-hash")
        .flatMap: root =>
          val file   = root.resolve("notes").resolve("readme.txt")
          val link   = root.resolve("notes").resolve("link.txt")
          val dir    = root.resolve("data")
          val first  = dir.resolve("a.txt")
          val second = dir.resolve("b.txt")
          val alias  = dir.resolve("alias.txt")
          val outside = root.resolve("outside.txt")
          val module = noted
          val graph  = ProjectGraph.trusted(List(module))
          val asked  = task(
            List(
              DeclaredInput.File(RepoPath("notes/readme.txt")),
              DeclaredInput.Directory(RepoPath("data")),
              DeclaredInput.Text(InputKey("label"), "hello"),
              DeclaredInput.Env(EnvName("HOME_LAW")),
              DeclaredInput.Note(core, key),
            )
          )
          for
            _       <- write(file, "one")
            _       <- write(first, "a")
            _       <- write(second, "b")
            _       <- write(outside, "hidden")
            _       <- linkTo(link, file)
            _       <- linkTo(alias, outside)
            hashed  <- PluginInputs.hash(request(root, graph, asked, Nil, Map("HOME_LAW" -> "desk")))
            changed <-
              write(file, "two") *> PluginInputs.hash(request(root, graph, asked, Nil, Map("HOME_LAW" -> "desk")))
            unlinked <-
              ZIO.attemptBlocking(Files.delete(alias)).catchAll(error => ZIO.dieMessage(error.getClass.getName)) *>
                PluginInputs.hash(request(root, graph, asked, Nil, Map("HOME_LAW" -> "desk")))
            unset <- PluginInputs.hash(request(root, graph, asked, Nil, Map.empty)).either
            gone  <- PluginInputs
              .hash(request(root, graph, task(List(DeclaredInput.File(RepoPath("notes/link.txt")))), Nil, Map.empty))
              .either
            note <- PluginInputs
              .hash(request(root, graph, task(List(DeclaredInput.Note(core, NoteKey("other")))), Nil, Map.empty))
              .either
            lost <- PluginInputs
              .hash(
                request(
                  root,
                  ProjectGraph.trusted(Nil),
                  task(List(DeclaredInput.Note(core, key))),
                  Nil,
                  Map.empty,
                )
              )
              .either
          yield
            val one    = Sha256.of(Chunk.fromArray("one".getBytes(StandardCharsets.UTF_8)))
            val envSha = Sha256.of(Chunk.fromArray("desk".getBytes(StandardCharsets.UTF_8)))
            val empty  = Sha256.of(Chunk.fromArray(Array.emptyByteArray))
            assertTrue(
              value(hashed, "in:file:notes/readme.txt") == Some(InputValue.Hash(one)),
              value(hashed, "in:text:label") == Some(InputValue.Text("hello")),
              value(hashed, "in:env:HOME_LAW") == Some(InputValue.Hash(envSha)),
              value(hashed, "in:note:core:owner") == Some(InputValue.Text("kept")),
              value(hashed, "plugin.task") == Some(InputValue.Text("demo/pack")),
              value(hashed, "in:dir:data") == value(unlinked, "in:dir:data"),
              value(hashed, "in:dir:data") != Some(InputValue.Hash(empty)),
              value(changed, "in:file:notes/readme.txt") != value(hashed, "in:file:notes/readme.txt"),
              value(changed, "in:text:label") == value(hashed, "in:text:label"),
              hashed.material == List(
                MaterialInput.File(RepoPath("notes/readme.txt"), content(one)),
                MaterialInput.Directory(RepoPath("data"), content(fileShaOf(hashed, "in:dir:data"))),
                MaterialInput.Text(InputKey("label"), "hello"),
                MaterialInput.Env(EnvName("HOME_LAW"), "desk"),
                MaterialInput.Note(core, key, NoteText.trusted("kept")),
              ),
              unset == Left(failed("env HOME_LAW is not set")),
              gone == Left(failed("declared input notes/link.txt is missing")),
              note == Left(PluginRunError.MissingNote(core, NoteKey("other"))),
              lost == Left(PluginRunError.MissingNote(core, key)),
            )
    ,
    test("equal jar bytes are one input, different bytes are two, and a missing jar is missing"):
      Workspace
        .work("histos-plugin-jars")
        .flatMap: root =>
          val left  = root.resolve("left.jar")
          val right = root.resolve("right.jar")
          val extra = root.resolve("extra.jar")
          val tree  = root.resolve("classes")
          val graph = ProjectGraph.trusted(Nil)
          val asked = task(Nil)
          for
            _       <- writeBytes(left, Array(1, 2))
            _       <- writeBytes(right, Array(1, 2))
            _       <- writeBytes(extra, Array(9))
            _       <- writeBytes(tree.resolve("A.class"), Array(4))
            same    <- PluginInputs.hash(request(root, graph, asked, List(left.toString, right.toString), Map.empty))
            differ  <- PluginInputs.hash(request(root, graph, asked, List(left.toString, extra.toString), Map.empty))
            folder  <- PluginInputs.hash(request(root, graph, asked, List(tree.toString), Map.empty))
            missing <- PluginInputs.hash(request(root, graph, asked, List(root.resolve("nope.jar").toString), Map.empty)).either
          yield
            val jars = same.inputs.map(_.name.value).filter(_.startsWith("plugin.jar:"))
            val two  = differ.inputs.map(_.name.value).filter(_.startsWith("plugin.jar:"))
            val dir  = folder.inputs.map(_.name.value).filter(_.startsWith("plugin.jar:"))
            assertTrue(
              jars.length == 1,
              two.length == 2,
              dir.length == 1,
              value(same, "plugin.api").isDefined,
              missing == Left(failed(s"declared input ${root.resolve("nope.jar")} is missing")),
            )
    ,
    test("a module input names the class tree, and a missing module is a failure"):
      Workspace
        .work("histos-plugin-module")
        .flatMap: root =>
          val graph   = ProjectGraph.trusted(List(noted))
          val classes = GraphCompile.classesDir(root, core, v3, Platform.Jvm)
          val asked   = task(List(DeclaredInput.Module(core, Some(v3), Some(Platform.Jvm))))
          val label   = "in:module:core:3.10.0:jvm"
          for
            before <- PluginInputs.hash(request(root, graph, asked, Nil, Map.empty))
            _      <- writeBytes(classes.resolve("A.class"), Array(1))
            after  <- PluginInputs.hash(request(root, graph, asked, Nil, Map.empty))
            _      <- linkTo(classes.resolve("link.class"), classes.resolve("A.class"))
            linked <- PluginInputs.hash(request(root, graph, asked, Nil, Map.empty))
            lost   <- PluginInputs
              .hash(
                request(
                  root,
                  graph,
                  task(List(DeclaredInput.Module(ProjectName.trusted("missing"), None, None))),
                  Nil,
                  Map.empty,
                )
              )
              .either
          yield assertTrue(
            value(before, label) != value(after, label),
            value(linked, label) == value(after, label),
            lost == Left(failed("module missing is not in the graph")),
          )
    ,
  )

  private val noted: Module =
    Module(
      core,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(v3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      notes = List(ModuleNote(plugin, key, NoteText.trusted("kept"))),
    )

  private def task(inputs: List[DeclaredInput]): DeclaredTask =
    DeclaredTask(TaskName("pack"), Seal.Hermetic, Set.empty, inputs, Nil, Nil)

  private def request(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask,
      jars: List[String],
      env: Map[String, String],
  ): PluginInputs.HashRequest =
    PluginInputs.HashRequest(
      repo,
      graph,
      plugin,
      task,
      PluginTaskId(plugin, TaskName("pack")),
      jars,
      histos.plugin.ApiDigest.trusted("ab" * 32),
      env,
    )

  private def value(hashed: PluginInputs.Hashed, label: String): Option[InputValue] =
    hashed.inputs.collectFirst:
      case Input(name, found) if name.value == label => found

  private def fileShaOf(hashed: PluginInputs.Hashed, label: String): Sha256 =
    value(hashed, label) match
      case Some(InputValue.Hash(sha)) => sha
      case _                          => Sha256.of(Chunk.empty)

  private def content(sha: Sha256): histos.plugin.ContentHash =
    histos.plugin.ContentHash.from(sha.value) match
      case Right(hash) => hash
      case Left(_)     => histos.plugin.ContentHash.trusted(sha.value)

  private def failed(message: String): PluginRunError =
    PluginRunError.Failed(PluginDetail.trusted(message))

  private def write(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    writeBytes(path, text.getBytes(StandardCharsets.UTF_8))

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
        ()
      }
      .catchAll(error => ZIO.dieMessage(error.getClass.getName))

  private def linkTo(link: Path, target: Path): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        Files.createSymbolicLink(link, target)
        ()
      }
      .catchAll(error => ZIO.dieMessage(error.getClass.getName))
