package histos.load

import histos.jdk.Sha256
import histos.plugin.{
  ApiDigest,
  ContentHash,
  DeclaredInput,
  DeclaredTask,
  MaterialInput,
  PluginDetail,
  PluginId,
  PluginRunError,
  PluginTaskId,
  RepoPath,
}
import histos.schedule.{Input, InputName, InputValue}
import histos.zinc.GraphCompile
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

/** Declared inputs plus the jars, the api digest, and the task id. The request sees the same hashes.
  */
private[load] object PluginInputs:
  final case class Hashed(inputs: List[Input], material: List[MaterialInput])

  /** One hash. `env` is the map `Machine.load` already read. */
  final case class HashRequest(
      repo: Path,
      graph: ProjectGraph,
      plugin: PluginId,
      task: DeclaredTask,
      id: PluginTaskId,
      jars: List[String],
      launcher: ApiDigest,
      env: Map[String, String],
  )

  def hash(request: HashRequest): IO[PluginRunError, Hashed] =
    val HashRequest(repo, graph, plugin, task, id, jars, launcher, env) =
      request
    for
      declared <- ZIO.foreach(task.inputs)(one(repo, graph, plugin, env, _))
      deps     <- compileInputs(repo, graph, task)
      jarsIn   <- ZIO.foreach(jars.sorted)(jarInput).map(_.distinct)
      api      <- named("plugin.api", InputValue.Hash(digest(launcher.value)))
      taskIn   <- named("plugin.task", InputValue.Text(id.render))
    yield Hashed(
      declared.map(_._1) ::: deps ::: jarsIn ::: List(api, taskIn),
      declared.map(_._2),
    )

  private def one(
      repo: Path,
      graph: ProjectGraph,
      plugin: PluginId,
      env: Map[String, String],
      input: DeclaredInput,
  ): IO[PluginRunError, (Input, MaterialInput)] =
    input match
      case DeclaredInput.File(path) =>
        fileHash(repo, path).flatMap: sha =>
          pair(
            s"in:file:${path.value}",
            InputValue.Hash(sha),
            MaterialInput.File(path, content(sha)),
          )
      case DeclaredInput.Directory(path) =>
        val root = repo.resolve(path.value)
        ZIO
          .attemptBlocking(Files.isDirectory(root))
          .orElseFail(missing(path.value))
          .flatMap:
            case false => ZIO.fail(missing(path.value))
            case true  =>
              dirHash(root, path.value).flatMap: sha =>
                pair(
                  s"in:dir:${path.value}",
                  InputValue.Hash(sha),
                  MaterialInput.Directory(path, content(sha)),
                )
      case DeclaredInput.Text(name, value) =>
        pair(
          s"in:text:${name.value}",
          InputValue.Text(value),
          MaterialInput.Text(name, value),
        )
      case DeclaredInput.Env(name) =>
        env.get(name.value) match
          case None =>
            ZIO.fail(
              PluginRunError.Failed(
                PluginDetail.trusted(s"env ${name.value} is not set")
              )
            )
          case Some(value) =>
            val sha =
              Sha256.of(Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8)))
            pair(
              s"in:env:${name.value}",
              InputValue.Hash(sha),
              MaterialInput.Env(name, value),
            )
      case DeclaredInput.Note(module, key) =>
        note(graph, plugin, module, key).flatMap: text =>
          pair(
            s"in:note:${module.value}:${key.value}",
            InputValue.Text(text.value),
            MaterialInput.Note(module, key, text),
          )
      case DeclaredInput.Module(name, version, platform) =>
        cells(repo, graph, name, version, platform).flatMap: sha =>
          pair(
            s"in:module:${name.value}:${star(version)}:${starPlatform(platform)}",
            InputValue.Hash(sha),
            MaterialInput.Module(name, content(sha)),
          )

  /** Class directories of `TaskDep.Compile`. The compiler has already written them. The hash is the plugin cache key.
    */
  def compileInputs(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask,
  ): IO[PluginRunError, List[Input]] =
    val chosen = ZIO.foreach(task.dependsOn):
      case histos.plugin.TaskDep.Same(_)                          => ZIO.none
      case histos.plugin.TaskDep.Compile(name, version, platform) =>
        cells(repo, graph, name, version, platform).flatMap: sha =>
          named(
            s"in:dep:${name.value}:${star(version)}:${starPlatform(platform)}",
            InputValue.Hash(sha),
          ).map(Some(_))
    chosen.map(_.flatten)

  private def note(
      graph: ProjectGraph,
      plugin: PluginId,
      name: ProjectName,
      key: histos.plugin.NoteKey,
  ): IO[PluginRunError, histos.plugin.NoteText] =
    module(graph, name) match
      case None        => ZIO.fail(PluginRunError.MissingNote(name, key))
      case Some(found) =>
        found.notes.find(item => item.plugin == plugin && item.key == key) match
          case Some(item) => ZIO.succeed(item.value)
          case None       => ZIO.fail(PluginRunError.MissingNote(name, key))

  private def cells(
      repo: Path,
      graph: ProjectGraph,
      name: ProjectName,
      version: Option[ScalaVersion],
      platform: Option[Platform],
  ): IO[PluginRunError, Sha256] =
    module(graph, name) match
      case None =>
        ZIO.fail(
          PluginRunError.Failed(
            PluginDetail.trusted(s"module ${name.value} is not in the graph")
          )
        )
      case Some(found) =>
        val versions  = version.fold(found.versions.all)(List(_))
        val platforms = platform.fold(found.platforms.all)(List(_))
        val chosen    = versions
          .flatMap(item => platforms.map(target => (item, target)))
          .sortBy((item, target) => (item.value, target.token))
        val lines = ZIO.foreach(chosen): (item, target) =>
          val dir = GraphCompile.classesDir(repo, name, item, target)
          dirHash(dir, s"${name.value}/${item.value}/${target.token}").map(sha =>
            s"${item.value} ${target.token} ${sha.value}"
          )
        lines.map: found =>
          Sha256.of(
            Chunk.fromArray(
              found.mkString("\n").getBytes(StandardCharsets.UTF_8)
            )
          )

  private def fileHash(repo: Path, path: RepoPath): IO[PluginRunError, Sha256] =
    val file = repo.resolve(path.value)
    ZIO
      .attemptBlocking(Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))
      .orElseFail(missing(path.value))
      .flatMap:
        case false => ZIO.fail(missing(path.value))
        case true  =>
          ZIO
            .attemptBlocking(
              Sha256.of(Chunk.fromArray(Files.readAllBytes(file)))
            )
            .orElseFail(missing(path.value))

  private def dirHash(root: Path, label: String): IO[PluginRunError, Sha256] =
    ZIO
      .attemptBlocking {
        val files =
          if !Files.isDirectory(root) then Nil
          else
            val walk = Files.walk(root)
            try
              walk.iterator.asScala
                .filter(path => Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS))
                .toList
                .sortBy(path => root.relativize(path).toString)
            finally walk.close()
        val lines = files.map: path =>
          val rel = root.relativize(path).toString.replace('\\', '/')
          val sha = Sha256.of(Chunk.fromArray(Files.readAllBytes(path))).value
          s"$rel $sha"
        Sha256.of(
          Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8))
        )
      }
      .orElseFail(
        PluginRunError.Failed(PluginDetail.trusted(s"could not read $label"))
      )

  private enum PluginFile:
    case Absent, Directory, Jar

  /** Named by content, so a path never enters the key and two jars that share a file name stay two inputs.
    */
  private def jarInput(path: String): IO[PluginRunError, Input] =
    val file = Path.of(path)
    ZIO
      .attemptBlocking {
        if Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS) then PluginFile.Directory
        else if Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) then PluginFile.Jar
        else PluginFile.Absent
      }
      .orElseFail(missing(path))
      .flatMap:
        case PluginFile.Absent    => ZIO.fail(missing(path))
        case PluginFile.Directory => dirHash(file, path).flatMap(byContent)
        case PluginFile.Jar       =>
          ZIO
            .attemptBlocking(
              Sha256.of(Chunk.fromArray(Files.readAllBytes(file)))
            )
            .orElseFail(missing(path))
            .flatMap(byContent)

  private def byContent(sha: Sha256): IO[PluginRunError, Input] =
    named(s"plugin.jar:${sha.value}", InputValue.Hash(sha))

  private def pair(
      name: String,
      value: InputValue,
      material: MaterialInput,
  ): IO[PluginRunError, (Input, MaterialInput)] =
    named(name, value).map(input => (input, material))

  private def named(
      name: String,
      value: InputValue,
  ): IO[PluginRunError, Input] =
    ZIO
      .fromEither(InputName.from(name))
      .mapBoth(_ => PluginRunError.Defect("InputName"), Input(_, value))

  private def module(graph: ProjectGraph, name: ProjectName): Option[Module] =
    graph.projects.collectFirst:
      case found: Module if found.name == name => found

  private def digest(hex: String): Sha256 =
    Sha256.from(hex) match
      case Right(sha) => sha
      case Left(_)    =>
        Sha256.of(Chunk.fromArray(hex.getBytes(StandardCharsets.UTF_8)))

  private def content(sha: Sha256): ContentHash =
    ContentHash.from(sha.value) match
      case Right(hash) => hash
      case Left(_)     => ContentHash.trusted(sha.value)

  private def star(version: Option[ScalaVersion]): String =
    version.fold("*")(_.value)

  private def starPlatform(platform: Option[Platform]): String =
    platform.fold("*")(_.token)

  private def missing(path: String): PluginRunError =
    PluginRunError.Failed(
      PluginDetail.trusted(s"declared input $path is missing")
    )
end PluginInputs
