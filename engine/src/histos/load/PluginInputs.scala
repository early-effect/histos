package histos.load

import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{
  ApiDigest,
  ArtifactFormat,
  ContentHash,
  DeclaredInput,
  DeclaredTask,
  MaterialInput,
  PluginArgs,
  PluginDetail,
  PluginId,
  PluginLinked,
  PluginOrigin,
  PluginRunError,
  PluginTaskId,
  RepoPath,
  SourceSet,
  VerifiedPluginManifest,
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
      args: PluginArgs,
      binding: Option[PluginLinked],
      provenance: List[ProvenanceResult],
      locations: List[Path],
      dependencyInputs: Option[List[Input]] = None,
  )

  def hash(request: HashRequest): IO[PluginRunError, Hashed] =
    val HashRequest(
      repo,
      graph,
      plugin,
      task,
      id,
      jars,
      launcher,
      env,
      args,
      binding,
      provenance,
      locations,
      dependencyInputs,
    ) =
      request
    for
      declared <- ZIO.foreach(task.inputs)(one(repo, graph, plugin, env, _))
      deps     <- dependencyInputs.fold(compileInputs(repo, graph, task))(ZIO.succeed(_))
      jarsIn   <- ZIO.foreach(jars.sorted)(jarInput)
      places   <- ZIO.foreach(locations.distinct.sortBy(_.toString))(path => jarInput(path.toString))
      closed   <- provenanceInputs(provenance)
      argsIn   <- named("plugin.args", InputValue.Text(argsLine(args)))
      bound    <- named("plugin.binding", InputValue.Text(bindingLine(binding)))
      api      <- named("plugin.api", InputValue.Hash(digest(launcher.value)))
      taskIn   <- named("plugin.task", InputValue.Text(id.render))
      outputs  <- named("plugin.outputs.v2", InputValue.Text(outputIdentity(task)))
      emitted  <- named("plugin.emissions", InputValue.Text(emissionIdentity(task)))
      bytes = (jarsIn ::: places).distinctBy(_.name.value)
    yield Hashed(
      declared.map(_._1) ::: deps ::: bytes ::: closed ::: List(argsIn, bound, api, taskIn, outputs, emitted),
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
              Sha256Digest.of(Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8)))
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

  /** Completed stage products and effective link modes contribute to the plugin cache key.
    */
  def compileInputs(
      repo: Path,
      graph: ProjectGraph,
      task: DeclaredTask,
  ): IO[PluginRunError, List[Input]] =
    for
      compiled <- ZIO.fromEither(PluginCells.selected(graph, task))
      linked   <- ZIO.fromEither(PluginCells.links(graph, task))
      stages   <- ZIO.foreach(compiled): cell =>
        val name  = cell.module.name
        val label = s"${name.value}:${cell.version.value}:${cell.platform.token}:${PluginCells.scopeToken(cell.scope)}"
        for
          main   <- dirHash(GraphCompile.classesDir(repo, name, cell.version, cell.platform), label)
          hashes <- cell.scope match
            case SourceSet.Main => ZIO.succeed(List(main))
            case SourceSet.Test =>
              dirHash(histos.testrun.TestCompile.testClasses(repo, name, cell.version, cell.platform), label)
                .map(test => List(main, test))
          sha = Sha256Digest.of(Chunk.fromArray(hashes.map(_.value).mkString("\n").getBytes(StandardCharsets.UTF_8)))
          input <- named(s"in:dep:$label", InputValue.Hash(sha))
        yield input
      scripts <- ZIO.foreach(linked.distinct): request =>
        val PluginCells.Link(cell, optimize) = request
        PluginCells
          .scriptDigest(repo, request)
          .flatMap: sha =>
            named(s"in:link:${cell.module.name.value}:${cell.version.value}:${optimize.token}", InputValue.Hash(sha))
    yield stages ++ scripts

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
          Sha256Digest.of(
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
              Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(file)))
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
          val sha = Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path))).value
          s"$rel $sha"
        Sha256Digest.of(
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
              Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(file)))
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
        Sha256Digest.of(Chunk.fromArray(hex.getBytes(StandardCharsets.UTF_8)))

  private def content(sha: Sha256): ContentHash =
    ContentHash.from(sha.value) match
      case Right(hash) => hash
      case Left(_)     => ContentHash.trusted(sha.value)

  private def star(version: Option[ScalaVersion]): String =
    version.fold("*")(_.value)

  private def starPlatform(platform: Option[Platform]): String =
    platform.fold("*")(_.token)

  /** Unverified is a refusal. A verified empty artifact list still names its activation, so it is not `absent`. */
  private def provenanceInputs(results: List[ProvenanceResult]): IO[PluginRunError, List[Input]] =
    results match
      case Nil =>
        named("plugin.provenance", InputValue.Text("absent")).map(List(_))
      case some =>
        ZIO.foreach(some.zipWithIndex): (result, index) =>
          result match
            case ProvenanceResult.Unverified(source) =>
              ZIO.fail(PluginRunError.Unverified(source))
            case ProvenanceResult.Verified(manifest) =>
              val sha = Sha256Digest.of(Chunk.fromArray(closureLine(manifest).getBytes(StandardCharsets.UTF_8)))
              named(s"plugin.closure.$index", InputValue.Hash(sha))

  private def closureLine(manifest: VerifiedPluginManifest): String =
    val activation = manifest.activation
    val origin     = activation.origin match
      case PluginOrigin.Source(id, digest)            => s"source ${id.value} ${digest.value}"
      case PluginOrigin.Published(coordinate, digest) => s"published ${coordinate.render} ${digest.value}"
    val closure = activation.closure.map(identityLine).mkString(",")
    val entries = manifest.artifacts
      .map: artifact =>
        val body = artifact.entries
          .map(entry => s"${entry.path.value} ${entry.length} ${entry.digest.value}")
          .mkString(",")
        s"${identityLine(artifact.identity)} $body"
      .mkString(";")
    List(
      s"source ${activation.source.value}",
      s"service ${activation.activation.value}",
      s"origin $origin",
      s"defining ${identityLine(activation.defining)}",
      s"closure $closure",
      s"host ${activation.host.value}",
      s"derived ${activation.derived.value}",
      s"artifacts $entries",
    ).mkString("\n")

  private def identityLine(identity: histos.plugin.ArtifactIdentity): String =
    val format = identity.format match
      case ArtifactFormat.Jar       => "jar"
      case ArtifactFormat.Directory => "directory"
    s"$format ${identity.digest.value}"

  private def framed(values: List[String]): String = values.map(value => s"${value.length}:$value").mkString

  private def outputIdentity(task: DeclaredTask): String =
    framed(
      task.outputs
        .sortBy(_.name.value)
        .flatMap(output => List(output.name.value, output.path.value, output.kind.toString))
    )

  private def emissionIdentity(task: DeclaredTask): String =
    import histos.plugin.{EmittedContent, TaskDep}
    val entries = task.dependsOn.flatMap:
      case TaskDep.Compile(_, _, _) | TaskDep.CompileScoped(_, _, _, _) | TaskDep.Link(_, _, _) | TaskDep.Same(_) |
          TaskDep.Generate(_) =>
        Nil
      case TaskDep.Emit(module) => List(framed(List(module.value, "main", "all", "source-tree")))
      case TaskDep.EmitScoped(module, scope, platforms, content, output) =>
        val selected = platforms match
          case histos.PlatformSelection.All          => "all"
          case histos.PlatformSelection.Only(values) => framed(values.all.map(_.token).distinct.sorted)
        val contents = content match
          case EmittedContent.SourceFile(entry)  => List("source-file", entry.value)
          case EmittedContent.SourceTree         => List("source-tree")
          case EmittedContent.ResourceFile(name) => List("resource-file", name.value)
          case EmittedContent.ResourceTree       => List("resource-tree")
        List(framed(List(module.value, PluginCells.scopeToken(scope), selected, output.value) ++ contents))
    framed(entries.distinct.sorted)

  private def argsLine(args: PluginArgs): String =
    val texts = args.texts.map(item => s"${item.name.value}=${item.value}").sorted.mkString("\n")
    val flags = args.flags.toList.map(_.value).sorted.mkString("\n")
    s"texts\n$texts\nflags\n$flags"

  private def bindingLine(binding: Option[PluginLinked]): String =
    binding match
      case None                                              => "unlinked"
      case Some(PluginLinked.FromBinary(api))                => s"binary ${api.value}"
      case Some(PluginLinked.FromSource(published, running)) =>
        s"source ${published.value} ${running.value}"

  private def missing(path: String): PluginRunError =
    PluginRunError.Failed(
      PluginDetail.trusted(s"declared input $path is missing")
    )
end PluginInputs
