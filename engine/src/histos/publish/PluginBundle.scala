package histos.publish

import histos.central.{
  Artifact,
  ArtifactId,
  DependencyVersion,
  MavenDependency,
  PublishError,
  Published,
  Publisher,
  Revision,
}
import histos.jdk.Sha256
import histos.pack.{JarError, PackageKey}
import histos.plugin.{
  ArtifactEntry,
  ArtifactFormat,
  ArtifactIdentity,
  ArtifactPath,
  ClosureArtifact,
  ClosureRole,
  Digests,
  InputRecord,
  RecipeError,
  RecipeFeature,
  RecipeInput,
  RecipeNode,
  RecipeNodeId,
  RecipeRole,
  RecipeVersion,
  RebuildRecipe,
  RepoPath,
  SealedArtifactManifest,
  SelectedInput,
  SemanticSetting,
  SourceKind,
  SourceProvenance,
  SourceSet,
  StageContract,
  StageContractError,
}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompilationCapture, CompilationSnapshot, CompileError, Sources}
import histos.{
  Dep,
  DepLink,
  MavenScope,
  Organization,
  Platform,
  PluginPublication,
  ScalaVersion,
  ScalacFlag,
  ScalacFlags,
}
import histos.load.KnownStageRegistry
import histos.plugin.StageAssurance
import histos.resolve.ArtifactName

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

/** One source the publication re-reads. The path is the snapshot path. */
final case class SourceBytes(path: RepoPath, bytes: Chunk[Byte])

/** One jar entry before the descriptor and rebuild payload are added. */
final case class EntryBytes(path: String, bytes: Chunk[Byte])

/** A jar that is ready to upload. `recipe` is absent for binary-only. */
final case class StagedPlugin(
    key: Sha256,
    bytes: Chunk[Byte],
    dependencies: List[MavenDependency],
    recipe: Option[RebuildRecipe],
)

/** What a plugin publication needs. The closure is the snapshot's closure, not a second classpath walk. */
final case class PluginPublishRequest(
    mode: PluginPublication,
    node: RecipeNodeId,
    capture: CompilationCapture,
    current: List[SourceBytes],
    products: List[EntryBytes],
    stamp: Option[EntryBytes],
    dependencies: List[MavenDependency],
    group: Organization,
    artifact: ArtifactId,
    revision: Revision,
)

enum PluginPublishFault:
  case Recipe(error: RecipeError)
  case Changed(path: RepoPath)
  case Seal(error: histos.plugin.ArtifactManifestError)
  case Jar(error: JarError)
  case Dependency(group: String, name: String, revision: String)
  case Sources(error: CompileError)
  case Io(failure: IoFailure)
  case Entry(name: String)
  case Publish(error: PublishError)

  def message: String = this match
    case Recipe(error)                     => error.message
    case Changed(path)                     => s"${path.value} changed after it was recorded"
    case Seal(error)                       => error.message
    case Jar(error)                        => error.message
    case Dependency(group, name, revision) => s"$group:$name:$revision is not a publishable dependency"
    case Sources(error)                    => error.message
    case Io(failure)                       => failure.message
    case Entry(name)                       => s"'$name' is not an artifact entry path"
    case Publish(error)                    => error.message
end PluginPublishFault

/** `plugin.json` and, for a rebuildable jar, `META-INF/histos/rebuild/...`. Validation finishes before any upload. */
object PluginBundle:
  val Descriptor: String = "META-INF/histos/plugin.json"

  def payload(node: RecipeNodeId, path: RepoPath): String =
    s"META-INF/histos/rebuild/${node.value}/${path.value}"

  /** Compile-only dependencies stay off this list. They belong to the snapshot closure, not the pom. */
  def dependencies(
      deps: List[Dep],
      version: ScalaVersion,
      platform: Platform,
  ): Either[PluginPublishFault, List[MavenDependency]] =
    Dep
      .declared(deps, platform)
      .foldLeft[Either[PluginPublishFault, List[MavenDependency]]](Right(Nil)):
        case (Left(error), _)  => Left(error)
        case (Right(acc), dep) =>
          maven(dep, version, platform).map(found => acc :+ found)

  def manifest(entries: List[EntryBytes]): Either[PluginPublishFault, SealedArtifactManifest] =
    seal(entries)

  def stage(request: PluginPublishRequest): Either[PluginPublishFault, StagedPlugin] =
    request.mode match
      case PluginPublication.BinaryOnly  => binary(request)
      case PluginPublication.Rebuildable => rebuildable(request)

  def publish(request: PluginPublishRequest): ZIO[Publisher, PluginPublishFault, Published] =
    ZIO
      .fromEither(stage(request))
      .flatMap: staged =>
        val artifact = Artifact(
          request.group,
          request.artifact,
          request.revision,
          staged.bytes,
          staged.dependencies,
        )
        ZIO.serviceWithZIO[Publisher](_.publish(artifact)).mapError(PluginPublishFault.Publish(_))

  /** Preferred main sources. Generated replacements use the unmanaged file's bytes. */
  def preferred(
      repo: Path,
      sourceRoot: Path,
      unmanagedRoot: Path,
      version: ScalaVersion,
      platform: Platform,
  ): IO[PluginPublishFault, List[SelectedInput]] =
    Sources
      .published(sourceRoot, unmanagedRoot, version, platform)
      .mapError(PluginPublishFault.Sources(_))
      .flatMap: files =>
        ZIO.foreach(files): file =>
          readFile(file.path).flatMap: bytes =>
            val relative = repo.relativize(file.path.toAbsolutePath.normalize).toString.replace('\\', '/')
            ZIO
              .fromEither(RepoPath.from(relative).left.map(_ => PluginPublishFault.Entry(relative)))
              .map: path =>
                val provenance =
                  if file.path.toAbsolutePath.normalize.startsWith(unmanagedRoot.toAbsolutePath.normalize) then
                    SourceProvenance.Generated
                  else SourceProvenance.Unmanaged
                SelectedInput(
                  path,
                  kindOf(file.entry),
                  SourceSet.Main,
                  provenance,
                  Digests.content(bytes),
                  bytes.length.toLong,
                )

  /** The jar for a module that opted into plugin publication. Unrecorded rebuildable fails before a jar exists. */
  def library(
      mode: PluginPublication,
      node: RecipeNodeId,
      capture: CompilationCapture,
      repo: Path,
      classes: Path,
      stamp: List[(String, Path)],
      dependencies: List[MavenDependency],
      group: Organization,
      artifact: ArtifactId,
      revision: Revision,
  ): IO[PluginPublishFault, StagedPlugin] =
    (mode, capture) match
      case (PluginPublication.Rebuildable, CompilationCapture.Unrecorded) =>
        ZIO.fail(PluginPublishFault.Recipe(RecipeError.Unrecorded(node)))
      case _ =>
        stagedLibrary(mode, node, capture, repo, classes, stamp, dependencies, group, artifact, revision)

  private def stagedLibrary(
      mode: PluginPublication,
      node: RecipeNodeId,
      capture: CompilationCapture,
      repo: Path,
      classes: Path,
      stamp: List[(String, Path)],
      dependencies: List[MavenDependency],
      group: Organization,
      artifact: ArtifactId,
      revision: Revision,
  ): IO[PluginPublishFault, StagedPlugin] =
    for
      products <- readTree(classes)
      extra    <- readNamed(stamp)
      current  <- currentOf(repo, capture)
      staged   <- ZIO.fromEither(
        stage(
          PluginPublishRequest(
            mode,
            node,
            capture,
            current,
            products ++ extra,
            None,
            dependencies,
            group,
            artifact,
            revision,
          )
        )
      )
    yield staged

  /** Stamp and class-tree identity for a plugin coordinate that is not a plugin publication. */
  def legacyKey(classes: Path, stamp: List[(String, Path)]): IO[PluginPublishFault, Sha256] =
    for
      products <- readTree(classes)
      extra    <- readNamed(stamp)
      sealedAt <- ZIO.fromEither(seal(products ++ extra))
    yield PackageKey.manifestOf(ArtifactFormat.Jar, sealedAt)

  def metadata(stamp: List[(String, Path)]): IO[PluginPublishFault, List[(String, Chunk[Byte])]] =
    readNamed(stamp).map(_.map(entry => entry.path -> entry.bytes))

  private def binary(request: PluginPublishRequest): Either[PluginPublishFault, StagedPlugin] =
    val recorded = request.capture match
      case CompilationCapture.Unrecorded         => None
      case CompilationCapture.Recorded(snapshot) => Some(snapshot)
    val json = descriptor(request.mode, request.node, request.dependencies, recorded)
    pack(request, json, Nil, None)

  private def rebuildable(request: PluginPublishRequest): Either[PluginPublishFault, StagedPlugin] =
    request.capture match
      case CompilationCapture.Unrecorded =>
        Left(PluginPublishFault.Recipe(RecipeError.Unrecorded(request.node)))
      case CompilationCapture.Recorded(snapshot) =>
        drift(snapshot, request.current) match
          case Some(path) => Left(PluginPublishFault.Changed(path))
          case None       =>
            recipeOf(request.node, snapshot).flatMap: recipe =>
              val payloads = (snapshot.sources ++ snapshot.resources).flatMap: input =>
                request.current
                  .find(_.path == input.path)
                  .map: found =>
                    EntryBytes(payload(request.node, input.path), found.bytes)
              val json = descriptor(request.mode, request.node, request.dependencies, Some(snapshot))
              pack(request, json, payloads, Some(recipe))

  private def pack(
      request: PluginPublishRequest,
      json: String,
      payloads: List[EntryBytes],
      recipe: Option[RebuildRecipe],
  ): Either[PluginPublishFault, StagedPlugin] =
    val descriptor = EntryBytes(Descriptor, Chunk.fromArray(json.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
    val entries    = request.products ++ request.stamp.toList ++ payloads :+ descriptor
    for
      _        <- distinct(entries.map(_.path))
      sealedAt <- seal(entries)
      zipped   <- jar(entries)
    yield StagedPlugin(
      PackageKey.sealedOf(ArtifactFormat.Jar, request.mode, sealedAt),
      zipped,
      request.dependencies,
      recipe,
    )

  private def recipeOf(node: RecipeNodeId, snapshot: CompilationSnapshot): Either[PluginPublishFault, RebuildRecipe] =
    recorded(snapshot).flatMap: body =>
      settings(node, body.settings).flatMap: _ =>
        stageNodes(snapshot).flatMap: stages =>
          val root = RecipeNode(node, RecipeRole.Root, stages.map(_.id), InputRecord.Frozen(body), None)
          RebuildRecipe
            .from(RecipeVersion.V1, node, stages :+ root)
            .left
            .map(PluginPublishFault.Recipe(_))

  private def recorded(snapshot: CompilationSnapshot): Either[PluginPublishFault, histos.plugin.RecordedInputs] =
    CompilationCapture.Recorded(snapshot).record match
      case InputRecord.Unrecorded =>
        Left(PluginPublishFault.Recipe(RecipeError.Unrecorded(RecipeNodeId.trusted("root"))))
      case InputRecord.Frozen(body) => Right(body)

  private def settings(node: RecipeNodeId, listed: List[SemanticSetting]): Either[PluginPublishFault, Unit] =
    listed.foldLeft[Either[PluginPublishFault, Unit]](Right(())):
      case (Left(error), _)    => Left(error)
      case (Right(_), setting) =>
        if !allowed(setting.key.value) then
          Left(PluginPublishFault.Recipe(RecipeError.Unsupported(RecipeFeature("flag"), node)))
        else if absolute(setting.value) then
          Left(PluginPublishFault.Recipe(RecipeError.Unsupported(RecipeFeature("path"), node)))
        else Right(())

  /** Flag tokens come from [[ScalacFlag]]. A new case needs no edit here. `encoding`, `release`, `target`, and `order`
    * are recipe settings, not compiler flags.
    */
  private val Allowed: Set[String] =
    ScalacFlag.values.iterator.map(flag => ScalacFlags.of(flag).canonical).toSet ++ Set(
      "encoding",
      "release",
      "target",
      "order",
    )

  private def allowed(key: String): Boolean =
    Allowed.contains(key)

  private def absolute(value: String): Boolean =
    value.startsWith("/") ||
      value.contains('\\') ||
      (value.length >= 2 && value.charAt(1) == ':' && value.charAt(0).isLetter)

  private def stageNodes(snapshot: CompilationSnapshot): Either[PluginPublishFault, List[RecipeNode]] =
    snapshot.stages.zipWithIndex.foldLeft[Either[PluginPublishFault, List[RecipeNode]]](Right(Nil)):
      case (Left(error), _)                => Left(error)
      case (Right(acc), (contract, index)) =>
        stageId(index).flatMap: id =>
          admit(id, contract).flatMap: valid =>
            recorded(snapshot).map: body =>
              val empty = body.copy(sources = Nil, settings = Nil, closure = Nil, stages = Nil, host = Nil)
              acc :+ RecipeNode(id, RecipeRole.Stage, valid.helpers, InputRecord.Frozen(empty), Some(valid))

  private def stageId(index: Int): Either[PluginPublishFault, RecipeNodeId] =
    RecipeNodeId.from(s"stage$index") match
      case Right(id) => Right(id)
      case Left(_)   =>
        Left(PluginPublishFault.Recipe(RecipeError.Incomplete(RecipeInput("stage"), RecipeNodeId.trusted("stage"))))

  private def admit(id: RecipeNodeId, contract: StageContract): Either[PluginPublishFault, StageContract] =
    if contract.entrypoints.isEmpty then
      Left(PluginPublishFault.Recipe(RecipeError.Incomplete(RecipeInput("stage"), id)))
    else
      contract.assurance match
        case StageAssurance.HostKnown =>
          contract.entrypoints.find(name => !KnownStageRegistry.names.contains(name)) match
            case Some(_) => Left(PluginPublishFault.Recipe(RecipeError.Unsupported(RecipeFeature("stage"), id)))
            case None    => checked(id, contract)
        case StageAssurance.ProducerDeclared => checked(id, contract)

  private def checked(id: RecipeNodeId, contract: StageContract): Either[PluginPublishFault, StageContract] =
    StageContract.validate(contract) match
      case Right(valid)                         => Right(valid)
      case Left(StageContractError.SecretInput) =>
        Left(PluginPublishFault.Recipe(RecipeError.Stage(StageContractError.SecretInput)))
      case Left(StageContractError.UnclassifiedInput) =>
        Left(PluginPublishFault.Recipe(RecipeError.Stage(StageContractError.UnclassifiedInput)))
      case Left(StageContractError.MissingBinding(_)) =>
        Left(PluginPublishFault.Recipe(RecipeError.Incomplete(RecipeInput("binding"), id)))
      case Left(StageContractError.BadJdk(_)) =>
        Left(PluginPublishFault.Recipe(RecipeError.Incomplete(RecipeInput("jdk"), id)))
      case Left(StageContractError.UnsupportedOffline) =>
        Left(PluginPublishFault.Recipe(RecipeError.Unsupported(RecipeFeature("offline"), id)))

  private def drift(snapshot: CompilationSnapshot, current: List[SourceBytes]): Option[RepoPath] =
    val recorded = snapshot.sources ++ snapshot.resources
    recorded.collectFirst:
      case input if !same(input, current) => input.path
    match
      case Some(path) => Some(path)
      case None       =>
        current.collectFirst:
          case item if !recorded.exists(_.path == item.path) => item.path

  private def same(input: SelectedInput, current: List[SourceBytes]): Boolean =
    current.find(_.path == input.path) match
      case None        => false
      case Some(found) =>
        Digests.content(found.bytes.toArray) == input.digest && found.bytes.length.toLong == input.length

  private def currentOf(repo: Path, capture: CompilationCapture): IO[PluginPublishFault, List[SourceBytes]] =
    capture match
      case CompilationCapture.Unrecorded         => ZIO.succeed(Nil)
      case CompilationCapture.Recorded(snapshot) =>
        ZIO.foreach(snapshot.sources ++ snapshot.resources): input =>
          readFile(repo.resolve(input.path.value)).map(bytes => SourceBytes(input.path, Chunk.fromArray(bytes)))

  private def descriptor(
      mode: PluginPublication,
      node: RecipeNodeId,
      dependencies: List[MavenDependency],
      snapshot: Option[CompilationSnapshot],
  ): String =
    val modeName = mode match
      case PluginPublication.Rebuildable => "rebuildable"
      case PluginPublication.BinaryOnly  => "binary-only"
    val binding = snapshot match
      case None        => "null"
      case Some(found) => bindingOf(found)
    val deps = dependencies.map(dependencyJson).mkString("[", ",", "]")
    snapshot match
      case Some(found) if mode == PluginPublication.Rebuildable =>
        val sources = (found.sources ++ found.resources).map(sourceJson).mkString("[", ",", "]")
        val closure = found.closure.map(closureJson).mkString("[", ",", "]")
        s"""{"version":1,"mode":${quoted(modeName)},"node":${quoted(
            node.value
          )},"binding":$binding,"dependencies":$deps,"closure":$closure,"sources":$sources}"""
      case _ =>
        s"""{"version":1,"mode":${quoted(modeName)},"node":${quoted(
            node.value
          )},"binding":$binding,"dependencies":$deps}"""

  private def bindingOf(snapshot: CompilationSnapshot): String =
    val compiler = snapshot.compiler.map(identityJson).mkString("[", ",", "]")
    val runtime  = snapshot.runtime.map(identityJson).mkString("[", ",", "]")
    s"""{"scala":${quoted(
        snapshot.version.value
      )},"jdk":${snapshot.jdk.major.value},"compiler":$compiler,"runtime":$runtime}"""

  private def identityJson(identity: ArtifactIdentity): String =
    val format = identity.format match
      case ArtifactFormat.Jar       => "jar"
      case ArtifactFormat.Directory => "directory"
    s"""{"format":${quoted(format)},"digest":${quoted(identity.digest.value)}}"""

  private def dependencyJson(dependency: MavenDependency): String =
    val scope = dependency.scope match
      case MavenScope.Compile  => "compile"
      case MavenScope.Provided => "provided"
    s"""{"group":${quoted(dependency.group.value)},"artifact":${quoted(dependency.name.value)},"revision":${quoted(
        dependency.version.value
      )},"scope":${quoted(scope)}}"""

  private def closureJson(item: ClosureArtifact): String =
    val format = item.artifact.format match
      case ArtifactFormat.Jar       => "jar"
      case ArtifactFormat.Directory => "directory"
    val role = item.role match
      case ClosureRole.Compile     => "compile"
      case ClosureRole.CompileOnly => "compile-only"
      case ClosureRole.Provided    => "provided"
      case ClosureRole.Runtime     => "runtime"
    s"""{"format":${quoted(format)},"role":${quoted(role)},"digest":${quoted(item.artifact.digest.value)}}"""

  private def sourceJson(input: SelectedInput): String =
    val kind = input.kind match
      case SourceKind.Scala    => "scala"
      case SourceKind.Java     => "java"
      case SourceKind.Resource => "resource"
    val provenance = input.provenance match
      case SourceProvenance.Unmanaged => "unmanaged"
      case SourceProvenance.Generated => "generated"
    s"""{"path":${quoted(input.path.value)},"kind":${quoted(kind)},"provenance":${quoted(provenance)},"digest":${quoted(
        input.digest.value
      )}}"""

  private def quoted(raw: String): String =
    val escaped = raw.flatMap:
      case '\\'                   => "\\\\"
      case '"'                    => "\\\""
      case '\n'                   => "\\n"
      case '\r'                   => "\\r"
      case '\t'                   => "\\t"
      case char if char.isControl => f"\\u${char.toInt}%04x"
      case char                   => String.valueOf(char)
    "\"" + escaped + "\""

  private def maven(
      dep: Dep,
      version: ScalaVersion,
      platform: Platform,
  ): Either[PluginPublishFault, MavenDependency] =
    val artifact = ArtifactName.of(dep, version, platform)
    val revision = dep.revision.value
    dep.link match
      case DepLink.CompileOnly => Left(PluginPublishFault.Dependency(dep.organization, artifact, revision))
      case DepLink.Published | DepLink.Provided =>
        Organization.from(dep.organization) match
          case Left(_)      => Left(PluginPublishFault.Dependency(dep.organization, artifact, revision))
          case Right(group) =>
            for
              name <- ArtifactId
                .from(artifact)
                .left
                .map(_ => PluginPublishFault.Dependency(group.value, artifact, revision))
              parsed <- DependencyVersion
                .from(revision)
                .left
                .map(_ => PluginPublishFault.Dependency(group.value, artifact, revision))
            yield MavenDependency(group, name, parsed, dep.exclusions, scope(dep.link))

  private def scope(link: DepLink): MavenScope =
    link match
      case DepLink.Provided                        => MavenScope.Provided
      case DepLink.Published | DepLink.CompileOnly => MavenScope.Compile

  private def kindOf(entry: String): SourceKind =
    if entry.endsWith(".java") then SourceKind.Java
    else if entry.endsWith(".scala") then SourceKind.Scala
    else SourceKind.Resource

  private def seal(entries: List[EntryBytes]): Either[PluginPublishFault, SealedArtifactManifest] =
    entries
      .foldLeft[Either[PluginPublishFault, List[ArtifactEntry]]](Right(Nil)):
        case (Left(error), _)    => Left(error)
        case (Right(acc), entry) =>
          ArtifactPath.from(entry.path) match
            case Left(_)     => Left(PluginPublishFault.Entry(entry.path))
            case Right(path) =>
              val raw = entry.bytes.toArray
              Right(acc :+ ArtifactEntry(path, raw.length.toLong, Digests.content(raw)))
      .flatMap(found => SealedArtifactManifest.from(found).left.map(PluginPublishFault.Seal(_)))

  private def distinct(names: List[String]): Either[PluginPublishFault, Unit] =
    names
      .foldLeft[(Set[String], Option[PluginPublishFault])]((Set.empty, None)):
        case ((_, found @ Some(_)), _)                   => (Set.empty, found)
        case ((seen, None), name) if rejected(name)      => (seen, Some(PluginPublishFault.Entry(name)))
        case ((seen, None), name) if seen.contains(name) =>
          (seen, Some(PluginPublishFault.Jar(JarError.Duplicate(name))))
        case ((seen, None), name) => (seen + name, None)
      ._2 match
      case Some(fault) => Left(fault)
      case None        => Right(())

  private def rejected(name: String): Boolean =
    name.isEmpty ||
      name.startsWith("/") ||
      name.contains('\\') ||
      name.split("/", -1).exists(part => part.isEmpty || part == "." || part == "..")

  private def jar(entries: List[EntryBytes]): Either[PluginPublishFault, Chunk[Byte]] =
    if entries.isEmpty then Left(PluginPublishFault.Jar(JarError.Empty(Path.of("plugin.jar"))))
    else
      try
        val out = new ByteArrayOutputStream
        val zip = new ZipOutputStream(out)
        try
          entries
            .sortBy(_.path)
            .foreach: entry =>
              val item = new ZipEntry(entry.path)
              item.setTime(0L)
              zip.putNextEntry(item)
              zip.write(entry.bytes.toArray)
              zip.closeEntry()
        finally zip.close()
        Right(Chunk.fromArray(out.toByteArray))
      catch case _: IOException => Left(PluginPublishFault.Io(IoFailure(Path.of("plugin.jar"), IoFault.Write)))

  private def readTree(root: Path): IO[PluginPublishFault, List[EntryBytes]] =
    ZIO
      .attemptBlocking(tree(root))
      .mapError(_ => PluginPublishFault.Io(IoFailure(root, IoFault.Walk)))
      .flatMap(ZIO.fromEither)

  private def tree(root: Path): Either[PluginPublishFault, List[EntryBytes]] =
    if !Files.exists(root) then Right(Nil)
    else if !Files.isDirectory(root) then Left(PluginPublishFault.Io(IoFailure(root, IoFault.Stat)))
    else
      val walk = Files.walk(root)
      try
        val found = walk.iterator.asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString)
        found.foldLeft[Either[PluginPublishFault, List[EntryBytes]]](Right(Nil)):
          case (Left(error), _)   => Left(error)
          case (Right(acc), path) =>
            val name = root.relativize(path).toString.replace('\\', '/')
            if rejected(name) then Left(PluginPublishFault.Entry(name))
            else if histos.pack.ClassJar.semanticdb(name) then Right(acc)
            else Right(acc :+ EntryBytes(name, Chunk.fromArray(Files.readAllBytes(path))))
      catch case _: IOException => Left(PluginPublishFault.Io(IoFailure(root, IoFault.Walk)))
      finally walk.close()

  private def readNamed(files: List[(String, Path)]): IO[PluginPublishFault, List[EntryBytes]] =
    ZIO.foreach(files): (name, path) =>
      if rejected(name) then ZIO.fail(PluginPublishFault.Entry(name))
      else readFile(path).map(bytes => EntryBytes(name, Chunk.fromArray(bytes)))

  private def readFile(path: Path): IO[PluginPublishFault, Array[Byte]] =
    ZIO
      .attemptBlocking(Option.when(Files.isRegularFile(path))(Files.readAllBytes(path)))
      .mapError(_ => PluginPublishFault.Io(IoFailure(path, IoFault.Read)))
      .flatMap:
        case Some(bytes) => ZIO.succeed(bytes)
        case None        => ZIO.fail(PluginPublishFault.Io(IoFailure(path, IoFault.Stat)))
end PluginBundle
