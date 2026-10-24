package histos.resolve

import histos.jdk.Sha256
import histos.plugin.LinkStrategy
import histos.{DepKind, Platform, ProjectName, ScalaVersion}

/** One direct dependency, in graph order. */
final case class LockedDirect(
    organization: String,
    name: String,
    revision: String,
    kind: DepKind
)

/** One classpath jar. `name` is the name after the Scala and platform suffix.
  */
final case class LockedArtifact(
    organization: String,
    name: String,
    revision: String,
    sha256: Sha256,
    repository: Repository,
    classifier: Option[JarClassifier] = None
)

/** Which classpath a lock cell is. Compile is published. Test is not. */
enum ClasspathScope:
  case Compile
  case Test

  def header: String = this match
    case Compile => "cell"
    case Test    => "test"

/** One module, Scala version, platform, and classpath. An aggregate has no
  * cell.
  */
final case class LockedCell(
    module: ProjectName,
    scalaVersion: ScalaVersion,
    platform: Platform,
    directs: List[LockedDirect],
    artifacts: List[LockedArtifact],
    evictions: List[Eviction],
    scope: ClasspathScope = ClasspathScope.Compile
)

/** One header coordinate. `source` is present for `Source` and
  * `BinaryOrSource`. `ExactBinary` omits it.
  */
final case class LockedPlugin(
    organization: String,
    name: String,
    revision: String,
    link: LinkStrategy,
    directs: List[LockedDirect],
    artifacts: List[LockedArtifact],
    source: Option[LockedArtifact]
)

/** The committed `.histos/dependencies.lock`. Cells stay in file order. Plugin
  * cells follow in file order.
  */
final case class DependencyLock(
    cells: List[LockedCell],
    plugins: List[LockedPlugin] = Nil,
    tools: List[LockedTool] = Nil
)

object PlatformToken:
  def render(platform: Platform): String = platform.token

  def parse(raw: String): Option[Platform] = Platform.from(raw).toOption

object LinkToken:
  def render(link: LinkStrategy): String =
    link match
      case LinkStrategy.ExactBinary    => "exact-binary"
      case LinkStrategy.Source         => "source"
      case LinkStrategy.BinaryOrSource => "binary-or-source"
      case LinkStrategy.Api            => "api"

  def parse(raw: String): Option[LinkStrategy] =
    raw match
      case "exact-binary"     => Some(LinkStrategy.ExactBinary)
      case "source"           => Some(LinkStrategy.Source)
      case "binary-or-source" => Some(LinkStrategy.BinaryOrSource)
      case _                  => None

object DependencyLock:
  def parse(text: String): Either[LockError, DependencyLock] =
    val lines = text.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList
    if lines.isEmpty then Left(LockError.Empty)
    else
      lines.foldLeft[Either[LockError, Acc]](Right(Acc()))(step).flatMap(build)

  def render(lock: DependencyLock): String =
    val parts = lock.cells.map(renderCell) ++ lock.plugins.map(
      renderPlugin
    ) ++ lock.tools.map(renderTool)
    if parts.isEmpty then ""
    else parts.mkString("\n") + "\n"

  def renderCell(cell: LockedCell): String =
    val header =
      s"${cell.scope.header} ${cell.module.value} ${cell.scalaVersion.value} ${PlatformToken.render(cell.platform)}"
    val body =
      cell.directs.map(renderDirect) ++ cell.artifacts.map(
        renderArtifact
      ) ++ cell.evictions.filter(Eviction.printable).map(renderEvict)
    (header :: body.toList).mkString("\n")

  def renderPlugin(plugin: LockedPlugin): String =
    val header =
      s"plugin ${plugin.organization} ${plugin.name} ${plugin.revision} ${LinkToken.render(plugin.link)}"
    val body =
      plugin.directs.map(renderDirect) ++ plugin.artifacts.map(
        renderArtifact
      ) ++ plugin.source.toList.map(renderSource)
    (header :: body.toList).mkString("\n")

  def renderTool(tool: LockedTool): String =
    val body = tool.roots.map(root =>
      s"root ${root.organization} ${root.name} ${root.revision} ${root.transitivity.token} ${root.origin.fold("build")(Repository.token)}"
    ) ++
      tool.repositories.map(repo =>
        s"resolver ${Repository.token(repo)}"
      ) ++ tool.artifacts.map(renderArtifact) ++ tool.evictions
        .filter(Eviction.printable)
        .map(renderEvict)
    (s"tool ${tool.id.token}" :: body).mkString("\n")

  private def renderDirect(direct: LockedDirect): String =
    s"direct ${direct.organization} ${direct.name} ${direct.revision} ${kind(direct.kind)}"

  private def renderArtifact(artifact: LockedArtifact): String =
    s"artifact ${artifact.organization} ${artifact.name} ${artifact.revision} ${artifact.sha256.value} ${Repository.token(artifact.repository)}${artifact.classifier
        .fold("")(classifier => s" ${classifier.value}")}"

  private def renderSource(artifact: LockedArtifact): String =
    s"source ${artifact.organization} ${artifact.name} ${artifact.revision} ${artifact.sha256.value} ${Repository.token(artifact.repository)}${artifact.classifier
        .fold("")(classifier => s" ${classifier.value}")}"

  private def renderEvict(eviction: Eviction): String =
    s"evict ${eviction.organization} ${eviction.name} ${eviction.requested} ${eviction.selected}"

  private def kind(kind: DepKind): String =
    kind match
      case DepKind.Java   => "java"
      case DepKind.Binary => "binary"
      case DepKind.Full   => "full"

  private final case class Building(
      module: ProjectName,
      scalaVersion: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
      directs: List[LockedDirect],
      artifacts: List[LockedArtifact],
      evictions: List[Eviction]
  )

  private final case class PluginBuilding(
      organization: String,
      name: String,
      revision: String,
      link: LinkStrategy,
      directs: List[LockedDirect],
      artifacts: List[LockedArtifact],
      source: Option[LockedArtifact]
  )

  private enum Open:
    case Module(cell: Building)
    case Plugin(cell: PluginBuilding)
    case Tool(cell: LockedTool)

  private final case class Acc(
      cells: List[LockedCell] = Nil,
      plugins: List[LockedPlugin] = Nil,
      open: Option[Open] = None,
      tools: List[LockedTool] = Nil
  )

  private def step(
      acc: Either[LockError, Acc],
      line: String
  ): Either[LockError, Acc] =
    acc.flatMap(state => applyLine(state, line))

  private def applyLine(state: Acc, line: String): Either[LockError, Acc] =
    tokens(line) match
      case Nil          => Left(LockError.BadLine(line))
      case head :: rest =>
        LockField.from(head) match
          case None        => Left(LockError.BadLine(line))
          case Some(field) =>
            val need = values(field)
            if rest.length < need then Left(LockError.Missing(field))
            else if rest.length > need && !((field == LockField.Artifact || field == LockField.Source) && rest.length == need + 1)
            then Left(LockError.BadLine(line))
            else accept(state, field, rest, line)

  private def values(field: LockField): Int =
    field match
      case LockField.Tool     => 3
      case LockField.Root     => 5
      case LockField.Resolver => 1
      case LockField.Cell     => 3
      case LockField.Test     => 3
      case LockField.Plugin   => 4
      case LockField.Direct   => 4
      case LockField.Artifact => 5
      case LockField.Source   => 5
      case LockField.Evict    => 4

  private def accept(
      state: Acc,
      field: LockField,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    field match
      case LockField.Tool     => openTool(state, rest, line)
      case LockField.Root     => rootTool(state, rest, line)
      case LockField.Resolver => repositoryTool(state, rest, line)
      case LockField.Cell     =>
        openModule(state, rest, line, ClasspathScope.Compile)
      case LockField.Test => openModule(state, rest, line, ClasspathScope.Test)
      case LockField.Plugin   => openPlugin(state, rest, line)
      case LockField.Direct   => onOpen(state, line, directModule, directPlugin)
      case LockField.Artifact =>
        onOpen(state, line, artifactModule, artifactPlugin)
      case LockField.Evict  => evictOnly(state, rest, line)
      case LockField.Source => sourceOnly(state, rest, line)

  private def onOpen(
      state: Acc,
      line: String,
      module: (Building, List[String]) => Either[LockError, Building],
      plugin: (PluginBuilding, List[String]) => Either[
        LockError,
        PluginBuilding
      ]
  ): Either[LockError, Acc] =
    tokens(line) match
      case _ :: rest =>
        state.open match
          case None => Left(LockError.Missing(LockField.Cell))
          case Some(Open.Module(cell)) =>
            module(cell, rest).map(next =>
              state.copy(open = Some(Open.Module(next)))
            )
          case Some(Open.Plugin(cell)) =>
            plugin(cell, rest).map(next =>
              state.copy(open = Some(Open.Plugin(next)))
            )
          case Some(Open.Tool(cell)) =>
            tokens(line) match
              case "artifact" :: fields =>
                artifactOf(fields, LockField.Artifact).map(artifact =>
                  state.copy(open =
                    Some(
                      Open.Tool(
                        cell.copy(artifacts = cell.artifacts :+ artifact)
                      )
                    )
                  )
                )
              case _ => Left(LockError.BadLine(line))
      case Nil => Left(LockError.BadLine(line))

  private def evictOnly(
      state: Acc,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    state.open match
      case None                  => Left(LockError.Missing(LockField.Cell))
      case Some(Open.Plugin(_))  => Left(LockError.BadLine(line))
      case Some(Open.Tool(cell)) =>
        rest match
          case org :: name :: requested :: selected :: Nil =>
            Right(
              state.copy(open =
                Some(
                  Open.Tool(
                    cell.copy(evictions =
                      cell.evictions :+ Eviction(org, name, requested, selected)
                    )
                  )
                )
              )
            )
          case _ => Left(LockError.BadLine(line))
      case Some(Open.Module(cell)) =>
        evict(cell, rest).map(next =>
          state.copy(open = Some(Open.Module(next)))
        )

  private def sourceOnly(
      state: Acc,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    state.open match
      case None => Left(LockError.Missing(LockField.Plugin))
      case Some(Open.Module(_)) | Some(Open.Tool(_)) =>
        Left(LockError.BadLine(line))
      case Some(Open.Plugin(cell)) =>
        if cell.source.isDefined then Left(LockError.BadLine(line))
        else
          artifactOf(rest, LockField.Source).map(source =>
            state.copy(open =
              Some(Open.Plugin(cell.copy(source = Some(source))))
            )
          )

  private def openTool(
      state: Acc,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    rest match
      case role :: version :: variant :: Nil =>
        for
          named <- ToolRole.from(role).toRight(LockError.BadLine(line))
          scalaVersion <- ScalaVersion
            .from(version)
            .left
            .map(_ => LockError.BadLine(line))
          chosen <- ToolVariant.from(variant).toRight(LockError.BadLine(line))
          closed = finish(state)
          id = ToolId(named, scalaVersion, chosen)
          _ <- Either.cond(
            !closed.tools.exists(_.id == id),
            (),
            LockError.DuplicateTool(id)
          )
        yield closed.copy(open =
          Some(Open.Tool(LockedTool(id, Nil, Nil, Nil, Nil)))
        )
      case _ => Left(LockError.BadLine(line))

  private def rootTool(
      state: Acc,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    (state.open, rest) match
      case (
            Some(Open.Tool(tool)),
            org :: name :: revision :: raw :: origin :: Nil
          ) =>
        for
          transitivity <- Transitivity
            .from(raw)
            .toRight(LockError.BadLine(line))
          source <-
            if origin == "build" then Right(None)
            else Repository.parse(origin).map(Some(_))
        yield state.copy(open =
          Some(
            Open.Tool(
              tool.copy(roots =
                tool.roots :+ ToolRoot(
                  org,
                  name,
                  revision,
                  transitivity,
                  source
                )
              )
            )
          )
        )
      case _ => Left(LockError.BadLine(line))

  private def repositoryTool(
      state: Acc,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    (state.open, rest) match
      case (Some(Open.Tool(tool)), raw :: Nil) =>
        Repository
          .parse(raw)
          .map(repo =>
            state.copy(open =
              Some(
                Open.Tool(tool.copy(repositories = tool.repositories :+ repo))
              )
            )
          )
      case _ => Left(LockError.BadLine(line))

  private def openModule(
      state: Acc,
      rest: List[String],
      line: String,
      scope: ClasspathScope
  ): Either[LockError, Acc] =
    rest match
      case moduleRaw :: versionRaw :: platformRaw :: Nil =>
        val parsed =
          for
            module <- ProjectName
              .from(moduleRaw)
              .left
              .map(_ => LockError.BadLine(line))
            version <- ScalaVersion
              .from(versionRaw)
              .left
              .map(_ => LockError.BadLine(line))
            platform <- PlatformToken
              .parse(platformRaw)
              .toRight(LockError.BadLine(line))
          yield (module, version, platform)
        parsed.flatMap: (module, version, platform) =>
          val closed = finish(state)
          if closed.cells.exists(cell =>
              cell.module == module && cell.scalaVersion == version && cell.platform == platform && cell.scope == scope
            )
          then Left(LockError.DuplicateCell(module, version, platform, scope))
          else
            Right(
              closed.copy(open =
                Some(
                  Open.Module(
                    Building(module, version, platform, scope, Nil, Nil, Nil)
                  )
                )
              )
            )
      case _ => Left(LockError.BadLine(line))

  private def openPlugin(
      state: Acc,
      rest: List[String],
      line: String
  ): Either[LockError, Acc] =
    rest match
      case organization :: name :: revision :: strategyRaw :: Nil =>
        LinkToken.parse(strategyRaw) match
          case None       => Left(LockError.BadLine(line))
          case Some(link) =>
            val closed = finish(state)
            if closed.plugins.exists(plugin =>
                plugin.organization == organization && plugin.name == name && plugin.revision == revision
              )
            then Left(LockError.DuplicatePlugin(organization, name, revision))
            else
              Right(
                closed.copy(open =
                  Some(
                    Open.Plugin(
                      PluginBuilding(
                        organization,
                        name,
                        revision,
                        link,
                        Nil,
                        Nil,
                        None
                      )
                    )
                  )
                )
              )
      case _ => Left(LockError.BadLine(line))

  private def directModule(
      cell: Building,
      rest: List[String]
  ): Either[LockError, Building] =
    directOf(rest).map(direct => cell.copy(directs = cell.directs :+ direct))

  private def directPlugin(
      cell: PluginBuilding,
      rest: List[String]
  ): Either[LockError, PluginBuilding] =
    directOf(rest).map(direct => cell.copy(directs = cell.directs :+ direct))

  private def directOf(rest: List[String]): Either[LockError, LockedDirect] =
    rest match
      case organization :: name :: revision :: kindRaw :: Nil =>
        kindOf(kindRaw) match
          case None =>
            Left(
              LockError.BadLine(
                s"direct $organization $name $revision $kindRaw"
              )
            )
          case Some(kind) =>
            Right(LockedDirect(organization, name, revision, kind))
      case _ => Left(LockError.Missing(LockField.Direct))

  private def artifactModule(
      cell: Building,
      rest: List[String]
  ): Either[LockError, Building] =
    artifactOf(rest, LockField.Artifact).map(artifact =>
      cell.copy(artifacts = cell.artifacts :+ artifact)
    )

  private def artifactPlugin(
      cell: PluginBuilding,
      rest: List[String]
  ): Either[LockError, PluginBuilding] =
    artifactOf(rest, LockField.Artifact).map(artifact =>
      cell.copy(artifacts = cell.artifacts :+ artifact)
    )

  private def artifactOf(
      rest: List[String],
      field: LockField
  ): Either[LockError, LockedArtifact] =
    rest match
      case organization :: name :: revision :: shaRaw :: repoRaw :: Nil =>
        for
          sha <- Sha256.from(shaRaw).left.map(_ => LockError.BadSha256(shaRaw))
          repository <- Repository.parse(repoRaw)
        yield LockedArtifact(organization, name, revision, sha, repository)
      case organization :: name :: revision :: shaRaw :: repoRaw :: classifierRaw :: Nil =>
        for
          artifact <- artifactOf(
            List(organization, name, revision, shaRaw, repoRaw),
            field
          )
          classifier <- JarClassifier
            .from(classifierRaw)
            .left
            .map(LockError.Classifier(_))
        yield artifact.copy(classifier = Some(classifier))
      case _ => Left(LockError.Missing(field))

  private def evict(
      cell: Building,
      rest: List[String]
  ): Either[LockError, Building] =
    rest match
      case organization :: name :: requested :: selected :: Nil =>
        Right(
          cell.copy(evictions =
            cell.evictions :+ Eviction(organization, name, requested, selected)
          )
        )
      case _ => Left(LockError.Missing(LockField.Evict))

  private def kindOf(raw: String): Option[DepKind] =
    raw match
      case "java"   => Some(DepKind.Java)
      case "binary" => Some(DepKind.Binary)
      case "full"   => Some(DepKind.Full)
      case _        => None

  private def finish(state: Acc): Acc =
    state.open match
      case None                    => state.copy(open = None)
      case Some(Open.Module(cell)) =>
        val locked = LockedCell(
          cell.module,
          cell.scalaVersion,
          cell.platform,
          cell.directs,
          cell.artifacts,
          cell.evictions,
          cell.scope
        )
        state.copy(cells = state.cells :+ locked, open = None)
      case Some(Open.Tool(cell)) =>
        state.copy(tools = state.tools :+ cell, open = None)
      case Some(Open.Plugin(cell)) =>
        val locked = LockedPlugin(
          cell.organization,
          cell.name,
          cell.revision,
          cell.link,
          cell.directs,
          cell.artifacts,
          cell.source
        )
        state.copy(plugins = state.plugins :+ locked, open = None)

  private def build(state: Acc): Either[LockError, DependencyLock] =
    val closed = finish(state)
    if closed.cells.isEmpty && closed.plugins.isEmpty && closed.tools.isEmpty
    then Left(LockError.Empty)
    else Right(DependencyLock(closed.cells, closed.plugins, closed.tools))

  private def tokens(line: String): List[String] =
    line.split("\\s+").toList.filter(_.nonEmpty)
