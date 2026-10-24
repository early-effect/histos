package histos.resolve

import histos.{JsEnv, Module, Platform, ProjectGraph, ScalaVersion, ToolCatalog}
import histos.native.{NativeOrigin, NativeToolchain}
import histos.toolchain.{BuildCompiler}
import java.nio.file.Path
import zio.{IO, ZIO, ZLayer}

/** One immutable lock generation and the build's resolver policy. */
final case class LockedTools(
    lock: DependencyLock,
    catalog: ToolCatalog,
    repositories: Repositories,
    resolve: Resolve
):
  def materialize(request: ToolRequest): IO[ResolveError, Classpath] =
    lock.tools.find(_.id == request.id) match
      case None       => ZIO.fail(ResolveError.NoTool(request.id))
      case Some(cell) =>
        ZIO.fromEither(LockedTools.validate(request, cell)) *> ZIO
          .foreach(cell.artifacts)(resolve.fetch)
          .map(Classpath(_, cell.evictions))

  def selected(request: ToolRequest): IO[ResolveError, ToolInput] = materialize(
    request
  ).map(classpath => ToolInput(request.id, classpath.artifacts))

  def compilationRequests(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      native: NativeOrigin
  ): Either[ResolveError, List[ToolRequest]] =
    val compiler = histos.zinc.CompilerRequests.compiler(version, repositories)
    val semantic = if version.value.startsWith("2.13.") then
      List(
        histos.zinc.CompilerRequests.semanticdb(version, catalog, repositories)
      )
    else Nil
    val platformRequests = platform match
      case Platform.Jvm => Right(Nil)
      case Platform.Js  =>
        Right(
          List(
            histos.js.JsRequests.libraries(
              version,
              module.jsVersion.getOrElse(catalog.js.revision),
              repositories,
              catalog
            )
          )
        )
      case Platform.Native =>
        NativeToolchain
          .of(module.nativeVersion.getOrElse(catalog.native.revision), native)
          .left
          .map(ResolveError.NativeOrigin(_))
          .map(toolchain =>
            List(
              histos.native.NativeRequests
                .libraries(version, toolchain, repositories)
            )
          )
    platformRequests.map(requests => compiler :: semantic ++ requests)

  def compilation(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      native: NativeOrigin
  ): IO[ResolveError, List[ToolInput]] =
    ZIO
      .fromEither(compilationRequests(module, version, platform, native))
      .flatMap(requests => ZIO.foreach(requests)(selected))

  def application(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      native: NativeOrigin
  ): IO[ResolveError, List[ToolInput]] =
    val execution = platform match
      case Platform.Jvm => Right(Nil)
      case Platform.Js  =>
        val revision = module.jsVersion.getOrElse(catalog.js.revision)
        Right(
          List(
            histos.js.JsRequests
              .linker(revision, catalog.js.bridgeScala, repositories),
            histos.js.JsRequests
              .environment(module.jsEnv, catalog, repositories)
          ) ++ histos.zinc.CompilerRequests.supporting(
            List(
              catalog.js.bridgeScala,
              histos.js.JsRequests.compiler(module.jsEnv, catalog)
            ),
            catalog,
            repositories
          )
        )
      case Platform.Native =>
        NativeToolchain
          .of(module.nativeVersion.getOrElse(catalog.native.revision), native)
          .left
          .map(ResolveError.NativeOrigin(_))
          .map(toolchain =>
            histos.native.NativeRequests
              .linker(toolchain, catalog, repositories) ::
              histos.zinc.CompilerRequests.supporting(
                List(catalog.native.bridgeScala),
                catalog,
                repositories
              )
          )
    for
      compiled <- compilation(module, version, platform, native)
      requests <- ZIO.fromEither(execution)
      linked <- ZIO.foreach(requests.distinct)(selected)
    yield (compiled ++ linked).distinctBy(_.id)

  def declaredCompilation(
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      native: NativeOrigin
  ): List[String] =
    compilationRequests(
      module,
      version,
      platform,
      native
    ).toOption.toList.flatten.flatMap(request =>
      lock.tools
        .find(_.id == request.id)
        .toList
        .map(cell =>
          request.id.token + "\n" + cell.artifacts
            .map(artifact =>
              s"${artifact.organization} ${artifact.name} ${artifact.revision} ${artifact.classifier.fold("")(_.value)} ${artifact.sha256.value}"
            )
            .mkString("\n")
        )
    )

  def compiler(version: ScalaVersion): IO[ResolveError, Classpath] =
    materialize(histos.zinc.CompilerRequests.compiler(version, repositories))

object LockedTools:
  def validate(
      request: ToolRequest,
      cell: LockedTool
  ): Either[ResolveError, Unit] =
    if cell.roots != request.roots || cell.repositories != Repositories.ordered(
        request.repositories
      )
    then Left(ResolveError.ToolDrift(request.id))
    else
      request.roots.foldLeft[Either[ResolveError, Unit]](Right(())):
        (checked, root) =>
          checked.flatMap: _ =>
            val revision = cell.evictions
              .find(item =>
                item.organization == root.organization && item.name == root.name && item.requested == root.revision
              )
              .fold(root.revision)(_.selected)
            cell.artifacts.filter(artifact =>
              artifact.organization == root.organization && artifact.name == root.name && artifact.revision == revision && artifact.classifier.isEmpty
            ) match
              case _ :: Nil => Right(())
              case Nil      =>
                Left(
                  ResolveError.ToolRoot(request.id, root, ToolRootFault.Missing)
                )
              case _ =>
                Left(
                  ResolveError.ToolRoot(
                    request.id,
                    root,
                    ToolRootFault.Duplicated
                  )
                )

  def make(
      lock: DependencyLock,
      graph: ProjectGraph
  ): ZIO[Resolve, Nothing, LockedTools] =
    ZIO.serviceWith[Resolve](resolve =>
      LockedTools(lock, graph.tools, BuildResolvers(graph.resolvers), resolve)
    )

  def layer(
      repo: Path,
      graph: ProjectGraph
  ): ZLayer[Resolve, ResolveError, LockedTools] =
    ZLayer.fromZIO(Resolve.readLock(repo).flatMap(make(_, graph)))

  def plan(
      graph: ProjectGraph,
      native: NativeOrigin
  ): IO[ResolveError, List[ToolRequest]] =
    val repos = BuildResolvers(graph.resolvers)
    val catalog = graph.tools
    val modules = graph.projects.collect { case module: Module => module }
    val compilerVersions = modules.flatMap(_.versions.all)
    val js = modules.filter(_.platforms.all.contains(Platform.Js))
    val natives = modules.filter(_.platforms.all.contains(Platform.Native))
    for
      build <- BuildCompiler.version
      nativePlans <- ZIO.foreach(natives): module =>
        ZIO
          .fromEither(
            NativeToolchain.of(
              module.nativeVersion.getOrElse(catalog.native.revision),
              native
            )
          )
          .mapError(ResolveError.NativeOrigin(_))
          .map: toolchain =>
            module.versions.all.map(version =>
              histos.native.NativeRequests.libraries(version, toolchain, repos)
            ) ++
              List(
                histos.native.NativeRequests.linker(toolchain, catalog, repos),
                histos.native.NativeRequests.suite(toolchain, catalog, repos)
              )
    yield
      val jsPlans = js.flatMap: module =>
        val revision = module.jsVersion.getOrElse(catalog.js.revision)
        module.versions.all.map(version =>
          histos.js.JsRequests.libraries(version, revision, repos, catalog)
        ) ++ List(
          histos.js.JsRequests.linker(revision, catalog.js.bridgeScala, repos),
          histos.js.JsRequests.environment(module.jsEnv, catalog, repos),
          histos.js.JsRequests.suite(revision, module.jsEnv, catalog, repos),
          histos.js.JsRequests.testBridge(revision, catalog, repos)
        )
      val auxiliary = js.flatMap(module =>
        List(
          catalog.js.bridgeScala,
          histos.js.JsRequests.compiler(module.jsEnv, catalog)
        )
      ) ++
        Option.when(natives.nonEmpty)(catalog.native.bridgeScala).toList
      val versions = (build :: compilerVersions ++ auxiliary).distinct
      val compilerPlans =
        versions.map(version =>
          histos.zinc.CompilerRequests.compiler(version, repos)
        ) ++
          versions
            .filter(_.value.startsWith("2.13."))
            .map(version =>
              histos.zinc.CompilerRequests.semanticdb(version, catalog, repos)
            )
      (compilerPlans ++ jsPlans ++ nativePlans.flatten).distinct
