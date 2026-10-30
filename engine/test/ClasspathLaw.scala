package histos.zinc

import histos.jdk.{JdkRuntime, Sha256}
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  LockedCell,
  LockedDirect,
  LockedTools,
  Repository,
  Resolve,
  ResolvedArtifact,
}
import histos.toolchain.BuildCompiler
import histos.workspace.Workspace
import histos.{Dep, Js, Jvm, Module, Platform, ProjectGraph, ProjectName, Projects, ScalaVersion, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Chunk, ZIO, ZLayer}
import zio.test.{assertTrue, suite, test}

/** Classpath and compile key. No scalac and no child.
  */
object ClasspathLaw:
  private val core = ProjectName("core")
  private val app  = ProjectName("app")

  def paths =
    suite("entries")(arranged, foreign, platform)

  def key =
    test("The compile key mixes an upstream's API digest, not a hash of that upstream's class bytes."):
      BuildCompiler.version.flatMap: version =>
        GraphCompile.jdkHere.flatMap: jdk =>
          val classBytes = Sha256.of(Chunk.fromArray("class bytes".getBytes(StandardCharsets.UTF_8)))
          val api        = Sha256.of(Chunk.fromArray("api".getBytes(StandardCharsets.UTF_8)))
          val other      = Sha256.of(Chunk.fromArray("other api".getBytes(StandardCharsets.UTF_8)))
          val sources    = Sha256.of(Chunk.fromArray("sources".getBytes(StandardCharsets.UTF_8)))
          val request    = compileRequest(version, jdk, List(api))
          val text       = CompileInputs.declared(request, sources).map(_.value).mkString("\n")
          ZIO.succeed(
            assertTrue(
              text.contains(api.value),
              !text.contains(classBytes.value),
              CompileKey.of(request, sources) != CompileKey.of(request.copy(upstreamApis = List(other)), sources),
            )
          )

  private def arranged =
    test(
      "Runtime, then the lock jars without a second copy of the standard library, then the class directory even when it is not there yet."
    ):
      BuildCompiler.version.flatMap: version =>
        val dependency = Dep("com.example:extra:1")
        val coreDef    = module("core", Jvm)(version)
        val appDef     = module("app", Jvm)(version).dependsOn(coreDef).libraryDependencies(dependency)
        Workspace
          .work("histos-classpath")
          .flatMap: root =>
            ZIO
              .fromEither(Projects(coreDef, appDef))
              .flatMap: graph =>
                val library = root.resolve("scala-library-locked.jar")
                val extra   = root.resolve("extra.jar")
                val classes = GraphCompile.classesDir(root, core, version)
                for
                  _       <- Workspace.writeString(library, "library")
                  _       <- Workspace.writeString(extra, "extra")
                  appMod  <- named(graph, app)
                  runtime <- JvmCompile.runtime(version)
                  got     <- fetching(library, extra):
                    ScalacClasspath
                      .of(
                        root,
                        modulesOf(graph),
                        appMod,
                        version,
                        Platform.Jvm,
                        lock(
                          graph,
                          app,
                          List(
                            artifact("org.scala-lang", "scala-library", version.value),
                            artifact("com.example", "extra", "1"),
                          ),
                        ),
                        BuildResolvers(graph.resolvers),
                        ClasspathScope.Compile,
                      )
                      .either
                yield got match
                  case Left(error)      => assertTrue(error.message.isEmpty)
                  case Right(classpath) =>
                    assertTrue(
                      classpath.paths == runtime ++ List(extra, classes),
                      !classpath.paths.contains(library),
                      !Files.exists(classes),
                    )

  private def foreign =
    test("An upstream built for another Scala version is not on the classpath."):
      BuildCompiler.version.flatMap: version =>
        val other   = ScalaVersion("3.3.6")
        val coreDef = module("core", Jvm)(version, other)
        val appDef  = module("app", Jvm)(version).dependsOn(coreDef)
        opened(Projects(coreDef, appDef)).flatMap: (root, graph) =>
          named(graph, app).flatMap: appMod =>
            reading:
              ScalacClasspath
                .of(
                  root,
                  modulesOf(graph),
                  appMod,
                  version,
                  Platform.Jvm,
                  lock(graph, app, Nil),
                  BuildResolvers(graph.resolvers),
                  ClasspathScope.Compile,
                )
                .either
                .map:
                  case Left(error)      => assertTrue(error.message.isEmpty)
                  case Right(classpath) =>
                    val current = GraphCompile.classesDir(root, core, version)
                    val built   = GraphCompile.classesDir(root, core, other)
                    assertTrue(classpath.paths.contains(current), !classpath.paths.contains(built))

  private def platform =
    test("A js-only upstream is on the js classpath and not the jvm one."):
      BuildCompiler.version.flatMap: version =>
        val coreDef = module("core", Js)(version)
        val appDef  = module("app", Jvm, Js)(version).dependsOn(Js)(coreDef)
        opened(Projects(coreDef, appDef)).flatMap: (root, graph) =>
          named(graph, app).flatMap: appMod =>
            val modules                 = modulesOf(graph)
            val locked                  = lock(graph, app, Nil)
            val repos                   = BuildResolvers(graph.resolvers)
            def one(platform: Platform) =
              ScalacClasspath
                .of(root, modules, appMod, version, platform, locked, repos, ClasspathScope.Compile)
                .either
            reading:
              one(Platform.Jvm)
                .zip(one(Platform.Js))
                .map:
                  case (Right(jvm), Right(js)) =>
                    val jsDir  = GraphCompile.classesDir(root, core, version, Platform.Js)
                    val jvmDir = GraphCompile.classesDir(root, core, version, Platform.Jvm)
                    assertTrue(js.paths.contains(jsDir), !jvm.paths.contains(jsDir), !jvm.paths.contains(jvmDir))
                  case (jvm, js) =>
                    val detail = List(jvm, js).collect { case Left(error) => error.message }.mkString("\n")
                    assertTrue(detail.isEmpty)

  private def compileRequest(version: ScalaVersion, jdk: JdkRuntime, apis: List[Sha256]) =
    CompileRequest(
      CompileToolchain(version, Platform.Jvm, jdk, histos.ScalacFlags.empty),
      List(Path.of("App.scala")),
      List(Path.of("classes")),
      Path.of("out"),
      Path.of("analysis"),
      Nil,
      apis,
    )

  private def fetching[A](library: Path, extra: Path)(
      use: ZIO[LockedTools & Stubbed[Resolve] & Resolve, Any, A]
  ) =
    (stub[Resolve](_.fetch(any[LockedArtifact]))((artifact: LockedArtifact) =>
      val path = if artifact.name == "scala-library" then library else extra
      ZIO.succeed(
        ResolvedArtifact(
          artifact.organization,
          artifact.name,
          artifact.revision,
          path,
          artifact.sha256,
          artifact.repository,
          artifact.classifier,
        )
      )
    ) *> use).provideSomeLayer[LockedTools](stubbedTraced[Resolve])

  private def reading[A](use: ZIO[LockedTools & Resolve, Any, A]) =
    use.provideSomeLayer[LockedTools](resolveLayer)

  private val resolveLayer =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve))

  private def opened(built: Either[histos.BuildError, ProjectGraph]) =
    for
      root  <- Workspace.work("histos-classpath")
      graph <- ZIO.fromEither(built)
    yield (root, graph)

  private def named(graph: ProjectGraph, name: ProjectName): ZIO[Any, Nothing, Module] =
    modulesOf(graph).filter(_.name == name) match
      case item :: Nil => ZIO.succeed(item)
      case _           => ZIO.dieMessage("classpath graph is missing its module")

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def lock(graph: ProjectGraph, named: ProjectName, jars: List[LockedArtifact]): DependencyLock =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          val directs = Dep
            .select(item.dependencies, platform)
            .map: dep =>
              LockedDirect(dep.organization, dep.name, dep.revision, dep.kind)
          val artifacts = if item.name == named && platform == Platform.Jvm then jars else Nil
          LockedCell(item.name, version, platform, directs, artifacts, Nil)
    DependencyLock(cells)

  private def artifact(organization: String, name: String, revision: String): LockedArtifact =
    LockedArtifact(
      organization,
      name,
      revision,
      Sha256.of(Chunk.fromArray(s"$organization:$name:$revision".getBytes(StandardCharsets.UTF_8))),
      Repository.Central,
    )
end ClasspathLaw
