package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{
  Classpath,
  ClasspathScope,
  DependencyLock,
  JarClassifier,
  LockedArtifact,
  LockedCell,
  LockedTools,
  Repositories,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ResolutionService,
  ToolRequest,
}
import histos.workspace.Workspace
import histos.{
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  ToolCatalog,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import slayer.{stub, stubbed}

import zio.{Ref, Scope, UIO, ZIO, ZLayer}
import zio.test.*

object ScalacClasspathLawSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion.trusted("3.7.0")
  private val scala213 = ScalaVersion.trusted("2.13.16")
  private val scala212 = ScalaVersion.trusted("2.12.20")
  private val coreName = ProjectName.trusted("core")
  private val appName  = ProjectName.trusted("app")
  private val missing  = ProjectName.trusted("missing")

  def spec = suite("scalac classpath laws")(
    test("lock hashes follow the chain, drop both scala libraries only on jvm and native, and a hole is none"):
      val core    = module(coreName, Nil)
      val app     = module(appName, List(ProjectDep(coreName, None)))
      val library = artifact("scala-library", "aa")
      val legacy  = artifact("scala3-library", "bb")
      val merged  = artifact("scala3-library_3", "cc")
      val kit     = artifact("kit", "dd")
      val again   = artifact("kit", "dd")
      val other   = artifact("kit", "ee")
      val sources = artifact("kit", "dd", Some(JarClassifier.trusted("sources")))
      val tests   = artifact("test-kit", "ff")
      val lock    = DependencyLock(
        List(
          cell(coreName, scala3, Platform.Jvm, ClasspathScope.Compile, List(library, legacy, kit, again, sources)),
          cell(appName, scala3, Platform.Jvm, ClasspathScope.Compile, List(other)),
          cell(appName, scala3, Platform.Jvm, ClasspathScope.Test, List(merged, tests, kit)),
          cell(coreName, scala3, Platform.Js, ClasspathScope.Compile, List(library, kit)),
          cell(appName, scala3, Platform.Js, ClasspathScope.Compile, List(kit)),
          cell(coreName, scala3, Platform.Native, ClasspathScope.Compile, List(library, merged, kit)),
          cell(appName, scala3, Platform.Native, ClasspathScope.Compile, List(kit)),
          cell(coreName, scala213, Platform.Jvm, ClasspathScope.Compile, List(artifact("scala-compiler", "11"))),
        )
      )
      val on = (version: ScalaVersion) => List(UpstreamCell(core, version, Path.of("core")))
      assertTrue(
        ScalacClasspath.lockHashes(lock, app, on(scala3), scala3, Platform.Jvm, ClasspathScope.Test) ==
          Some(List(legacy.sha256, kit.sha256, sources.sha256, tests.sha256)),
        ScalacClasspath.lockHashes(lock, app, on(scala3), scala3, Platform.Jvm, ClasspathScope.Compile) ==
          Some(List(legacy.sha256, kit.sha256, sources.sha256, other.sha256)),
        ScalacClasspath.lockHashes(lock, app, on(scala3), scala3, Platform.Js, ClasspathScope.Compile) ==
          Some(List(library.sha256, kit.sha256)),
        ScalacClasspath.lockHashes(lock, app, on(scala3), scala3, Platform.Native, ClasspathScope.Compile) ==
          Some(List(kit.sha256)),
        ScalacClasspath.lockHashes(lock, app, on(scala213), scala213, Platform.Jvm, ClasspathScope.Compile).isEmpty,
        ScalacClasspath.lockHashes(lock, core, Nil, scala3, Platform.Jvm, ClasspathScope.Test).isEmpty,
        ScalacClasspath
          .asks(List(Dep.trusted("earlyeffect", "histos-api-extra", "1", DepKind.Java)), scala3, Platform.Jvm),
        ScalacClasspath.asks(List(Dep.trusted("earlyeffect", "histos-api", "1", DepKind.Binary)), scala3, Platform.Js),
        !ScalacClasspath.asks(List(Dep.trusted("org.example", "kit", "1", DepKind.Java)), scala213, Platform.Native),
      )
    ,
    test("paths are the runtime, then lock jars, then class directories, and a failed fetch is the lock"):
      val program =
        for
          dir <- Workspace.work("histos-scalac-cp")
          merged   = dir.resolve("scala3-runtime.jar")
          compiler = dir.resolve("scala3-compiler.jar")
          bridge   = dir.resolve("scala3-bridge.jar")
          library  = dir.resolve("library-2.13.jar")
          nsc      = dir.resolve("scala-compiler.jar")
          reflect  = dir.resolve("scala-reflect.jar")
          sbt      = dir.resolve("scala2-bridge.jar")
          jline    = dir.resolve("jline.jar")
          _ <- jar(
            merged,
            "scala/collection/immutable/List.class" -> "",
            "scala/runtime/Scala3RunTime.class"     -> "",
            props(scala3),
          )
          _    <- jar(compiler, ClassMarker.DottyDriver.entry -> "")
          _    <- jar(bridge, ClassMarker.DottyBridge.entry -> "")
          _    <- jar(library, "scala/collection/immutable/List.class" -> "", props(scala213))
          _    <- jar(nsc, ClassMarker.NscGlobal.entry -> "")
          _    <- jar(reflect, ClassMarker.JavaUniverse.entry -> "")
          _    <- jar(sbt, ClassMarker.XsbtBridge.entry -> "")
          _    <- jar(jline, "org/jline/Terminal.class" -> "")
          seen <- Ref.make(List.empty[ClasspathScope])
          _    <- stub[ResolutionService](_.module(slayer.any[ResolveCell]))((cell: ResolveCell) =>
            if cell.module == missing then ZIO.fail(ResolveError.NotFound("org.example", "missing", "1"))
            else seen.update(_ :+ cell.scope) *> ZIO.succeed(Classpath(artifactsOf(dir, cell), Nil))
          )
          _ <- stub[ResolutionService](_.tool(slayer.any[ToolRequest]))((request: ToolRequest) =>
            val paths =
              if request.id.scalaVersion == scala213 then List(library, nsc, reflect, sbt, jline)
              else List(compiler, bridge, merged)
            ZIO.succeed(Classpath(paths.map(resolved(_, "compiler")), Nil))
          )
          service <- ZIO.service[ResolutionService]
          resolve <- ZIO.service[Resolve]
          tools  = LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve, Some(service))
          base   = module(appName, Nil)
          linked = module(appName, List(ProjectDep(coreName, None)))
            .copy(testDependencies = List(Dep.trusted("org.example", "kit", "1", DepKind.Java)))
          mods = List(module(coreName, Nil), linked)
          js           <- read(dir, mods, base, scala3, Platform.Js, ClasspathScope.Compile, tools, resolve)
          _            <- seen.set(Nil)
          native       <- read(dir, mods, linked, scala3, Platform.Native, ClasspathScope.Compile, tools, resolve)
          nativeScopes <- seen.get
          jvm3         <- read(dir, mods, linked, scala3, Platform.Jvm, ClasspathScope.Compile, tools, resolve)
          jvm213       <- read(dir, mods, base, scala213, Platform.Jvm, ClasspathScope.Compile, tools, resolve)
          _            <- seen.set(Nil)
          tested       <- read(dir, mods, base, scala3, Platform.Js, ClasspathScope.Test, tools, resolve)
          testScopes   <- seen.get
          _            <- seen.set(Nil)
          testedDeps   <- read(dir, mods, linked, scala3, Platform.Js, ClasspathScope.Test, tools, resolve)
          depScopes    <- seen.get
          refused      <- ScalacClasspath
            .of(
              dir,
              base,
              Nil,
              scala212,
              Platform.Jvm,
              DependencyLock(Nil),
              Repositories.central,
              ClasspathScope.Compile,
            )
            .provide(ZLayer.succeed(tools) ++ ZLayer.succeed(resolve))
            .either
          locked <- ScalacClasspath
            .of(
              dir,
              module(missing, Nil),
              Nil,
              scala3,
              Platform.Jvm,
              DependencyLock(Nil),
              Repositories.central,
              ClasspathScope.Compile,
            )
            .provide(ZLayer.succeed(tools) ++ ZLayer.succeed(resolve))
            .either
        yield
          val keptJs = List(
            dir.resolve("scala-library-2.13.16.jar"),
            dir.resolve("scala3-library_3-3.7.0.jar"),
            dir.resolve("scala-library.jar"),
            dir.resolve("kit.jar"),
          )
          val plain     = dir.resolve("scala-library.jar")
          val kit       = dir.resolve("kit.jar")
          val coreJar   = dir.resolve("core.jar")
          val kept      = List(coreJar, plain, kit)
          val names     = List("scala-library", "scala3-library_3", "plain-library", "kit")
          val upJvm     = GraphCompile.classesDir(dir, coreName, scala3, Platform.Jvm)
          val upJs      = GraphCompile.classesDir(dir, coreName, scala3, Platform.Js)
          val upNat     = GraphCompile.classesDir(dir, coreName, scala3, Platform.Native)
          val own       = GraphCompile.classesDir(dir, appName, scala3, Platform.Js)
          val testedJar = dir.resolve("tested.jar")
          assertTrue(
            js == Right(Line(keptJs, keptJs, names)),
            nativeScopes == List(ClasspathScope.Compile, ClasspathScope.Compile),
            native == Right(Line(kept :+ upNat, kept, List("core", "plain-library", "kit"))),
            jvm3 == Right(Line(merged +: kept :+ upJvm, kept, List("core", "plain-library", "kit"))),
            jvm213 == Right(Line(List(library, plain, kit), List(plain, kit), List("plain-library", "kit"))),
            testScopes == List(ClasspathScope.Compile),
            tested == Right(Line(keptJs :+ own, keptJs, names)),
            depScopes == List(ClasspathScope.Compile, ClasspathScope.Test),
            testedDeps == Right(
              Line(List(coreJar, testedJar, own, upJs), List(coreJar, testedJar), List("core", "tested"))
            ),
            refused == Left(
              GraphCompileError.Failed(appName, scala212, Platform.Jvm, CompileError.UnsupportedScala(scala212))
            ),
            locked == Left(GraphCompileError.Lock(ResolveError.NotFound("org.example", "missing", "1"))),
          )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[ResolutionService] ++ stubbed[Resolve]),
  )

  private final case class Line(paths: List[Path], jars: List[Path], hashes: List[String])

  private def read(
      repo: Path,
      modules: List[Module],
      module: Module,
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
      tools: LockedTools,
      resolve: Resolve,
  ) =
    val upstream = Module
      .chain(module, modules, platform)
      .map(item => UpstreamCell(item, version, GraphCompile.classesDir(repo, item.name, version, platform)))
    ScalacClasspath
      .of(repo, module, upstream, version, platform, DependencyLock(Nil), Repositories.central, scope)
      .provide(ZLayer.succeed(tools) ++ ZLayer.succeed(resolve))
      .map(found => Line(found.paths, found.jars, found.hashes.map(_.value)))
      .either

  private def artifactsOf(dir: Path, cell: ResolveCell): List[ResolvedArtifact] =
    if cell.module == coreName then List(resolved(dir.resolve("core.jar"), "core"))
    else if cell.scope == ClasspathScope.Test then List(resolved(dir.resolve("tested.jar"), "tested"))
    else
      List(
        resolved(dir.resolve("scala-library-2.13.16.jar"), "scala-library"),
        resolved(dir.resolve("scala3-library_3-3.7.0.jar"), "scala3-library_3"),
        resolved(dir.resolve("scala-library.jar"), "plain-library"),
        resolved(dir.resolve("kit.jar"), "kit"),
        resolved(dir.resolve("histos-api.jar"), "histos-api"),
        resolved(dir.resolve("kit.jar"), "kit-again"),
      )

  private def resolved(path: Path, name: String): ResolvedArtifact =
    ResolvedArtifact("org.example", name, "1", path, Sha256.trusted(name), histos.resolve.Repository.Central)

  private def props(version: ScalaVersion): (String, String) =
    "library.properties" -> s"version.number=${version.value}\n"

  private def jar(path: Path, entries: (String, String)*): UIO[Unit] =
    ZIO.attemptBlocking {
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        entries.foreach: (name, text) =>
          out.putNextEntry(new ZipEntry(name))
          out.write(text.getBytes(StandardCharsets.UTF_8))
          out.closeEntry()
      finally out.close()
    }.orDie

  private def module(name: ProjectName, upstream: List[ProjectDep]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, List(Platform.Js, Platform.Native)),
      ScalaVersions(scala3, List(scala213)),
      Nil,
      upstream,
      SourcePos("histos.scala", 1, 1),
    )

  private def artifact(name: String, sha: String, classifier: Option[JarClassifier] = None): LockedArtifact =
    LockedArtifact("org.example", name, "1", Sha256.trusted(sha), histos.resolve.Repository.Central, classifier)

  private def cell(
      name: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
      artifacts: List[LockedArtifact],
  ): LockedCell =
    LockedCell(name, version, platform, Nil, artifacts, Nil, scope)
end ScalacClasspathLawSpec
