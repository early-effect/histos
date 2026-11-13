package histos.testrun

import histos.jdk.{Sha256, Sha256Digest}
import histos.native.{NativeOrigin, NativeToolchainError}
import histos.resolve.{
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  LockedCell,
  LockedTool,
  Repositories,
  ResolveError,
  ToolInput,
  ToolRequest,
}
import histos.toolchain.{RuntimeIdentity, Runtimes}
import histos.workspace.Workspace
import histos.zinc.{ApiDigest, CompilerRequests, GraphCompile, UpstreamCell}
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
  ScalacFlag,
  ScalacFlags,
  SourcePos,
  ToolCatalog,
}

import java.nio.file.Path

import zio.{Chunk, ZIO}
import zio.test.*

object SuiteCoverLawSpec extends ZIOSpecDefault:
  private val scala3   = ScalaVersion.trusted("3.7.0")
  private val scala213 = ScalaVersion.trusted("2.13.16")
  private val coreName = ProjectName.trusted("core")
  private val appName  = ProjectName.trusted("app")
  private val bareName = ProjectName.trusted("bare")
  private val jdk      = RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab"))
  private val node     = RuntimeIdentity.Local(Path.of("/node"), Sha256.trusted("cd"))
  private val repos    = Repositories.central
  private val library  = Sha256.trusted("aa")
  private val merged   = Sha256.trusted("bb")
  private val staging  = Sha256.trusted("cc")
  private val kit      = Sha256.trusted("dd")
  private val extra    = Sha256.trusted("ee")
  private val widget   = Sha256.trusted("ff")
  private val toolSha  = Sha256.trusted("11")
  private val compile  = CompilerRequests.compiler(scala3, repos)
  private val old      = CompilerRequests.compiler(scala213, repos)
  private val sem      = CompilerRequests.semanticdb(scala213, ToolCatalog.default, repos)

  def spec = suite("suite cover laws")(
    test("the first gap wins, and a stored cover is own jars, then tools, then the runtime token"):
      val core = module(coreName, Nil, Nil, Nil)
      val app  = module(appName, List(dep("kit")), List(dep("test-kit")), List(ProjectDep(coreName, None)))
      val bare = module(bareName, Nil, Nil, Nil)
      val lock = DependencyLock(
        List(
          cell(
            coreName,
            scala3,
            Platform.Jvm,
            ClasspathScope.Compile,
            List("scala-library" -> library, "extra" -> extra, "extra" -> extra, "kit" -> kit),
          ),
          cell(appName, scala3, Platform.Jvm, ClasspathScope.Compile, List("widget" -> widget)),
          cell(
            appName,
            scala3,
            Platform.Jvm,
            ClasspathScope.Test,
            List("scala-library" -> library, "scala3-library" -> merged, "scala3-staging" -> staging, "kit" -> kit),
          ),
          cell(appName, scala213, Platform.Jvm, ClasspathScope.Test, Nil),
        ),
        Nil,
        List(tool(compile, toolSha), tool(old, Sha256.trusted("22"))),
      )
      val finger  = ToolInput.fingerprint(compile.id, compile.roots.map(row(_, toolSha)))
      val token   = SuiteCover.tokenHash(jdk).value
      val refused = NativeOrigin.Refused(NativeToolchainError.Empty)
      assertTrue(
        SuiteCover.stored(ask(bare, List(bare), scala3, Platform.Js, lock, jdk, None)) ==
          Left(CoverGap.NoNode),
        SuiteCover
          .stored(ask(bare, List(bare), scala3, Platform.Js, DependencyLock(Nil), jdk, Some(node))) ==
          Left(CoverGap.Locked(ResolveError.NoTool(compile.id))),
        SuiteCover.stored(ask(bare, List(bare), scala3, Platform.Native, DependencyLock(Nil), jdk, None, refused)) ==
          Left(CoverGap.Locked(ResolveError.NativeOrigin(NativeToolchainError.Empty))),
        SuiteCover
          .stored(ask(app, List(core, app), scala3, Platform.Native, lock, jdk, None, refused)) ==
          Left(CoverGap.Absent(appName, scala3, Platform.Native, ClasspathScope.Test)),
        SuiteCover.stored(ask(app, List(core, app), scala213, Platform.Jvm, lock, jdk, None)) ==
          Left(CoverGap.Locked(ResolveError.NoTool(sem.id))),
        SuiteCover.stored(
          ask(
            bare,
            List(bare),
            scala3,
            Platform.Jvm,
            DependencyLock(Nil, Nil, List(tool(compile, toolSha))),
            jdk,
            None,
            refused,
          )
        ) ==
          Right(SuiteStamp.Cover(List(finger.value, token), Nil, Runtimes.identityToken(jdk), Nil, Nil)),
        SuiteCover.stored(ask(app, List(core, app), scala3, Platform.Jvm, lock, jdk, None)) ==
          Right(
            SuiteStamp.Cover(
              List(staging.value, kit.value, finger.value, token),
              List(library.value, extra.value),
              Runtimes.identityToken(jdk),
              List("-deprecation"),
              Nil,
            )
          ),
      )
    ,
    test("a missing api file is skipped, an unreadable file is a gap, and a readable file is one module line"):
      val core = module(coreName, Nil, Nil, Nil)
      val app  = module(appName, Nil, List(dep("test-kit")), List(ProjectDep(coreName, None)))
      val lock = DependencyLock(
        List(
          cell(coreName, scala3, Platform.Jvm, ClasspathScope.Compile, Nil),
          cell(appName, scala3, Platform.Jvm, ClasspathScope.Test, Nil),
        ),
        Nil,
        List(tool(compile, toolSha)),
      )
      val digest = Sha256Digest.of(Chunk.fromArray("core-api".getBytes))
      val finger = ToolInput.fingerprint(compile.id, compile.roots.map(row(_, toolSha)))
      val token  = SuiteCover.tokenHash(jdk).value
      val base   =
        SuiteStamp.Cover(List(finger.value, token), Nil, Runtimes.identityToken(jdk), List("-deprecation"), Nil)
      Workspace
        .work("histos-suite-cover")
        .flatMap: repo =>
          val classes  = GraphCompile.classesDir(repo, coreName, scala3, Platform.Jvm)
          val analysis = GraphCompile.analysisDirOf(classes)
          val asked    = ask(app, List(core, app), scala3, Platform.Jvm, lock, jdk, None, repo = repo)
          val missing  = SuiteCover.stored(asked)
          val write    = analysis match
            case Some(dir) =>
              val path = dir.resolve(ApiDigest.fileName)
              Workspace.writeString(path, "nope\n") *>
                ZIO
                  .succeed(SuiteCover.stored(asked))
                  .flatMap: unreadable =>
                    Workspace.writeString(path, digest.value + "\n") *>
                      ZIO.succeed(SuiteCover.stored(asked)).map(readable => (true, unreadable, readable))
            case None => ZIO.succeed((false, missing, missing))
          write.map: (defined, unreadable, readable) =>
            assertTrue(
              defined,
              missing == Right(base),
              unreadable == Left(CoverGap.NoApi(coreName, scala3, Platform.Jvm)),
              readable == Right(base.copy(apis = List(s"${coreName.value} ${digest.value}"))),
            ),
  )

  private def ask(
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      lock: DependencyLock,
      runtime: RuntimeIdentity,
      nodeRuntime: Option[RuntimeIdentity],
      native: NativeOrigin = NativeOrigin.Central,
      repo: Path = Path.of("repo"),
  ): SuiteAsk =
    val upstream = Module
      .chain(module, modules, platform)
      .map(item => UpstreamCell(item, version, GraphCompile.classesDir(repo, item.name, version, platform)))
    SuiteAsk(module, upstream, version, platform, lock, repos, ToolCatalog.default, native, runtime, nodeRuntime)

  private def module(name: ProjectName, dependencies: List[Dep], tests: List[Dep], upstream: List[ProjectDep]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, List(Platform.Js, Platform.Native)),
      ScalaVersions(scala3, List(scala213)),
      dependencies,
      upstream,
      SourcePos("histos.scala", 1, 1),
      testDependencies = tests,
      flags = if name == appName then ScalacFlags.of(ScalacFlag.Deprecation) else ScalacFlags.empty,
    )

  private def dep(name: String): Dep =
    Dep.trusted("org.example", name, "1", DepKind.Java)

  private def cell(
      name: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
      artifacts: List[(String, Sha256)],
  ): LockedCell =
    LockedCell(
      name,
      version,
      platform,
      Nil,
      artifacts.map((artifactName, sha) =>
        LockedArtifact("org.example", artifactName, "1", sha, histos.resolve.Repository.Central)
      ),
      Nil,
      scope,
    )

  private def tool(request: ToolRequest, sha: Sha256): LockedTool =
    LockedTool(
      request.id,
      request.roots,
      Repositories.ordered(request.repositories),
      request.roots.map(root =>
        LockedArtifact(root.organization, root.name, root.revision, sha, histos.resolve.Repository.Central)
      ),
      Nil,
    )

  private def row(root: histos.resolve.ToolRoot, sha: Sha256): ToolInput.Row =
    ToolInput.Row(root.organization, root.name, root.revision, None, sha)
end SuiteCoverLawSpec
