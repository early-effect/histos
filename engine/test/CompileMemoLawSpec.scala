package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{ClasspathScope, DependencyLock, LockedArtifact, LockedCell, LockedDirect, Repository}
import histos.workspace.Workspace
import histos.{DepKind, Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, ZIO}
import zio.test.*

object CompileMemoLawSpec extends ZIOSpecDefault:
  private val name    = ProjectName("core")
  private val version = ScalaVersion("3.10.0")
  private val sha     = Sha256.trusted("ab" * 32)
  private val library = Sha256.trusted("cd" * 32)
  private val direct  = LockedDirect("org.example", "demo", "1.0.0", DepKind.Java)

  def spec = suite("compile memo laws")(
    test("a missing lock is the pin error, a rewritten lock is read again, and note ignores the file"):
      Workspace
        .work("histos-memo-lock")
        .flatMap: repo =>
          val pin  = lock(sha)
          val next = lock(library)
          val file = histos.resolve.Resolve.lockPath(repo)
          for
            memo    <- CompileMemo.live.build.map(_.get[CompileMemo])
            missing <- memo.lock(repo).either
            _       <- write(repo, pin)
            first   <- memo.lock(repo)
            second  <- memo.lock(repo)
            noted   <- memo.note(pin)
            other   <- memo.note(next)
            written <- ZIO.attemptBlocking(Files.getLastModifiedTime(file))
            _       <- write(repo, next)
            _       <- ZIO.attemptBlocking(Files.setLastModifiedTime(file, written))
            reread  <- memo.lock(repo)
          yield assertTrue(
            missing == Left(
              GraphCompileError.Lock(histos.resolve.ResolveError.NoLock(histos.resolve.Resolve.lockPath(repo)))
            ),
            DependencyLock.parse(DependencyLock.render(pin)) == Right(pin),
            first.lock == pin,
            second == first,
            first.stamp.token.startsWith("f:"),
            noted.lock == pin,
            noted.stamp != other.stamp,
            noted.stamp.token.startsWith("c:"),
            reread.lock == next,
            reread.stamp != first.stamp,
          )
    ,
    test("main and test sources stay apart, and a deleted file is not a hit"):
      Workspace
        .work("histos-memo-sources")
        .flatMap: root =>
          val main   = root.resolve("src/main/scala/demo/A.scala")
          val packed = root.resolve("src/main/scala/js/Pkg.scala")
          val java   = root.resolve("src/main/java/demo/J.java")
          val js     = root.resolve("src/main/js/scala/Js.scala")
          val test   = root.resolve("src/test/scala/demo/T.scala")
          val body   = "object A".getBytes(StandardCharsets.UTF_8)
          for
            memo    <- CompileMemo.live.build.map(_.get[CompileMemo])
            absent  <- memo.sources(root.resolve("missing"), version, Platform.Jvm)
            _       <- write(main, body)
            _       <- write(packed, body)
            _       <- write(java, body)
            _       <- write(js, body)
            _       <- write(test, body)
            jvm     <- memo.sources(root, version, Platform.Jvm)
            jsMain  <- memo.sources(root, version, Platform.Js)
            tests   <- memo.tests(root, version, Platform.Jvm)
            again   <- memo.sources(root, version, Platform.Jvm)
            _       <- ZIO.attemptBlocking(Files.deleteIfExists(main))
            dropped <- memo.sources(root, version, Platform.Jvm)
          yield assertTrue(
            absent.isEmpty,
            jvm.map(_.getFileName.toString).sorted == List("A.scala", "J.java", "Pkg.scala"),
            jsMain.map(_.getFileName.toString).sorted == List("A.scala", "Js.scala", "Pkg.scala"),
            tests.map(_.getFileName.toString) == List("T.scala"),
            again == jvm,
            !dropped.map(_.getFileName.toString).contains("A.scala"),
          )
    ,
    test("a hash is remembered for the stamp, a standard library jar is not a jvm hash, and forget drops it"):
      val module = Module(
        name,
        Platforms(Platform.Jvm, List(Platform.Js)),
        ScalaVersions(version, Nil),
        Nil,
        Nil,
        SourcePos("histos.scala", 1, 1),
      )
      val demo  = artifact("demo", sha)
      val std   = artifact("scala-library", library)
      val pin   = DependencyLock(List(cell(List(demo, std))))
      val js    = DependencyLock(List(cell(List(demo, std), Platform.Js)))
      val stamp = LockStamp.content(sha)
      for
        memo   <- CompileMemo.live.build.map(_.get[CompileMemo])
        jvm    <- memo.hashes(pin, List(module), module, version, Platform.Jvm, ClasspathScope.Compile, stamp)
        cached <- memo.hashes(
          DependencyLock(Nil),
          List(module),
          module,
          version,
          Platform.Jvm,
          ClasspathScope.Compile,
          stamp,
        )
        script <- memo.hashes(
          js,
          List(module),
          module,
          version,
          Platform.Js,
          ClasspathScope.Compile,
          LockStamp.content(library),
        )
        missing <- memo.hashes(
          DependencyLock(Nil),
          List(module),
          module,
          version,
          Platform.Jvm,
          ClasspathScope.Compile,
          LockStamp.content(library),
        )
        _     <- memo.forget
        fresh <- memo.hashes(pin, List(module), module, version, Platform.Jvm, ClasspathScope.Compile, stamp)
      yield assertTrue(
        jvm == Some(List(sha)),
        cached == Some(List(sha)),
        script == Some(List(sha, library)),
        missing == None,
        fresh == Some(List(sha)),
      ),
  )

  private def cell(artifacts: List[LockedArtifact], platform: Platform = Platform.Jvm): LockedCell =
    LockedCell(name, version, platform, List(direct), artifacts, Nil)

  private def artifact(artifactName: String, checksum: Sha256): LockedArtifact =
    LockedArtifact("org.example", artifactName, "1.0.0", checksum, Repository.Central)

  private def lock(checksum: Sha256): DependencyLock =
    DependencyLock(List(cell(List(artifact("demo", checksum)))))

  private def write(repo: Path, pin: DependencyLock) =
    ZIO.attemptBlocking:
      val path = histos.resolve.Resolve.lockPath(repo)
      Files.createDirectories(repo)
      Files.writeString(path, DependencyLock.render(pin))

  private def write(path: Path, body: Array[Byte]) =
    ZIO.attemptBlocking:
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.write(path, body)
end CompileMemoLawSpec
