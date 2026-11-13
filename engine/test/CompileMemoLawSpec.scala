package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{ClasspathScope, DependencyLock, LockedArtifact, LockedCell, LockedDirect, Repository}
import histos.workspace.Workspace
import histos.{DepKind, Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

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
        jvm    <- memo.hashes(pin, module, Nil, version, Platform.Jvm, ClasspathScope.Compile, stamp)
        cached <- memo.hashes(DependencyLock(Nil), module, Nil, version, Platform.Jvm, ClasspathScope.Compile, stamp)
        script <- memo.hashes(js, module, Nil, version, Platform.Js, ClasspathScope.Compile, LockStamp.content(library))
        missing <- memo.hashes(
          DependencyLock(Nil),
          module,
          Nil,
          version,
          Platform.Jvm,
          ClasspathScope.Compile,
          LockStamp.content(library),
        )
        _     <- memo.forget
        fresh <- memo.hashes(pin, module, Nil, version, Platform.Jvm, ClasspathScope.Compile, stamp)
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
end CompileMemoLawSpec
