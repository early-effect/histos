package histos.zinc

import histos.jdk.Sha256
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
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ToolRequest,
}
import histos.{Dep, DepKind, Js, Jvm, Module, Platform, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, Ref, Scope, ZIO, ZLayer}
import zio.test.*

object CompileMemoSpec extends ZIOSpecDefault:
  def spec = suite("CompileMemo")(
    test("a deleted source is not a hit after the directory time is restored"):
      ZIO.scoped:
        for
          root <- temp
          hello = root.resolve("src/main/scala/demo/Hello.scala")
          other = root.resolve("src/main/scala/demo/Other.scala")
          _ <- ZIO.attempt {
            write(hello, "package demo\n\nobject Hello\n")
            write(other, "package demo\n\nobject Other\n")
          }
          memo   <- CompileMemo.live.build.map(_.get[CompileMemo])
          listed <- memo.sources(root, ScalaVersion("3.9.0"), Platform.Jvm)
          saved  <- ZIO.attempt(dirTimes(root))
          _      <- ZIO.attempt(Files.delete(other))
          _      <- ZIO.attempt(saved.foreach((path, time) => Files.setLastModifiedTime(path, time)))
          again  <- memo.sources(root, ScalaVersion("3.9.0"), Platform.Jvm)
          restored = saved.forall((path, time) => Files.getLastModifiedTime(path) == time)
        yield assertTrue(
          restored,
          listed.map(name).sorted == List("Hello.scala", "Other.scala"),
          again.map(name) == List("Hello.scala"),
        )
    ,
    test("forget drops a source digest that still has the old file time"):
      ZIO.scoped:
        for
          root <- temp
          path = root.resolve("Hello.scala")
          _      <- ZIO.attempt(Files.writeString(path, "hello"))
          memo   <- CompileMemo.live.build.map(_.get[CompileMemo])
          first  <- memo.digest(List(path))
          time   <- ZIO.attempt(Files.getLastModifiedTime(path))
          _      <- ZIO.attempt(Files.writeString(path, "world"))
          _      <- ZIO.attempt(Files.setLastModifiedTime(path, time))
          second <- memo.digest(List(path))
          _      <- memo.forget
          third  <- memo.digest(List(path))
        yield assertTrue(first == second, first != third)
    ,
    test("one lock stamp fetches a classpath once"):
      val extra = Dep.trusted("com.example", "extra", "1.0.0", DepKind.Java)
      val sha   = Sha256.trusted("ab" * 32)
      Ref.make(0).flatMap { fetches =>
        val asked =
          for
            built <- ZIO.fromEither(Projects(module("app", Js)(ScalaVersion("3.9.0")).libraryDependencies(extra)))
            app   <- only(built.projects.collect { case item: Module => item })
            artifact = LockedArtifact("com.example", "extra", "1.0.0", sha, Repository.Central)
            cell     = LockedCell(
              ProjectName("app"),
              ScalaVersion("3.9.0"),
              Platform.Js,
              List(LockedDirect(extra.organization, extra.name, extra.revision, extra.kind)),
              List(artifact),
              Nil,
            )
            lock = DependencyLock(List(cell))
            tools <- LockedTools.make(lock, built)
            memo  <- CompileMemo.live.build.map(_.get[CompileMemo])
            ask = (stamp: LockStamp) =>
              memo
                .classpath(
                  Path.of("repo"),
                  List(app),
                  app,
                  ScalaVersion("3.9.0"),
                  Platform.Js,
                  lock,
                  BuildResolvers(built.resolvers),
                  ClasspathScope.Compile,
                  stamp,
                )
                .provideSome[Resolve](ZLayer.succeed(tools))
            _     <- ask(LockStamp.file(1, 1))
            once  <- fetches.get
            _     <- ask(LockStamp.file(1, 1))
            twice <- fetches.get
            _     <- ask(LockStamp.file(2, 1))
            moved <- fetches.get
          yield assertTrue(once == 1, twice == 1, moved == 2)
        asked.provideSome[Scope](ZLayer.succeed(Counting(fetches)))
      },
  )

  private def only(found: List[Module]): IO[Nothing, Module] =
    found match
      case item :: Nil => ZIO.succeed(item)
      case _           => ZIO.die(new IllegalStateException("one module"))

  private def name(path: Path): String =
    path.getFileName.toString

  private def dirTimes(root: Path): List[(Path, FileTime)] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala
        .filter(path => Files.isDirectory(path))
        .map { path =>
          path -> Files.getLastModifiedTime(path)
        }
        .toList
    finally walk.close()

  private def write(path: Path, text: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-memo"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
      }.ignore

  private final class Counting(fetches: Ref[Int]) extends Resolve:
    def resolveTool(request: ToolRequest): IO[ResolveError, histos.resolve.Classpath] =
      unused

    def resolve(cell: ResolveCell): IO[ResolveError, histos.resolve.Classpath] =
      unused

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      fetches
        .update(_ + 1)
        .as(
          ResolvedArtifact(
            artifact.organization,
            artifact.name,
            artifact.revision,
            Path.of("extra.jar"),
            artifact.sha256,
            artifact.repository,
            artifact.classifier,
          )
        )

    def resolvePlugin(
        dependency: Dep,
        repositories: histos.resolve.Repositories,
    ): IO[ResolveError, histos.resolve.Classpath] =
      unused

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      unused

    private def unused[A]: IO[ResolveError, A] =
      ZIO.fail(ResolveError.NotFound("unused", "unused", "unused"))
  end Counting
end CompileMemoSpec
