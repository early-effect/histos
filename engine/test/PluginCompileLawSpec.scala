package histos.load

import histos.cache.ContentStore
import histos.plugin.{ApiDigest, PluginIo, PluginLoadError}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object PluginCompileLawSpec extends ZIOSpecDefault:
  private val launcher = ApiDigest.trusted("ab" * 32)

  def spec = suite("plugin compile refusals")(
    test("a plugin tree or jar with no scala sources fails before scalac"):
      Workspace
        .work("histos-plugin-empty")
        .flatMap: dir =>
          val tree  = dir.resolve("tree")
          val store = ContentStore.in(dir.resolve("cas"))
          for
            _       <- runningJdk
            _       <- Workspace.writeString(tree.resolve("notes.txt"), "note")
            jarPath <- zip(dir.resolve("plugin.jar"), List("notes.txt" -> "note".getBytes(StandardCharsets.UTF_8)))
            fromDir <- PluginCompile.fromTree(store, launcher, tree, Nil).either
            fromZip <- PluginCompile.fromJar(store, launcher, jarPath, Nil, Nil).either
          yield assertTrue(
            fromDir == emptySources,
            fromZip == emptySources,
          )
    ,
    test("a jar entry that leaves the extract directory is a plugin read error"):
      Workspace
        .work("histos-plugin-slip")
        .flatMap: dir =>
          val store = ContentStore.in(dir.resolve("cas"))
          for
            _       <- runningJdk
            jarPath <- zip(
              dir.resolve("plugin.jar"),
              List("../evil.scala" -> "class Evil".getBytes(StandardCharsets.UTF_8)),
            )
            found <- PluginCompile.fromJar(store, launcher, jarPath, Nil, Nil).either
          yield found match
            case Left(LoadError.Plugin(PluginLoadError.Io(PluginIo.Read(path)))) =>
              assertTrue(path != jarPath)
            case other =>
              assertTrue(other == Left(LoadError.Plugin(PluginLoadError.Io(PluginIo.Read(jarPath))))),
  )

  private val emptySources =
    Left(LoadError.CompileFailed(List(CompilerMessage.fromCompiler("plugin sources are empty"))))

  private def runningJdk: ZIO[Any, Nothing, Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): name =>
      ZIO
        .attempt(Option(System.getProperty(name)))
        .orDie
        .flatMap:
          case Some(value) if value.nonEmpty => TestSystem.putProperty(name, value)
          case _                             => ZIO.die(new IllegalStateException(name))

  private def zip(path: Path, entries: List[(String, Array[Byte])]): ZIO[Any, IoFailure, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new java.util.zip.ZipOutputStream(Files.newOutputStream(path))
        try
          entries.foreach: (name, body) =>
            out.putNextEntry(new java.util.zip.ZipEntry(name))
            out.write(body)
            out.closeEntry()
        finally out.close()
        path
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
end PluginCompileLawSpec
