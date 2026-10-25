package histos.native

import histos.pack.MainClass
import histos.testrun.{ModuleTest, ModuleTestError, TestRun}
import histos.toolchain.Runtimes
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, JvmCompile}
import histos.{
  ClangMajor,
  ClangPin,
  Module,
  Native,
  NativeVersion,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Projects,
  Requires,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  TestMachine,
  TestMain,
  Tool,
  module
}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, System, UIO, ZIO, durationInt}
import zio.test.*

object NativeSpec extends ZIOSpecDefault:
  private val major21 = ClangMajor.trusted(21)
  private val major17 = ClangMajor.trusted(17)
  private val v21 = ClangVersion.trusted("21.0.0", major21)
  private val v17 = ClangVersion.trusted("17.0.0", major17)
  private val ivySnapshot = NativeVersion("0.5.13-SNAPSHOT")
  private val versions = suite("clang version")(
    test(
      "apple clang reports its version and target, and a major is not a version"
    ):
      val text =
        """Apple clang version 21.0.0 (clang-2100.3.34.2)
          |Target: arm64-apple-darwin27.0.0
          |Thread model: posix
          |""".stripMargin
      val parsed = ClangReport.parse(text)
      assertTrue(
        parsed.map(_.version) == Right(v21),
        parsed.map(_.target.value) == Right("arm64-apple-darwin27.0.0"),
        ClangVersion.from("21") == Left(ClangText.BadVersion("21")),
        ClangReport.parse("clang version 0.1.0\nTarget: x\n") == Left(
          ClangText.BadVersion("0.1.0")
        ),
        ClangReport.parse("") == Left(ClangText.Empty)
      )
    ,
    test("the linker child keeps PATH and nothing else"):
      assertTrue(
        NativeLink.linkerEnv(
          Map("PATH" -> "/usr/bin", "HOME" -> "/tmp")
        ) == Map("PATH" -> "/usr/bin"),
        NativeLink.linkerEnv(Map("HOME" -> "/tmp")).isEmpty,
        NativeLink.linkerEnv(Map("PATH" -> "")).isEmpty
      )
    ,
    test("the link key changes when the clang version or the target changes"):
      val sources = histos.jdk.Sha256.of(
        zio.Chunk.fromArray("src".getBytes(StandardCharsets.UTF_8))
      )
      for
        arm64 <- ZIO.fromEither(ClangTarget.from("arm64-apple-darwin27.0.0"))
        x64 <- ZIO.fromEither(ClangTarget.from("x86_64-apple-darwin27.0.0"))
        base = NativeKey
          .of(histos.ScalaVersion("3.9.0"), v21, arm64, sources, Nil)
      yield assertTrue(
        base != NativeKey
          .of(histos.ScalaVersion("3.9.0"), v17, arm64, sources, Nil),
        base != NativeKey.of(
          histos.ScalaVersion("3.9.0"),
          v21,
          x64,
          sources,
          Nil
        )
      )
  )

  private val probe = suite("clang on PATH")(
    test("a clang of the wrong major is a miss and does not search further"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(pair(root, "17.0.0"))
          out <- ClangProbe
            .resolve(major21, Map("PATH" -> root.toString))
            .either
        yield assertTrue(
          out == Left(ClangMiss.WrongMajor(v17, major21, root.resolve("clang")))
        )
    ,
    test("clang++ has to be the same compiler, beside clang"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            script(root, "clang", "21.0.0")
            script(root, "clang++", "17.0.0")
          }
          out <- ClangProbe
            .resolve(major21, Map("PATH" -> root.toString))
            .either
        yield assertTrue(out == Left(ClangMiss.Mismatched(v21, v17)))
    ,
    test("no clang on PATH names the major to install"):
      ClangProbe
        .resolve(major21, Map("PATH" -> ""))
        .either
        .map: out =>
          assertTrue(out == Left(ClangMiss.NotOnPath(major21)))
  )

  private val declare = suite("declaration")(
    test("only a native module names a clang major"):
      val named = ProjectName
        .from("core")
        .map: name =>
          def one(platform: Platform, clang: ClangPin) =
            ProjectGraph.trusted(
              List(
                Module(
                  name,
                  Platforms(platform, Nil),
                  ScalaVersions(ScalaVersion.trusted("3.9.0"), Nil),
                  Nil,
                  Nil,
                  SourcePos("histos.scala", 1, 1),
                  clang
                )
              )
            )
          (
            NativeProject.declaration(one(Platform.Jvm, ClangPin.Unused)),
            NativeProject
              .declaration(one(Platform.Native, ClangPin.Major(major21))),
            NativeProject.declaration(one(Platform.Native, ClangPin.Unused))
          )
      val core = ProjectName.from("core")
      assertTrue(
        named.map((jvm, native, missing) =>
          (jvm, native, missing, core)
        ) == Right(
          (
            NativeDeclaration.NotNative,
            NativeDeclaration.Major(major21),
            core match
              case Right(name) => NativeDeclaration.Missing(name)
              case Left(_)     => NativeDeclaration.NotNative
            ,
            core
          )
        )
      )
  )

  private val toolchain = suite("toolchain")(
    test(
      "unset HISTOS_NATIVE is Central, and the revision stays on the module"
    ):
      val ivy = NativeToolchain.source(Map(NativeToolchain.Key -> "ivy-local"))
      val file =
        NativeToolchain.source(Map(NativeToolchain.Key -> "file:///tmp/native"))
      val revision = NativeToolchain.source(
        Map(NativeToolchain.Key -> "ivy-local@0.5.13-SNAPSHOT")
      )
      val release = NativeToolchain.of(
        NativeToolchain.ReleaseRevision,
        NativeOrigin.read(Map.empty)
      )
      assertTrue(
        ivy == Right(NativeSource.IvyLocal),
        file match
          case Right(NativeSource.Maven(_)) => true
          case _                            => false
        ,
        revision.isLeft,
        NativeToolchain.parse("").isLeft,
        NativeToolchain.parse("central").isLeft,
        release == Right(NativeToolchain.Release),
        NativeToolchain.Release.revision.value == "0.5.12"
      )
    ,
    test("the native linker is not on the engine classpath"):
      for
        classpath <- System.property("java.class.path")
        loads <- ZIO
          .attempt(Class.forName("scala.scalanative.build.Build"))
          .isSuccess
        entries = classpath.toList
          .flatMap(_.split(File.pathSeparatorChar).toList)
          .filter(_.nonEmpty)
      yield assertTrue(
        entries.nonEmpty,
        !entries.exists(entry =>
          Option(Path.of(entry).getFileName)
            .exists(_.toString.startsWith("tools_3"))
        ),
        !loads
      )
    @@ TestAspect.withLiveSystem
  )

  private val link = suite("link")(
    test("an ivy-local override fetches the snapshot jars"):
      for
        toolchain = NativeToolchain(NativeSource.IvyLocal, ivySnapshot)
        libs <- NativeClasspath
          .fetch(toolchain)
          .provideSomeLayer[histos.resolve.LockedTools](
            histos.ToolFixture.nativeLayer(toolchain)
          )
        paths = (libs.plugin :: libs.libraries).map(_.toString)
      yield assertTrue(
        paths.exists(_.contains(ivySnapshot.value)),
        paths.exists(_.contains("nativelib_native0.5_3")),
        !paths.exists(_.contains("tools_3"))
      )
    @@ Requires(Tool.IvyLocalNative(ivySnapshot)),
    test(
      "the toolchain jars are the native libraries and the plugin, not tools"
    ):
      for
        env <- liveEnv
        toolchain <- ZIO.fromEither(
          NativeToolchain.of(
            NativeToolchain.ReleaseRevision,
            NativeOrigin.read(env)
          )
        )
        libs <- NativeClasspath.fetch(toolchain)
        names = (libs.plugin :: libs.libraries).flatMap(path =>
          Option(path.getFileName).map(_.toString)
        )
      yield assertTrue(
        names.exists(_.startsWith("nscplugin_3.9.0")),
        names.exists(_.startsWith("nativelib_native0.5_3")),
        names.exists(_.startsWith("scala3lib_native0.5_3")),
        names.exists(_.startsWith("scalalib_native0.5_3")),
        !names.exists(_.startsWith("tools_3"))
      )
    ,
    test("hello links with the clang on PATH and prints hi"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(write(root, "native/Hello.scala", hello))
          _ <- ZIO.attempt(
            write(root, "jvm/Skip.scala", "object Skip { def n = 1 }\n")
          )
          env <- liveEnv
          found <- ClangProbe.found(env)
          jdk <- GraphCompile.jdkHere
          main <- ZIO.fromEither(MainClass.from("Hello"))
          linked <- NativeLink.link(
            NativeLinkRequest(
              found.version.major,
              NativeToolchain.Release,
              env,
              root.resolve("work"),
              jdk,
              main,
              histos.ScalaVersion("3.9.0")
            ),
            root
          )
          ran <- ZIO.attemptBlocking {
            val process = new ProcessBuilder(linked.binary.toString)
              .redirectErrorStream(true)
              .start()
            try
              val text = new String(
                process.getInputStream.readAllBytes,
                StandardCharsets.UTF_8
              )
              val code = process.waitFor()
              (code, text)
            finally process.destroy()
          }
          shown =
            if Files.isRegularFile(linked.binary) then "file"
            else linked.binary.toString
        yield assertTrue(ran == (0, "hi\n"), shown == "file")
    @@ Requires(Tool.AnyClang)
  ) @@ TestAspect.withLiveSystem

  private val origin = suite("HISTOS_NATIVE")(
    test(
      "the origin a command read from the machine reaches compile, the test compile, and the link"
    ):
      ZIO.scoped:
        for
          root <- temp
          empty = root.resolve("empty-repo")
          _ <- ZIO.attempt {
            Files.createDirectories(empty)
            write(
              root,
              "app/src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def main(args: Array[String]): Unit = ()\n"
            )
            write(
              root,
              "check/src/test/scala/demo/Check.scala",
              "package demo\n\nobject Check:\n  def main(args: Array[String]): Unit = ()\n"
            )
            write(
              root,
              ".histos/dependencies.lock",
              "cell app 3.9.0 native\ncell check 3.9.0 native\n"
            )
          }
          chosen <- ZIO.fromEither(NativeToolchain.parse(empty.toUri.toString))
          missing = CompileError.NativeFetch(
            NativeFetch.Resolved(
              histos.resolve.ResolveError.ToolDrift(
                histos.resolve.ToolId(
                  histos.resolve.ToolRole.NativeLibraries,
                  histos.ScalaVersion("3.9.0"),
                  histos.resolve.ToolVariant.Native(
                    NativeToolchain.ReleaseRevision
                  )
                )
              )
            )
          )
          here <- Runtimes.read(
            TestMachine.at(
              root,
              Map(NativeToolchain.Key -> empty.toUri.toString)
            )
          )
          app <- ZIO.fromEither(ProjectName.from("app"))
          built <- ZIO.fromEither(
            Projects(
              module("app", Native)(histos.ScalaVersion("3.9.0"))
                .clang(major21),
              module("check", Native)(histos.ScalaVersion("3.9.0"))
                .clang(major21)
                .test(TestMain("demo.Check"))
            )
          )
          jdk <- GraphCompile.jdkHere
          _ <- histos.ToolFixture.seed(root, built)
          compiled <- GraphCompile
            .run(
              root,
              built,
              List("app"),
              jdk,
              here.native,
              None,
              _ => ZIO.unit
            )
            .provideSome[histos.resolve.LockedTools](histos.ToolFixture.resolver)
            .either
          tested <- ModuleTest
            .run(TestRun(root, built, List("check"), jdk, here))
            .provideSome[histos.resolve.LockedTools](histos.ToolFixture.resolver)
            .either
          main <- ZIO.fromEither(MainClass.from("demo.Hello"))
          toolchain <- ZIO.fromEither(
            NativeToolchain.of(NativeToolchain.ReleaseRevision, here.native)
          )
          linked <- NativeLink
            .linkClasspath(
              NativeLinkRequest(
                major21,
                toolchain,
                Map.empty,
                root.resolve("link"),
                jdk,
                main,
                histos.ScalaVersion("3.9.0")
              ),
              Nil
            )
            .either
        yield assertTrue(
          compiled == Left(
            GraphCompileError.Cells(
              zio.NonEmptyChunk(
                GraphCompileError
                  .Failed(
                    app,
                    histos.ScalaVersion("3.9.0"),
                    Platform.Native,
                    missing
                  )
              )
            )
          ),
          tested == Left(
            ModuleTestError.Lock(
              histos.resolve.ResolveError.ToolDrift(
                histos.resolve.ToolId(
                  histos.resolve.ToolRole.NativeLibraries,
                  histos.ScalaVersion("3.9.0"),
                  histos.resolve.ToolVariant
                    .Native(NativeToolchain.ReleaseRevision)
                )
              )
            )
          ),
          linked == Left(
            NativeError.Fetch(
              NativeFetch.Resolved(
                histos.resolve.ResolveError.ToolDrift(
                  histos.resolve.ToolId(
                    histos.resolve.ToolRole.NativeLibraries,
                    histos.ScalaVersion("3.9.0"),
                    histos.resolve.ToolVariant.Native(
                      NativeToolchain.ReleaseRevision
                    )
                  )
                )
              )
            )
          )
        )
  )

  private val hello =
    """object Hello:
      |  def main(args: Array[String]): Unit =
      |    println("hi")
      |""".stripMargin

  private val liveEnv: UIO[Map[String, String]] =
    System.envs.orElseSucceed(Map.empty)

  private def pair(dir: Path, version: String): Unit =
    script(dir, "clang", version)
    script(dir, "clang++", version)

  private def script(dir: Path, name: String, version: String): Path =
    Files.createDirectories(dir)
    val file = dir.resolve(name)
    Files.writeString(
      file,
      s"""#!/bin/sh
         |printf '%s\\n' 'clang version $version'
         |printf '%s\\n' 'Target: arm64-apple-darwin27.0.0'
         |""".stripMargin
    )
    file.toFile.setExecutable(true)
    file

  private def write(root: Path, name: String, text: String): Path =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
    path

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-native"))):
      dir =>
        ZIO.attempt {
          if Files.exists(dir) then
            val walk = Files.walk(dir)
            val paths =
              try walk.iterator.asScala.toList
              finally walk.close()
            paths
              .sortBy(_.getNameCount)
              .reverse
              .foreach(path => Files.deleteIfExists(path))
        }.ignore

  def spec =
    suite("scala native")(versions, probe, declare, toolchain, origin, link)
      .provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
        histos.ToolFixture.layer(
          histos.Projects(
            histos
              .module("native", histos.Native)(histos.ScalaVersion("3.9.0"))
              .clang(histos.ClangMajor(21))
          )
        )
      ) @@
      TestAspect.withLiveSystem @@
      TestAspect.timeout(5.minutes)
