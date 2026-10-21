package histos.native

import histos.jdk.{JdkVersion, ReleaseError}
import histos.pack.MainClass
import histos.zinc.{JvmCompile, ResolvedJdk}
import histos.{ClangMajor, ClangPin, Module, NativeVersion, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object NativeSpec extends ZIOSpecDefault:
  private val major21 = ClangMajor.trusted(21)
  private val major17 = ClangMajor.trusted(17)
  private val v21     = ClangVersion.trusted("21.0.0", major21)
  private val v17     = ClangVersion.trusted("17.0.0", major17)
  private val versions = suite("clang version")(
    test("apple clang reports its version and target, and a major is not a version"):
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
        ClangReport.parse("clang version 0.1.0\nTarget: x\n") == Left(ClangText.BadVersion("0.1.0")),
        ClangReport.parse("") == Left(ClangText.Empty),
      )
    ,
    test("the link key changes when the clang version or the target changes"):
      val sources = histos.jdk.Sha256.of(zio.Chunk.fromArray("src".getBytes(StandardCharsets.UTF_8)))
      val target = ClangTarget.from("arm64-apple-darwin27.0.0")
      val other  = ClangTarget.from("x86_64-apple-darwin27.0.0")
      (target, other) match
        case (Right(arm64), Right(x64)) =>
          val base = NativeKey.of(JvmCompile.Scala390, v21, arm64, sources, Nil)
          assertTrue(
            base != NativeKey.of(JvmCompile.Scala390, v17, arm64, sources, Nil),
            base != NativeKey.of(JvmCompile.Scala390, v21, x64, sources, Nil),
          )
        case (left, right) => assertTrue(left == target, right == other)
  )

  private val probe = suite("clang on PATH")(
    test("a clang of the wrong major is a miss and does not search further"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt(pair(root, "17.0.0"))
          out  <- ClangProbe.resolve(major21, Map("PATH" -> root.toString)).either
        yield assertTrue(out == Left(ClangMiss.WrongMajor(v17, major21, root.resolve("clang"))))
    ,
    test("clang++ has to be the same compiler, beside clang"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt {
                    script(root, "clang", "21.0.0")
                    script(root, "clang++", "17.0.0")
                  }
          out <- ClangProbe.resolve(major21, Map("PATH" -> root.toString)).either
        yield assertTrue(out == Left(ClangMiss.Mismatched(v21, v17)))
    ,
    test("no clang on PATH names the major to install"):
      ClangProbe.resolve(major21, Map("PATH" -> "")).either.map: out =>
        assertTrue(out == Left(ClangMiss.NotOnPath(major21)))
  )

  private val declare = suite("declaration")(
    test("only a native module names a clang major"):
      val named = ProjectName.from("core").map: name =>
        def one(platform: Platform, clang: ClangPin) =
          ProjectGraph.trusted(List(Module(
            name,
            Platforms(platform, Nil),
            ScalaVersions(ScalaVersion.trusted("3.9.0"), Nil),
            Nil,
            Nil,
            SourcePos("histos.scala", 1, 1),
            clang,
          )))
        (
          NativeProject.declaration(one(Platform.Jvm, ClangPin.Unused)),
          NativeProject.declaration(one(Platform.Native, ClangPin.Major(major21))),
          NativeProject.declaration(one(Platform.Native, ClangPin.Unused)),
        )
      val core = ProjectName.from("core")
      assertTrue(
        named.map((jvm, native, missing) => (jvm, native, missing, core)) == Right((
          NativeDeclaration.NotNative,
          NativeDeclaration.Major(major21),
          core match
            case Right(name) => NativeDeclaration.Missing(name)
            case Left(_)     => NativeDeclaration.NotNative
          ,
          core,
        ))
      )
  )

  private val toolchain = suite("toolchain")(
    test("unset HISTOS_NATIVE is Central, and the revision stays on the module"):
      val ivy = NativeToolchain.source(Map(NativeToolchain.Key -> "ivy-local"))
      val file = NativeToolchain.source(Map(NativeToolchain.Key -> "file:///tmp/native"))
      val revision = NativeToolchain.source(Map(NativeToolchain.Key -> "ivy-local@0.5.13-SNAPSHOT"))
      val release = NativeToolchain.resolve(NativeToolchain.ReleaseRevision, Map.empty)
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
        NativeToolchain.Release.revision.value == "0.5.12",
      )
    ,
    test("the engine does not load the native linker"):
      val loaded =
        try
          Class.forName("scala.scalanative.build.Build")
          true
        catch case _: ClassNotFoundException => false
      assertTrue(!loaded)
  )

  private val link = suite("link")(
    test("an ivy-local override fetches the snapshot jars"):
      NativeClasspath.fetch(NativeToolchain(NativeSource.IvyLocal, NativeVersion("0.5.13-SNAPSHOT"))).either.map:
        case Right(libs) =>
          val paths = (libs.plugin :: libs.libraries).map(_.toString)
          assertTrue(
            paths.exists(_.contains("0.5.13-SNAPSHOT")),
            paths.exists(_.contains("nativelib_native0.5_3")),
            !paths.exists(_.contains("tools_3")),
          )
        case Left(NativeFetch.Unavailable(source)) => assertTrue(source == NativeSource.IvyLocal)
        case Left(error)                            => assertTrue(error == NativeFetch.Unavailable(NativeSource.IvyLocal))
    ,
    test("the toolchain jars are the native libraries and the plugin, not tools"):
      for
        toolchain <- ZIO.fromEither(NativeToolchain.resolve(NativeToolchain.ReleaseRevision, liveEnv))
        libs <- NativeClasspath.fetch(toolchain)
        names = (libs.plugin :: libs.libraries).flatMap(path => Option(path.getFileName).map(_.toString))
      yield assertTrue(
        names.exists(_.startsWith("nscplugin_3.9.0")),
        names.exists(_.startsWith("nativelib_native0.5_3")),
        names.exists(_.startsWith("scala3lib_native0.5_3")),
        names.exists(_.startsWith("scalalib_native0.5_3")),
        !names.exists(_.startsWith("tools_3")),
      )
    ,
    test("hello links with the clang on PATH and prints hi"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt(write(root, "native/Hello.scala", hello))
          _    <- ZIO.attempt(write(root, "jvm/Skip.scala", "object Skip { def n = 1 }\n"))
          found <- ClangProbe.found(liveEnv)
          jdk <- ZIO.fromEither(running)
          main <- ZIO.fromEither(MainClass.from("Hello"))
          linked <- NativeLink.link(found.version.major, NativeToolchain.ReleaseRevision, liveEnv, root, root.resolve("work"), jdk, main)
          ran <- ZIO.attemptBlocking {
                   val process = new ProcessBuilder(linked.binary.toString).redirectErrorStream(true).start()
                   try
                     val text = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
                     val code = process.waitFor()
                     (code, text)
                   finally process.destroy()
                 }
          shown = if Files.isRegularFile(linked.binary) then "file" else linked.binary.toString
        yield assertTrue(ran == (0, "hi\n"), shown == "file")
  )

  private val hello =
    """object Hello:
      |  def main(args: Array[String]): Unit =
      |    println("hi")
      |""".stripMargin

  private def liveEnv: Map[String, String] = sys.env

  private def running: Either[ReleaseError, ResolvedJdk] =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then Files.readString(home.resolve("release")) else ""
    JdkVersion.fromRelease(text).map(version => ResolvedJdk(home, version))

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
         |""".stripMargin,
    )
    file.toFile.setExecutable(true)
    file

  private def write(root: Path, name: String, text: String): Path =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
    path

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-native"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("scala native")(versions, probe, declare, toolchain, link) @@ TestAspect.sequential @@ TestAspect.timeout(5.minutes)
