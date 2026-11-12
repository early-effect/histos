package histos.native

import histos.jdk.Sha256
import histos.workspace.Workspace
import histos.{
  ClangMajor,
  ClangPin,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.Path

import zio.{Exit, ZIO}
import zio.test.*

object NativeLawSpec extends ZIOSpecDefault:
  private val version = ScalaVersion.trusted("3.3.7")
  private val sources = Sha256.trusted("ab")
  private val jar     = Sha256.trusted("cd")

  def spec = suite("native laws")(
    test("clang text is a version and a target, or the gap that text actually has"):
      val happy = ClangReport.parse("clang version 21.0.0\nTarget: arm64-apple-darwin27.0.0")
      assertTrue(
        ClangReport.parse("") == Left(ClangText.Empty),
        ClangReport.parse("hello") == Left(ClangText.NoVersion),
        ClangReport.parse("clang version nope") == Left(ClangText.BadVersion("nope")),
        ClangReport.parse("clang version 21.0.0") == Left(ClangText.NoTarget),
        ClangReport.parse("clang version 21.0.0\nTarget: has space") == Left(ClangText.NoTarget),
        happy match
          case Right(report) => report.version.major == ClangMajor(21) && report.version.value == "21.0.0"
          case Left(_)       => false,
      )
    ,
    test("a path with no clang is absent, and resolve names the major that was wanted"):
      ZIO.scoped:
        Workspace
          .work("histos-clang")
          .flatMap: root =>
            for
              unset   <- ClangProbe.found(Map.empty).exit
              empty   <- ClangProbe.found(Map("PATH" -> "")).exit
              folder  <- ClangProbe.found(Map("PATH" -> root.toString)).exit
              wanted  <- ClangProbe.resolve(ClangMajor(21), Map("PATH" -> root.toString)).exit
              invalid <- ClangProbe.found(Map("PATH" -> "\u0000")).exit
            yield assertTrue(
              failed(unset, ClangMiss.Absent),
              failed(empty, ClangMiss.Absent),
              failed(folder, ClangMiss.Absent),
              failed(wanted, ClangMiss.NotOnPath(ClangMajor(21))),
              failed(invalid, ClangMiss.BadPath("\u0000")),
            )
    ,
    test("an origin is central when unset, ivy-local or a file url when set, and central is not a chosen origin"):
      val file = NativeToolchain.parse("file:/tmp/m2")
      assertTrue(
        NativeToolchain.parse("") == Left(NativeToolchainError.Empty),
        NativeToolchain.parse("org.example@1.0.0") == Left(NativeToolchainError.Revision("org.example@1.0.0")),
        NativeToolchain.parse("ivy-local") == Right(NativeSource.IvyLocal),
        NativeToolchain.parse("central") == Left(NativeToolchainError.Origin("central")),
        NativeToolchain.parse("https://example.test/m2") ==
          Left(NativeToolchainError.Origin("https://example.test/m2")),
        file match
          case Right(NativeSource.Maven(url)) => url.value == "file:/tmp/m2"
          case _                              => false
        ,
        NativeOrigin.read(Map.empty) == NativeOrigin.Central,
        NativeOrigin.read(Map(NativeToolchain.Key -> "ivy-local")) ==
          NativeOrigin.Chosen(NativeSource.IvyLocal),
        NativeOrigin.read(Map(NativeToolchain.Key -> "central")) ==
          NativeOrigin.Refused(NativeToolchainError.Origin("central")),
      )
    ,
    test("a native key changes with clang, target, sources, and artifacts"):
      val clang = ClangVersion.trusted("21.0.0", ClangMajor(21))
      val next  = ClangVersion.trusted("22.0.0", ClangMajor(22))
      val arm   = ClangTarget.from("arm64-apple-darwin")
      val linux = ClangTarget.from("x86_64-pc-linux")
      (arm, linux) match
        case (Right(target), Right(other)) =>
          val base = NativeKey.of(version, clang, target, sources, List(jar))
          assertTrue(
            NativeKey.of(version, next, target, sources, List(jar)) != base,
            NativeKey.of(version, clang, other, sources, List(jar)) != base,
            NativeKey.of(version, clang, target, Sha256.trusted("ef"), List(jar)) != base,
            NativeKey.of(version, clang, target, sources, List(Sha256.trusted("11"))) != base,
            NativeKey.of(version, clang, target, sources, List(jar)) == base,
          )
        case _ => assertTrue(false)
    ,
    test("a native declaration is missing, a major, or a disagreement, and a jvm graph is not native"):
      val unused = native(ProjectName.trusted("client"), ClangPin.Unused)
      val left   = native(ProjectName.trusted("left"), ClangPin.Major(ClangMajor(21)))
      val right  = native(ProjectName.trusted("right"), ClangPin.Major(ClangMajor(22)))
      val agree  = native(ProjectName.trusted("right"), ClangPin.Major(ClangMajor(21)))
      val jvm    = NativeProject.declaration(ProjectGraph.trusted(List(moduleJvm)))
      val missed = NativeProject.declaration(ProjectGraph.trusted(List(unused)))
      val split  = NativeProject.declaration(ProjectGraph.trusted(List(left, right)))
      val same   = NativeProject.declaration(ProjectGraph.trusted(List(left, agree)))
      assertTrue(
        jvm == NativeDeclaration.NotNative,
        missed == NativeDeclaration.Missing(unused.name),
        missed.message.contains(unused.name.value),
        split == NativeDeclaration.Disagree(left.name, right.name),
        same == NativeDeclaration.Major(ClangMajor(21)),
        same.message == "clang 21",
      ),
  )

  private def failed[A](exit: Exit[ClangMiss, A], error: ClangMiss): Boolean =
    exit match
      case Exit.Failure(cause) => !cause.isDie && cause.failureOption.contains(error)
      case Exit.Success(_)     => false

  private def native(name: ProjectName, pin: ClangPin): Module =
    moduleJvm.copy(name = name, platforms = Platforms(Platform.Native, Nil), clang = pin)

  private def moduleJvm: Module =
    Module(
      ProjectName.trusted("core"),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(version, Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )
end NativeLawSpec
