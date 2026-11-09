package histos.zinc

import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.toolchain.RuntimeIdentity
import histos.jdk.JdkRuntime

import java.nio.file.Path

import zio.Chunk
import zio.test.*

object CompileLawSpec extends ZIOSpecDefault:
  def spec = suite("compile identity")(
    test("the key changes exactly on the named rows that changed"):
      check(Gen.fromIterable(CompileInput.values), Gen.string, Gen.string): (input, before, after) =>
        val diff = CompileInputs.diff(List(CompileRow(input, before)), List(CompileRow(input, after)))
        assertTrue(diff == (if before == after then Nil else List(input)))
    ,
    test("an api digest and a jar checksum are different rows"):
      val apis      = List(CompileRow(CompileInput.Apis, "digest"))
      val artifacts = List(CompileRow(CompileInput.Artifacts, "digest"))
      assertTrue(
        CompileInputs.diff(apis, artifacts) == List(CompileInput.Artifacts, CompileInput.Apis),
        !CompileInput.values.map(_.token).contains("classes"),
      )
    ,
    test("the stored rows round-trip, and a repeated input does not"):
      val rows = CompileInput.values.toList.map(input => CompileRow(input, input.token))
      val dup  = List(CompileRow(CompileInput.Apis, "a"), CompileRow(CompileInput.Apis, "b"))
      assertTrue(
        CompileInputs.decode(CompileInputs.encode(rows)) == Some(rows),
        CompileInputs.decode(CompileInputs.encode(dup)) == None,
        CompileInputs.decode(Chunk.empty) == None,
      )
    ,
    test("compiler jars replace the compiler row and stay out of the declared rows"):
      val rows = List(CompileRow(CompileInput.Compiler, "old"), CompileRow(CompileInput.Apis, "api"))
      val next = CompileInputs.withCompiler(rows, List(Sha256.trusted("ab")))
      assertTrue(
        next == List(CompileRow(CompileInput.Apis, "api"), CompileRow(CompileInput.Compiler, "ab"))
      )
    ,
    test("a coverage row is absent from a normal compile and present on a coverage compile"):
      val plain   = CompileInputs.declared(request, Sha256.trusted("src"))
      val covered = CompileInputs.declared(request.copy(coverage = Some("on")), Sha256.trusted("src"))
      assertTrue(
        !plain.exists(_.input == CompileInput.Coverage),
        CompileInputs.diff(plain, covered) == List(CompileInput.Coverage),
      ),
  )

  private def request: CompileRequest =
    CompileRequest(
      CompileToolchain(
        histos.ScalaVersion.trusted("3.10.0"),
        histos.Platform.Jvm,
        JdkRuntime(
          Path.of("/jdk"),
          JdkVersion.trusted("21", JdkMajor.trusted(21)),
          RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
        ),
        histos.ScalacFlags.empty,
      ),
      Nil,
      Nil,
      Path.of("classes"),
      Path.of("analysis"),
      Nil,
      Nil,
    )
end CompileLawSpec
