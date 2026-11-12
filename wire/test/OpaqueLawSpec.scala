package histos

import histos.jdk.{JdkError, JdkMajor, JdkVersion, Sha256, Sha256Error}
import histos.load.{BinaryName, CompilerMessage}
import histos.native.{ClangTarget, ClangText}
import histos.node.{NodeError, NodeMajor, NodeVersion, NodeVersionError}
import histos.pack.{MainClass as PackMain, MainClassError as PackMainError}
import histos.plugin.{ApiDigest, ContentHash, FlagName, Need, PluginDetail, PluginDetailError}
import histos.process.ProcessOutput
import histos.report.{Severity, WireJson}
import histos.resolve.{
  ClasspathScope,
  ClassifierError,
  JarClassifier,
  LockError,
  LockField,
  RepoUrl,
  Repository,
  ToolRole,
  ToolVariant,
  Transitivity,
}
import histos.schedule.{
  ActionId,
  CacheNamespace,
  DevName,
  DevNameError,
  Digest,
  InputName,
  NamespaceError,
  ScheduleSamples,
}
import histos.testrun.{SuiteName, SuiteNameError, SuitePattern, SuitePatternError}
import histos.version.{CiVersion, PullRequestError, PullRequestNumber, ShipNumber, ShipNumberError, TagRelation}
import histos.zinc.{CompileDiagnostic, CompileInput}
import histos.native.NativeSamples

import histos.jdk.ReleaseError
import histos.task.{RunId, TaskError}

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.json.*
import zio.test.*

import WireJson.given

object OpaqueLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  private val dev = DevName("russ")
  private val pr  = PullRequestNumber("4")

  /** 64 lowercase hex from four longs. Char-by-char `listOfN` generators overflowed the Scala Native stack here. */
  private val hex64: Gen[Any, String] =
    Gen
      .listOfN(4)(Gen.long)
      .map(_.map(word => java.lang.Long.toHexString(word).reverse.padTo(16, '0').reverse).mkString)

  def spec = suite("opaque and token laws")(
    test("sha256 is 64 hex after a trim and a lowercase, and any other length or a non-hex char is refused"):
      val nonHex = Gen.unicodeChar.filterNot(char => "0123456789abcdefABCDEF".contains(char))
      for
        accepted <- check(hex64): raw =>
          assertTrue(
            Sha256.from(raw).map(_.value) == Right(raw),
            Sha256.from(s" ${raw.toUpperCase}\n").map(_.value) == Right(raw),
            Sha256.from(raw).map(sha => sha.toJson.fromJson[Sha256]) == Sha256.from(raw).map(Right(_)),
          )
        foreign <- check(hex64, Gen.int(0, 63), nonHex): (hex, at, char) =>
          val raw = hex.patch(at, char.toString, 1)
          assertTrue(Sha256.from(raw) == Left(Sha256Error.Bad(raw)))
        sized <- check(hex64, Gen.int(0, 63), Gen.boolean): (hex, cut, longer) =>
          val raw = if longer then hex + hex.take(cut + 1) else hex.take(cut)
          assertTrue(Sha256.from(raw) == Left(Sha256Error.Bad(raw)))
      yield accepted && foreign && sized && assertTrue(
        Sha256.from("  zz") == Left(Sha256Error.Bad("  zz")),
        "abc".toJson.fromJson[Sha256].isLeft,
      )
    ,
    test("a digest is 64 lowercase hex with no trim, and a dev namespace reads itself then main"):
      val main = CacheNamespace.Main
      val pull = CacheNamespace.PullRequest(pr)
      val name = CacheNamespace.Dev(dev)
      assertTrue(
        Digest.from(hex).contains(ScheduleSamples.digest),
        Digest.from(hex.toUpperCase).isEmpty,
        Digest.from(s" $hex").isEmpty,
        Digest.from(hex.dropRight(1)).isEmpty,
        ScheduleSamples.digest.toJson.fromJson[Digest] == Right(ScheduleSamples.digest),
        hex.dropRight(1).toJson.fromJson[Digest].isLeft,
        DevName.from("russ") == Right(dev),
        DevName.from("") == Left(DevNameError.Segment("")),
        DevName.from("a/b") == Left(DevNameError.Segment("a/b")),
        DevName.from("a b") == Left(DevNameError.Segment("a b")),
        main.reads == List(main),
        pull.reads == List(pull, main),
        name.reads == List(name, main),
        CacheNamespace.from("main") == Right(main),
        CacheNamespace.from(pull.token) == Right(pull),
        CacheNamespace.from(name.token) == Right(name),
        CacheNamespace.from("pr/0") == Left(NamespaceError.Number(PullRequestError.NotPositive(0))),
        CacheNamespace.from("pr/01") == Left(NamespaceError.Number(PullRequestError.NotANumber("01"))),
        CacheNamespace.from("dev/") == Left(NamespaceError.Name(DevNameError.Segment(""))),
        CacheNamespace.from("dev/a/b") == Left(NamespaceError.Shape("dev/a/b")),
        CacheNamespace.from("other") == Left(NamespaceError.Shape("other")),
        main.token == "main",
        pull.token == "pr/4",
        name.token == "dev/russ",
      )
    ,
    test("a compile input's document is the case name and its token is lowercase"):
      val inputs = CompileInput.values.toList
      assertTrue(
        EnumLaw.covered(inputs),
        inputs.map(input => input.toJson.fromJson[CompileInput]) == inputs.map(Right(_)),
        inputs.map(_.toJson) == inputs.map(input => s"\"${input.productPrefix}\""),
        inputs.map(input => CompileInput.from(input.token)) == inputs.map(Some(_)),
        CompileInput.from("Scala").isEmpty,
        ClasspathScope.Compile.toJson == "\"cell\"",
        ClasspathScope.Test.toJson == "\"test\"",
        "\"Compile\"".fromJson[ClasspathScope].isLeft,
        LockField.values.toList.map(field => field.toJson) ==
          LockField.values.toList.map(field => s"\"${LockField.token(field)}\""),
        "\"Cell\"".fromJson[LockField].isLeft,
        EnumLaw.covered(LockField.values.toList),
      )
    ,
    test("tool, scope, and tag tokens round-trip, and the case name is refused"):
      val roles    = ToolRole.values.toList
      val variants = List(
        ToolVariant.Jvm,
        ToolVariant.Js(jsVersion),
        ToolVariant.Suite(jsVersion, JsEnv.Node),
        ToolVariant.Node,
        ToolVariant.Jsdom,
        ToolVariant.Firefox,
        ToolVariant.Native(nativeVersion),
      )
      assertTrue(
        EnumLaw.covered(roles),
        roles.map(_.toJson) == roles.map(role => s"\"${role.productPrefix.toLowerCase}\""),
        roles.map(role => role.toJson.fromJson[ToolRole]) == roles.map(Right(_)),
        "\"Compiler\"".fromJson[ToolRole].isLeft,
        EnumLaw.covered(variants),
        variants.map(variant => variant.toJson.fromJson[ToolVariant]) == variants.map(Right(_)),
        variants.map(_.toJson) == variants.map(variant => s"\"${variant.token}\""),
        "\"Jvm\"".fromJson[ToolVariant].isLeft,
        Transitivity.Closure.toJson == "\"closure\"",
        Transitivity.Single.toJson == "\"single\"",
        "\"Closure\"".fromJson[Transitivity].isLeft,
        JsEnv.values.toList.map(_.toJson) == JsEnv.values.toList.map(env => s"\"${env.token}\""),
        "\"Node\"".fromJson[JsEnv].isLeft,
        Need.Network.toJson == "\"network\"",
        Need.Docker.toJson == "\"docker\"",
        "\"Network\"".fromJson[Need].isLeft,
        TagRelation.Matches.toJson == "\"matches\"",
        TagRelation.Moved.toJson == "\"moved\"",
        TagRelation.Unknown.toJson == "\"unknown\"",
        "\"Matches\"".fromJson[TagRelation].isLeft,
        PomFact.License.toJson == "\"License\"",
        PomFact.Developer.toJson == "\"Developer\"",
        PomFact.Scm.toJson == "\"Scm\"",
        "\"license\"".fromJson[PomFact].isLeft,
        Severity.Error.toJson == "\"error\"",
        Severity.Warn.toJson == "\"warn\"",
        Severity.Info.toJson == "\"info\"",
        "\"Error\"".fromJson[Severity].isLeft,
        Platform.Jvm.toJson == "\"jvm\"",
        "\"Jvm\"".fromJson[Platform].isLeft,
      )
    ,
    test("a repository token is central, ivy-local, or maven, and serves uses the canonical root"):
      val https = "https://repo.example/maven"
      val file  = "file:///tmp/m2"
      val maven = Repository.parse(s"maven:$https")
      val local = Repository.parse("ivy-local")
      assertTrue(
        Repository.parse("central") == Right(Repository.Central),
        local == Right(Repository.IvyLocal),
        maven.map(Repository.token) == Right(s"maven:$https"),
        maven.map(_.toJson.fromJson[Repository]) == maven.map(Right(_)),
        Repository.parse("maven:nope") == Left(LockError.BadRepository("nope")),
        Repository.parse("other") == Left(LockError.BadRepository("other")),
        RepoUrl.from(https).map(_.value) == Right(https),
        RepoUrl.from("http://repo.example") == Left(LockError.BadRepository("http://repo.example")),
        "\"central\"".fromJson[Repository] == Right(Repository.Central),
        "\"nope\"".fromJson[Repository].isLeft,
        Repository.serves(Repository.Central, "https://repo1.maven.org/maven2/a/b") == true,
        Repository.serves(Repository.Central, "https://repo1.maven.org/maven2") == false,
        Repository.serves(Repository.Central, "https://repo1.maven.org/maven2/") == false,
        Repository.serves(Repository.Central, "https://repo1.maven.org/maven2-extra/a") == false,
        Repository.serves(Repository.IvyLocal, "file://root/.ivy2/local/a") == true,
        Repository.serves(Repository.IvyLocal, "file://root/local/a") == false,
        maven.exists(repo => Repository.serves(repo, s"$https/dev/zio/zio/1")),
        maven.exists(repo => Repository.serves(repo, https)),
        Repository.serves(Repository.Maven(RepoUrl.trusted(file)), "file:/tmp/m2/a"),
        Repository.serves(Repository.Maven(RepoUrl.trusted("file://tmp/m2")), "file:/tmp/m2/a"),
      )
    ,
    test("node and jdk majors are positive ints, and a bare node major is only a codec"):
      assertTrue(
        JdkMajor.from(21).map(_.value) == Right(21),
        JdkMajor.from(0) == Left(JdkError.BadMajor(0)),
        JdkMajor.from(-1) == Left(JdkError.BadMajor(-1)),
        "21".fromJson[JdkMajor].map(_.value) == Right(21),
        "0".fromJson[JdkMajor].isLeft,
        ClangMajor.from(21).map(_.value) == Right(21),
        ClangMajor.from(0) == Left(ClangError.NotPositive(0)),
        "21".fromJson[ClangMajor].map(_.value) == Right(21),
        "0".fromJson[ClangMajor].isLeft,
        NodeMajor.from(22).map(_.value) == Right(22),
        NodeMajor.from(0) == Left(NodeError.BadMajor(0)),
        "22".fromJson[NodeMajor].map(_.value) == Right(22),
        "0".fromJson[NodeMajor].isLeft,
        NodeVersion.from("22.14.0") == Right(node),
        NodeVersion.from("v22.14.0") == Right(node),
        NodeVersion.from("V22.14.0") == Right(node),
        NodeVersion.from("25") == Left(NodeVersionError.Bad("25")),
        "\"v25.0.0-rc.1\"".fromJson[NodeVersion] ==
          Right(NodeVersion.trusted("25.0.0-rc.1", NodeMajor.trusted(25))),
        "\"22.14.0\"".fromJson[NodeVersion] == Right(node),
        "\"v22.14.0\"".fromJson[NodeVersion] == Right(node),
        "\"25\"".fromJson[NodeVersion] == Right(NodeVersion.trusted("25", NodeMajor.trusted(25))),
        "\"0\"".fromJson[NodeVersion].isLeft,
        "\"01\"".fromJson[NodeVersion].isLeft,
        "\"-1\"".fromJson[NodeVersion].isLeft,
        JdkVersion.from("21.0.2") == Right(jdk),
        JdkVersion.from("nope") == Left(histos.jdk.VersionError.Bad("nope")),
        "\"nope\"".fromJson[JdkVersion].isLeft,
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2\"") == Right(jdk),
        JdkVersion.fromRelease("") == Left(ReleaseError.MissingKey),
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2") == Left(ReleaseError.Unclosed),
        JdkVersion.fromRelease("JAVA_VERSION=\"nope\"") == Left(ReleaseError.BadVersion("nope")),
      )
    ,
    test("every parsed text codec round-trips its sample and refuses what its parser refuses"):
      JsonLaw.parsed(core, "", ProjectName.from) &&
      JsonLaw.parsed(scala3, "3", ScalaVersion.from) &&
      JsonLaw.parsed(dep, "nope", Dep.from) &&
      JsonLaw.parsed(binaryDep, "dev.zio::zio", Dep.from) &&
      JsonLaw.parsed(fullDep, "dev.zio:::zio", Dep.from) &&
      JsonLaw.parsed(ship, "", histos.version.ShipName.from) &&
      JsonLaw.parsed(number, "01.2.3", ShipNumber.from) &&
      JsonLaw.parsed(param, "", histos.plugin.ParamName.from) &&
      JsonLaw.parsed(flag, "1flag", FlagName.from) &&
      JsonLaw.parsed(plugin, "", histos.plugin.PluginId.from) &&
      JsonLaw.parsed(taskId, "nope", histos.plugin.PluginTaskId.parse) &&
      JsonLaw.parsed(org, "", Organization.from) &&
      JsonLaw.parsed(WireSamples.main, "", histos.MainClass.from) &&
      JsonLaw.parsed(packed, "", PackMain.from) &&
      JsonLaw.parsed(jsVersion, "", JsVersion.from) &&
      JsonLaw.parsed(nativeVersion, "", NativeVersion.from) &&
      JsonLaw.parsed(revision, "", ToolRevision.from) &&
      JsonLaw.parsed(projectPath, "../x", histos.ProjectPath.from) &&
      JsonLaw.parsed(repoPath, "../x", histos.plugin.RepoPath.from) &&
      JsonLaw.parsed(entry, "", histos.plugin.EntryName.from) &&
      JsonLaw.parsed(digest, "abcd", ApiDigest.from) &&
      JsonLaw.parsed(hash, "abcd", ContentHash.from) &&
      JsonLaw.parsed(testMain, "", TestMain.from) &&
      JsonLaw.parsed(framework, "", FrameworkClass.from) &&
      JsonLaw.parsed(SuiteName("demo.Suite"), "", SuiteName.from) &&
      JsonLaw.parsed(SuitePattern("App*"), "a b", SuitePattern.from) &&
      JsonLaw.parsed(JarClassifier("tests"), "has space", JarClassifier.from) &&
      JsonLaw.parsed(ScheduleSamples.compile, "", ActionId.from) &&
      JsonLaw.parsed(ScheduleSamples.sources, "", InputName.from) &&
      JsonLaw.parsed(RunId("run-1"), "", RunId.from) &&
      JsonLaw.parsed(ClangTarget("arm64-apple-darwin"), "has space", ClangTarget.from) &&
      JsonLaw.parsed(NativeSamples.version, "nope", histos.native.ClangVersion.from) &&
      assertTrue(
        List(dep, binaryDep, fullDep).map(_.toJson) ==
          List("\"dev.zio:zio:2.1.27\"", "\"dev.zio::zio:2.1.27\"", "\"dev.zio:::zio:2.1.27\""),
        Dep.from("dev.zio::zio:2.1.27") == Right(binaryDep),
        Dep.from("dev.zio:::zio:2.1.27") == Right(fullDep),
      )
    ,
    test("a pattern, run id, and clang target literal is checked when it compiles, and each parser keeps its error"):
      assertTrue(
        SuitePattern("App*").value == "App*",
        SuitePattern.from("App*") == Right(SuitePattern("App*")),
        SuitePattern.from("") == Left(SuitePatternError.Empty),
        SuitePattern.from("a b") == Left(SuitePatternError.Blank("a b")),
        typeCheckErrors("""histos.testrun.SuitePattern("a b")""").map(_.message) ==
          List(SuitePatternError.Blank("a b").message),
        typeCheckErrors("""histos.testrun.SuitePattern("")""").map(_.message) ==
          List(SuitePatternError.Empty.message),
        RunId("run-1").value == "run-1",
        RunId.from("run-1") == Right(RunId("run-1")),
        RunId.from("") == Left(TaskError.EmptyId),
        typeCheckErrors("""histos.task.RunId("")""").map(_.message) == List(TaskError.EmptyId.message),
        ClangTarget("arm64-apple-darwin").value == "arm64-apple-darwin",
        ClangTarget.from("arm64-apple-darwin") == Right(ClangTarget("arm64-apple-darwin")),
        ClangTarget.from("") == Left(ClangText.NoTarget),
        ClangTarget.from("has space") == Left(ClangText.NoTarget),
        typeCheckErrors("""histos.native.ClangTarget("has space")""").map(_.message) ==
          List(ClangText.NoTarget.message),
      )
    ,
    test("a total text codec keeps any text, and the other small codecs keep their value"):
      assertTrue(
        SuiteName.from("") == Left(SuiteNameError.Empty),
        JarClassifier.from("tests") == Right(JarClassifier("tests")),
        JarClassifier.from("") == Left(ClassifierError.Empty),
        JarClassifier.from("has space") == Left(ClassifierError.Invalid("has space")),
        PluginDetail.from("") == Left(PluginDetailError.Empty),
        "".toJson.fromJson[PluginDetail].map(_.value) == Right(""),
        detail.toJson.fromJson[PluginDetail] == Right(detail),
        CompilerMessage.fromCompiler("").text.isEmpty,
        "".toJson.fromJson[CompilerMessage].map(_.text) == Right(""),
        BinaryName.trusted("a.B").toJson.fromJson[BinaryName].map(_.value) == Right("a.B"),
        CompileDiagnostic.fromCompiler("warn").toJson.fromJson[CompileDiagnostic].map(_.text) == Right("warn"),
        ProcessOutput.from("out").toJson.fromJson[ProcessOutput].map(_.text) == Right("out"),
        CiVersion(number).toJson == number.toJson,
        CiVersion(number).toJson.fromJson[CiVersion] == Right(CiVersion(number)),
        ShipNumber.from("01.2.3") == Left(ShipNumberError.NotAShip("01.2.3")),
        ActionId.from("") == Left(histos.schedule.ActionIdError.Empty),
        InputName.from("") == Left(histos.schedule.InputNameError.Empty),
        path.toJson.fromJson[java.nio.file.Path] == Right(path),
        "a\u0000b".toJson.fromJson[java.nio.file.Path].isLeft,
        library.toJson.fromJson[ProviderLibrary] == Right(library),
        """{"organization":"","name":"zio"}""".fromJson[ProviderLibrary].isLeft,
        provider.toJson.fromJson[TestProvider] == Right(provider),
        PackMain.from("") == Left(PackMainError.Empty),
      ),
  )
end OpaqueLawSpec
