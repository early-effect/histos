package histos

import histos.jdk.{JdkError, JdkMajor, JdkVersion, Sha256, Sha256Error}
import histos.load.{BinaryName, CompilerMessage}
import histos.native.{ClangTarget, ClangText}
import histos.node.{NodeError, NodeMajor, NodeVersion, NodeVersionError}
import histos.pack.{MainClass as PackMain, MainClassError as PackMainError}
import histos.plugin.{ApiDigest, ContentHash, FlagName, Need, PluginDetail, PluginDetailError, PluginTaskError}
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

import zio.json.*
import zio.test.*
import zio.Chunk

import WireJson.given

object OpaqueLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  private val dev = DevName("russ")
  private val pr  = PullRequestNumber("4")

  def spec = suite("opaque and token laws")(
    test("sha256 accepts 64 hex, lowercases it, and rejects anything else"):
      val alphabet = "0123456789abcdef"
      check(Gen.listOfN(64)(Gen.elements(alphabet.toList*))): chars =>
        val raw   = chars.mkString
        val upper = s" $raw".toUpperCase + "\n"
        assertTrue(
          Sha256.from(raw).map(_.value) == Right(raw),
          Sha256.from(upper).map(_.value) == Right(raw),
          Sha256.from(raw).map(sha => sha.toJson.fromJson[Sha256]) == Sha256.from(raw).map(Right(_)),
        ) && assertTrue(
          Sha256.from("abc") == Left(Sha256Error.Bad("abc")),
          Sha256.from("zz") == Left(Sha256Error.Bad("zz")),
          Sha256.from("  zz") == Left(Sha256Error.Bad("  zz")),
          "abc".toJson.fromJson[Sha256] == Left(s"(${Sha256Error.Bad("abc").message})"),
          "1".fromJson[Sha256] == Left("(expected string)"),
          "\"".fromJson[Sha256] == Left("Unexpected end of input"),
          Sha256.of(Chunk.empty).value == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          Sha256.of(Chunk.fromArray("a".getBytes("UTF-8"))) != Sha256.of(Chunk.empty),
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
        hex.dropRight(1).toJson.fromJson[Digest] == Left(s"('${hex.dropRight(1)}' is not a digest)"),
        "1".fromJson[Digest] == Left("(expected string)"),
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
        "\"scala\"".fromJson[CompileInput] == Left("(invalid enumeration value)"),
        ClasspathScope.Compile.toJson == "\"cell\"",
        ClasspathScope.Test.toJson == "\"test\"",
        "\"Compile\"".fromJson[ClasspathScope] == Left("('Compile' is not a classpath scope)"),
        LockField.values.toList.map(field => field.toJson) ==
          LockField.values.toList.map(field => s"\"${LockField.token(field)}\""),
        "\"Cell\"".fromJson[LockField] == Left("('Cell' is not a lock field)"),
        EnumLaw.covered(LockField.values.toList),
      )
    ,
    test("tool, scope, and tag tokens round-trip, and the case name is rejected"):
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
        "\"Compiler\"".fromJson[ToolRole] == Left("('Compiler' is not a tool role)"),
        EnumLaw.covered(variants),
        variants.map(variant => variant.toJson.fromJson[ToolVariant]) == variants.map(Right(_)),
        variants.map(_.toJson) == variants.map(variant => s"\"${variant.token}\""),
        "\"Jvm\"".fromJson[ToolVariant] == Left("('Jvm' is not a tool variant)"),
        Transitivity.Closure.toJson == "\"closure\"",
        Transitivity.Single.toJson == "\"single\"",
        "\"Closure\"".fromJson[Transitivity] == Left("('Closure' is not a transitivity)"),
        JsEnv.values.toList.map(_.toJson) == JsEnv.values.toList.map(env => s"\"${env.token}\""),
        "\"Node\"".fromJson[JsEnv] == Left("('Node' is not a js environment)"),
        Need.Network.toJson == "\"network\"",
        Need.Docker.toJson == "\"docker\"",
        "\"Network\"".fromJson[Need] == Left("('Network' is not a need)"),
        TagRelation.Matches.toJson == "\"matches\"",
        TagRelation.Moved.toJson == "\"moved\"",
        TagRelation.Unknown.toJson == "\"unknown\"",
        "\"Matches\"".fromJson[TagRelation] == Left("('Matches' is not a tag relation)"),
        PomFact.License.toJson == "\"License\"",
        PomFact.Developer.toJson == "\"Developer\"",
        PomFact.Scm.toJson == "\"Scm\"",
        "\"license\"".fromJson[PomFact] == Left("('license' is not a pom fact)"),
        Severity.Error.toJson == "\"error\"",
        Severity.Warn.toJson == "\"warn\"",
        Severity.Info.toJson == "\"info\"",
        "\"Error\"".fromJson[Severity] == Left("(unknown severity Error)"),
        Platform.Jvm.toJson == "\"jvm\"",
        "\"Jvm\"".fromJson[Platform] == Left(s"(${PlatformError.Unknown("Jvm").message})"),
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
        maven.map(_.toJson.fromJson[Repository]) == maven.map(Right(_)),
        Repository.parse("maven:nope") == Left(LockError.BadRepository("nope")),
        Repository.parse("other") == Left(LockError.BadRepository("other")),
        RepoUrl.from(https).map(_.value) == Right(https),
        RepoUrl.from("http://repo.example") == Left(LockError.BadRepository("http://repo.example")),
        "\"central\"".fromJson[Repository] == Right(Repository.Central),
        "\"nope\"".fromJson[Repository] == Left(s"(${LockError.BadRepository("nope").message})"),
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
        "\"21\"".fromJson[JdkMajor] == "21".fromJson[JdkMajor],
        "0".fromJson[JdkMajor] == Left(s"(${JdkError.BadMajor(0).message})"),
        "\"0\"".fromJson[JdkMajor] == Left(s"(${JdkError.BadMajor(0).message})"),
        "nope".fromJson[JdkMajor] == Left("(expected an Int)"),
        "\"nope\"".fromJson[JdkMajor] == Left("(expected an Int)"),
        ClangMajor.from(21).map(_.value) == Right(21),
        ClangMajor.from(0) == Left(ClangError.NotPositive(0)),
        "\"21\"".fromJson[ClangMajor] == "21".fromJson[ClangMajor],
        "0".fromJson[ClangMajor] == Left(s"(${ClangError.NotPositive(0).message})"),
        "nope".fromJson[ClangMajor] == Left("(expected an Int)"),
        NodeMajor.from(22).map(_.value) == Right(22),
        NodeMajor.from(0) == Left(NodeError.BadMajor(0)),
        "\"22\"".fromJson[NodeMajor] == "22".fromJson[NodeMajor],
        "0".fromJson[NodeMajor] == Left(s"(${NodeError.BadMajor(0).message})"),
        "nope".fromJson[NodeMajor] == Left("(expected an Int)"),
        NodeVersion.from("22.14.0") == Right(node),
        NodeVersion.from("v22.14.0") == Right(node),
        NodeVersion.from("V22.14.0") == Right(node),
        NodeVersion.from("25") == Left(NodeVersionError.Bad("25")),
        "\"22.14.0\"".fromJson[NodeVersion] == Right(node),
        "\"v22.14.0\"".fromJson[NodeVersion] == Right(node),
        "\"25\"".fromJson[NodeVersion] == Right(NodeVersion.trusted("25", NodeMajor.trusted(25))),
        "\"0\"".fromJson[NodeVersion] == Left(s"(${NodeVersionError.Bad("0").message})"),
        "\"01\"".fromJson[NodeVersion] == Left(s"(${NodeVersionError.Bad("01").message})"),
        "\"-1\"".fromJson[NodeVersion] == Left(s"(${NodeVersionError.Bad("-1").message})"),
        JdkVersion.from("21.0.2") == Right(jdk),
        JdkVersion.from("nope") == Left(histos.jdk.VersionError.Bad("nope")),
        "\"nope\"".fromJson[JdkVersion] == Left(s"(${histos.jdk.VersionError.Bad("nope").message})"),
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2\"") == Right(jdk),
        JdkVersion.fromRelease("") == Left(ReleaseError.MissingKey),
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.2") == Left(ReleaseError.Unclosed),
        JdkVersion.fromRelease("JAVA_VERSION=\"nope\"") == Left(ReleaseError.BadVersion("nope")),
      )
    ,
    test("a total string codec keeps text from rejects, and a parsed codec keeps the typed error"):
      val suite      = SuiteName("demo.Suite")
      val clang      = ClangTarget.from("arm64-apple-darwin")
      val patterns   = SuitePattern.from("App*").toOption.toList
      val patternLaw = patterns match
        case pattern :: Nil => JsonLaw.rejectedText(pattern, "a b", SuitePattern.from, _.message)
        case _              => assertTrue(SuitePattern.from("App*") == Left(SuitePatternError.Empty))
      JsonLaw.rejectedText(core, "", ProjectName.from, _.message) &&
      JsonLaw.rejectedText(scala3, "3", ScalaVersion.from, _.message) &&
      JsonLaw.rejectedText(dep, "nope", Dep.from, _.message) &&
      JsonLaw.rejectedText(ship, "", histos.version.ShipName.from, _.message) &&
      JsonLaw.rejectedText(number, "01.2.3", ShipNumber.from, _.message) &&
      JsonLaw.rejectedText(param, "", histos.plugin.ParamName.from, _.message) &&
      JsonLaw.rejectedText(flag, "1flag", FlagName.from, _.message) &&
      JsonLaw.rejectedText(plugin, "", histos.plugin.PluginId.from, _.message) &&
      JsonLaw.rejectedText(taskId, "nope", histos.plugin.PluginTaskId.parse, _.message) &&
      JsonLaw.rejectedText(org, "", Organization.from, _.message) &&
      JsonLaw.rejectedText(WireSamples.main, "", histos.MainClass.from, _.message) &&
      JsonLaw.rejectedText(packed, "", PackMain.from, _.message) &&
      JsonLaw.rejectedText(jsVersion, "", JsVersion.from, _.message) &&
      JsonLaw.rejectedText(nativeVersion, "", NativeVersion.from, _.message) &&
      JsonLaw.rejectedText(revision, "", ToolRevision.from, _.message) &&
      JsonLaw.rejectedText(projectPath, "../x", histos.ProjectPath.from, _.message) &&
      JsonLaw.rejectedText(repoPath, "../x", histos.plugin.RepoPath.from, _.message) &&
      JsonLaw.rejectedText(entry, "", histos.plugin.EntryName.from, _.message) &&
      JsonLaw.rejectedText(digest, "abcd", ApiDigest.from, _.message) &&
      JsonLaw.rejectedText(hash, "abcd", ContentHash.from, _.message) &&
      JsonLaw.rejectedText(testMain, "", TestMain.from, _.message) &&
      JsonLaw.rejectedText(framework, "", FrameworkClass.from, _.message) &&
      JsonLaw.rejectedText(suite, "", SuiteName.from, _.message) &&
      patternLaw &&
      assertTrue(
        SuitePattern.from("App*") == SuitePattern.from("App*"),
        SuitePattern.from("") == Left(SuitePatternError.Empty),
        SuitePattern.from("a b") == Left(SuitePatternError.Blank("a b")),
        SuiteName.from("") == Left(SuiteNameError.Empty),
        JarClassifier.from("tests").map(_.toJson.fromJson[JarClassifier]).flatten == JarClassifier.from("tests"),
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
        "1".fromJson[CompilerMessage] == Left("(expected string)"),
        clang.map(target => target.toJson.fromJson[ClangTarget]) == clang.map(Right(_)),
        ClangTarget.from("") == Left(ClangText.NoTarget),
        ClangTarget.from("has space") == Left(ClangText.NoTarget),
        NativeSamples.version.toJson.fromJson[histos.native.ClangVersion] == Right(NativeSamples.version),
        "nope".toJson.fromJson[histos.native.ClangVersion] == Left(s"(${ClangText.BadVersion("nope").message})"),
        CiVersion(number).toJson == number.toJson,
        CiVersion(number).toJson.fromJson[CiVersion] == Right(CiVersion(number)),
        ShipNumber.from("01.2.3") == Left(ShipNumberError.NotAShip("01.2.3")),
        ActionId.from("") == Left(histos.schedule.ActionIdError.Empty),
        "".toJson.fromJson[ActionId] == Left("(an action id is not empty)"),
        ScheduleSamples.compile.toJson.fromJson[ActionId] == Right(ScheduleSamples.compile),
        InputName.from("") == Left(histos.schedule.InputNameError.Empty),
        "".toJson.fromJson[InputName] == Left("(an input name is not empty)"),
        ScheduleSamples.sources.toJson.fromJson[InputName] == Right(ScheduleSamples.sources),
        path.toJson.fromJson[java.nio.file.Path] == Right(path),
        "a\u0000b".toJson.fromJson[java.nio.file.Path] == Left("(invalid path a\u0000b)"),
        "1".fromJson[java.nio.file.Path] == Left("(expected string)"),
        library.toJson.fromJson[ProviderLibrary] == Right(library),
        """{"organization":"","name":"zio"}""".fromJson[ProviderLibrary] ==
          Left(s"(${ProviderLibraryError.Empty.message})"),
        "1".fromJson[ProviderLibrary] == Left("(expected '{' got '1')"),
        provider.toJson.fromJson[TestProvider] == Right(provider),
        PackMain.from("") == Left(PackMainError.Empty),
        "1flag".toJson.fromJson[FlagName] == Left(s"(${histos.plugin.FlagNameError.Bad("1flag").message})"),
        "nope".toJson.fromJson[histos.plugin.PluginTaskId] == Left(s"(${PluginTaskError.Shape("nope").message})"),
      )
    ,
    test("a run id rejects an empty id"):
      val ids = RunId.from("run-1").toOption.toList
      assertTrue(
        ids.map(id => id.toJson.fromJson[RunId]) == ids.map(Right(_)),
        ids.length == 1,
        "".toJson.fromJson[RunId] == Left(s"(${TaskError.EmptyId.message})"),
        "1".fromJson[RunId] == Left("(expected string)"),
        "\"".fromJson[RunId] == Left("Unexpected end of input"),
      ),
  )
end OpaqueLawSpec
