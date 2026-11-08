package histos

import histos.central.{MetadataFault, PublishFault}
import histos.node.{NodeFileFault, NodeSource}
import histos.process.GroupOp
import histos.preview.PreviewError
import histos.report.WireJson
import histos.resolve.{RepositoryIo, ToolRootFault}
import histos.zinc.{ClasspathBan, CompilerPart, JsLibraryMiss}
import histos.testrun.{SuitePattern, SuitePatternError}
import histos.workspace.IoFault

import zio.json.*
import zio.test.*

import WireJson.given

object CodecLawSpec extends ZIOSpecDefault:
  import CodecSamples.*

  def spec = suite("codec laws")(
    JsonLaw.enumLaw("every resolve error round-trips, and a drift keeps what changed", resolveErrors),
    JsonLaw.enumLaw("every lock error round-trips, and a wrong kind is rejected", lockErrors),
    JsonLaw.enumLaw("every graph compile error round-trips, and a wrong kind is rejected", graphErrors),
    JsonLaw.enumLaw("every compile error round-trips, and a wrong kind is rejected", compileErrors),
    JsonLaw.enumLaw("every toolchain miss round-trips, and a wrong kind is rejected", toolchainMisses),
    JsonLaw.enumLaw("every native fetch round-trips, and a wrong kind is rejected", nativeFetches),
    JsonLaw.enumLaw("every build error round-trips, and a missing cell keeps its version", buildErrors),
    JsonLaw.enumLaw("every ci fault round-trips, and a wrong kind is rejected", ciFaults),
    JsonLaw.enumLaw("every load error round-trips, and a wrong kind is rejected", loadErrors),
    JsonLaw.enumLaw("every plugin load error round-trips, and a wrong kind is rejected", pluginLoads),
    JsonLaw.enumLaw("every plugin run error round-trips, and a wrong kind is rejected", pluginRuns),
    JsonLaw.enumLaw("every plugin task error round-trips, and a wrong kind is rejected", pluginTasks),
    JsonLaw.values("a plugin task error keeps a bad plugin id and a bad task name", pluginTaskExtras),
    JsonLaw.enumLaw("every plugin io round-trips, and a wrong kind is rejected", pluginIos),
    JsonLaw.enumLaw("every plugin fault round-trips, and a scheduled fault stays scheduled", pluginFaults),
    JsonLaw.enumLaw("every schedule error round-trips, and a wrong kind is rejected", scheduleErrors),
    moduleTestLaw,
    JsonLaw.enumLaw("every program error round-trips, and an absent name keeps its neighbors", programErrors),
    JsonLaw.enumLaw("every local publish error round-trips, and a wrong kind is rejected", localPublishes),
    test("every run miss round-trips inside the preview that carries it"):
      val wrapped = runMisses.map(PreviewError.Run(_))
      assertTrue(
        EnumLaw.covered(runMisses),
        wrapped.map(miss => miss.toJson.fromJson[PreviewError]) == wrapped.map(Right(_)),
      )
    ,
    JsonLaw.enumLaw("every preview error round-trips, and a wrong kind is rejected", previewErrors),
    JsonLaw.enumLaw("every ci error round-trips, and an unknown project keeps its neighbors", ciErrors),
    JsonLaw.enumLaw("every script error round-trips, and a wrong kind is rejected", scriptErrors),
    JsonLaw.enumLaw("every assemble error round-trips, and a wrong kind is rejected", assembleErrors),
    JsonLaw.enumLaw("every launcher error round-trips, and a wrong kind is rejected", launcherErrors),
    JsonLaw.enumLaw("every publish error round-trips, and a wrong kind is rejected", publishErrors),
    JsonLaw.enumLaw("every transport fault round-trips, and a wrong kind is rejected", transports),
    JsonLaw.enumLaw("every jar error round-trips, and a wrong kind is rejected", jarErrors),
    JsonLaw.enumLaw("every jar io round-trips, and a wrong kind is rejected", jarIos),
    JsonLaw.enumLaw("every assembly error round-trips, and a wrong kind is rejected", assemblyErrors),
    JsonLaw.enumLaw("every lazy val error round-trips, and a wrong kind is rejected", lazyErrors),
    JsonLaw.enumLaw("every lazy val frame round-trips, and a wrong kind is rejected", lazyFrames),
    JsonLaw.enumLaw("every child error round-trips, and a wrong kind is rejected", childErrors),
    JsonLaw.enumLaw("every toolchain error round-trips, and a wrong kind is rejected", toolchainErrors),
    JsonLaw.enumLaw("every survey error round-trips, and a wrong kind is rejected", surveyErrors),
    JsonLaw.enumLaw("every remote error round-trips, and a wrong kind is rejected", remoteErrors),
    JsonLaw.enumLaw("every cache exchange round-trips, and a wrong kind is rejected", exchanges),
    JsonLaw.enumLaw("every cache error round-trips, and a wrong kind is rejected", cacheErrors),
    JsonLaw.enumLaw("every namespace error round-trips, and a wrong kind is rejected", namespaceErrors),
    JsonLaw.enumLaw("every node error round-trips, and a wrong kind is rejected", nodeErrors),
    JsonLaw.enumLaw("every node miss round-trips, and a wrong kind is rejected", nodeMisses),
    JsonLaw.enumLaw("every node version error round-trips, and a wrong kind is rejected", nodeVersionErrors),
    JsonLaw.enumLaw("every node reject round-trips, and a wrong kind is rejected", nodeRejects),
    JsonLaw.enumLaw("every node override round-trips, and a wrong kind is rejected", nodeOverrideErrors),
    JsonLaw.enumLaw("every node path error round-trips, and a wrong kind is rejected", nodePathErrors),
    JsonLaw.enumLaw("every runtime location error round-trips, and a wrong kind is rejected", locationErrors),
    JsonLaw.enumLaw("every jdk error round-trips, and a wrong kind is rejected", jdkErrors),
    JsonLaw.enumLaw("every release error round-trips, and a wrong kind is rejected", releaseErrors),
    JsonLaw.enumLaw("every jdk reject round-trips, and a wrong kind is rejected", rejects),
    JsonLaw.enumLaw("every toolchain override round-trips, and a wrong kind is rejected", overrideErrors),
    JsonLaw.enumLaw("every clang miss round-trips, and a wrong kind is rejected", clangMisses),
    JsonLaw.enumLaw("every clang text round-trips, and a wrong kind is rejected", clangTexts),
    JsonLaw.enumLaw("every native origin round-trips, and a wrong kind is rejected", nativeOrigins),
    JsonLaw.enumLaw("every native error round-trips, and a wrong kind is rejected", nativeErrors),
    JsonLaw.enumLaw("every native suite error round-trips, and a wrong kind is rejected", nativeSuiteErrors),
    JsonLaw.enumLaw("every link error round-trips, and a wrong kind is rejected", linkErrors),
    JsonLaw.enumLaw("every class shape error round-trips, and a wrong kind is rejected", shapeErrors),
    JsonLaw.enumLaw("every js link error round-trips, and a wrong kind is rejected", jsLinkErrors),
    JsonLaw.enumLaw("every js run error round-trips, and a wrong kind is rejected", jsRunErrors),
    JsonLaw.enumLaw("every js suite error round-trips, and a wrong kind is rejected", jsSuiteErrors),
    JsonLaw.enumLaw("every platform tool error round-trips, and a wrong kind is rejected", platformTools),
    JsonLaw.enumLaw("every select error round-trips, and an unknown project keeps its neighbors", selectErrors),
    JsonLaw.enumLaw("every watch error round-trips, and a wrong kind is rejected", watchErrors),
    JsonLaw.enumLaw("every version error round-trips, and a wrong kind is rejected", versionErrors),
    JsonLaw.enumLaw("every session error round-trips, and a wrong kind is rejected", sessionErrors),
    JsonLaw.enumLaw("every registry error round-trips, and a wrong kind is rejected", registryErrors),
    JsonLaw.enumLaw("every refusal round-trips, and a wrong kind is rejected", refusals),
    JsonLaw.enumLaw("every task error round-trips, and a wrong kind is rejected", taskErrors),
    JsonLaw.enumLaw("every commit sha error round-trips, and a wrong kind is rejected", commitErrors),
    JsonLaw.enumLaw("every branch error round-trips, and a wrong kind is rejected", branchErrors),
    JsonLaw.enumLaw("every pull request error round-trips, and a wrong kind is rejected", pullErrors),
    JsonLaw.enumLaw("every project name error round-trips, and a wrong kind is rejected", nameErrors),
    JsonLaw.enumLaw("every scala version error round-trips, and a wrong kind is rejected", scalaErrors),
    JsonLaw.enumLaw("every ship name error round-trips, and a wrong kind is rejected", shipNameErrors),
    JsonLaw.enumLaw("every ship number error round-trips, and a wrong kind is rejected", shipNumberErrors),
    JsonLaw.enumLaw("every dependency error round-trips, and a wrong kind is rejected", depErrors),
    JsonLaw.enumLaw("every parameter error round-trips, and a wrong kind is rejected", paramErrors),
    JsonLaw.enumLaw("every flag error round-trips, and a wrong kind is rejected", flagErrors),
    JsonLaw.enumLaw("every sha256 error round-trips, and a wrong kind is rejected", shaErrors),
    JsonLaw.enumLaw("every suite pattern error round-trips, and a wrong kind is rejected", patternErrors),
    JsonLaw.enumLaw("every classifier error round-trips, and a wrong kind is rejected", classifierErrors),
    JsonLaw.enumLaw("every repository url error round-trips, and a wrong kind is rejected", repoUrlErrors),
    JsonLaw.enumLaw("every packed main error round-trips, and a wrong kind is rejected", packMainErrors),
    JsonLaw.enumLaw("every tag relation error round-trips, and a wrong kind is rejected", tagErrors),
    JsonLaw.enumLaw("every suite name error round-trips, and a wrong kind is rejected", suiteNameErrors),
    JsonLaw.enumLaw("every dev name error round-trips, and a wrong kind is rejected", devNameErrors),
    JsonLaw.enumLaw("every snapshot revision error round-trips, and a wrong kind is rejected", snapshotErrors),
    JsonLaw.enumLaw("every platform error round-trips, and a wrong kind is rejected", platformErrors),
    JsonLaw.enumLaw("every stamp error round-trips, and a wrong kind is rejected", stampErrors),
    JsonLaw.enumLaw("every quick error round-trips, and a wrong kind is rejected", quickErrors),
    JsonLaw.enumLaw("every io fault round-trips, and a wrong kind is rejected", IoFault.values.toList),
    JsonLaw.enumLaw("every classpath ban round-trips, and a wrong kind is rejected", ClasspathBan.values.toList),
    JsonLaw.enumLaw("every compiler part round-trips, and a wrong kind is rejected", CompilerPart.values.toList),
    JsonLaw.enumLaw("every js library miss round-trips, and a wrong kind is rejected", JsLibraryMiss.values.toList),
    JsonLaw.enumLaw("every tool root fault round-trips, and a wrong kind is rejected", ToolRootFault.values.toList),
    JsonLaw.enumLaw("every repository io round-trips, and a wrong kind is rejected", RepositoryIo.values.toList),
    JsonLaw.enumLaw("every process group op round-trips, and a wrong kind is rejected", GroupOp.values.toList),
    JsonLaw.enumLaw("every node source round-trips, and a wrong kind is rejected", NodeSource.values.toList),
    JsonLaw.enumLaw("every node file fault round-trips, and a wrong kind is rejected", NodeFileFault.values.toList),
    JsonLaw.enumLaw("every publish fault round-trips, and a wrong kind is rejected", PublishFault.values.toList),
    JsonLaw.enumLaw("every metadata fault round-trips, and a wrong kind is rejected", MetadataFault.values.toList),
    JsonLaw.enumLaw("every runtime identity round-trips, and a wrong kind is rejected", List(identity)),
    JsonLaw.values("a tool root keeps its origin", List(toolRoot)),
    JsonLaw.values("a plugin coordinate keeps a published dep and a source path", coordinates),
    JsonLaw.values("a jdk survey keeps its home", List(jdkSurvey)),
    JsonLaw.values("a node survey keeps its binary", List(nodeSurvey)),
    JsonLaw.values("a reached survey keeps its node", List(reached)),
    test("a native declaration names the major or the projects"):
      import WireSamples.{api, clang, core}
      assertTrue(
        declarations.map(_.message) == List(
          "",
          s"clang ${clang.value}",
          s"project '${core.value}' targets Native and does not name a clang major",
          s"projects '${core.value}' and '${api.value}' name different clang majors",
        ),
        EnumLaw.covered(declarations),
      )
    ,
    test("an input value says text or hash"):
      import WireSamples.sha
      assertTrue(
        inputs.map(_.canonical) == List("text src", s"hash ${sha.value}")
      ),
  )

  private val moduleTestLaw: Spec[Any, Nothing] =
    SuitePattern.from("App*").toOption.toList match
      case pattern :: Nil =>
        JsonLaw.enumLaw(
          "every module test error round-trips, and a pattern keeps its text",
          moduleTests(pattern),
        )
      case _ =>
        test("every module test error round-trips, and a pattern keeps its text"):
          assertTrue(SuitePattern.from("App*") == Left(SuitePatternError.Empty))
end CodecLawSpec
