package histos

import histos.preview.PreviewError
import histos.report.WireJson

import zio.json.*
import zio.test.*

import WireJson.given

object CodecLawSpec extends ZIOSpecDefault:
  import CodecSamples.*

  def spec = suite("codec laws")(
    JsonLaw.enumLaw("every resolve error round-trips, and a drift keeps what changed", resolveErrors),
    JsonLaw.enumLaw("every lock error round-trips", lockErrors),
    JsonLaw.enumLaw("every graph compile error round-trips", graphErrors),
    JsonLaw.enumLaw("every compile error round-trips", compileErrors),
    JsonLaw.enumLaw("every toolchain miss round-trips", toolchainMisses),
    JsonLaw.enumLaw("every native fetch round-trips", nativeFetches),
    JsonLaw.enumLaw("every build error round-trips, and a missing cell keeps its version", buildErrors),
    JsonLaw.enumLaw("every plugin graph fault round-trips, and a missing cell keeps its version", pluginGraphFaults),
    JsonLaw.enumLaw("every ci fault round-trips", ciFaults),
    JsonLaw.enumLaw("every load error round-trips", loadErrors),
    JsonLaw.enumLaw("every plugin load error round-trips", pluginLoads),
    JsonLaw.enumLaw("every plugin run error round-trips", pluginRuns),
    JsonLaw.enumLaw("every plugin task error round-trips", pluginTasks),
    JsonLaw.values("a plugin task error keeps a bad plugin id and a bad task name", pluginTaskExtras),
    JsonLaw.enumLaw("every plugin io round-trips", pluginIos),
    JsonLaw.enumLaw("every plugin fault round-trips, and a scheduled fault stays scheduled", pluginFaults),
    JsonLaw.enumLaw("every schedule error round-trips", scheduleErrors),
    JsonLaw.enumLaw("every module test error round-trips, and a pattern keeps its text", moduleTests),
    JsonLaw.enumLaw("every program error round-trips, and an absent name keeps its neighbors", programErrors),
    JsonLaw.enumLaw("every local publish error round-trips", localPublishes),
    test("every run miss round-trips inside the preview that carries it"):
      val wrapped = runMisses.map(PreviewError.Run(_))
      assertTrue(
        EnumLaw.covered(runMisses),
        wrapped.map(miss => miss.toJson.fromJson[PreviewError]) == wrapped.map(Right(_)),
      )
    ,
    JsonLaw.enumLaw("every preview error round-trips", previewErrors),
    JsonLaw.enumLaw("every ci error round-trips, and an unknown project keeps its neighbors", ciErrors),
    JsonLaw.enumLaw("every script error round-trips", scriptErrors),
    JsonLaw.enumLaw("every assemble error round-trips", assembleErrors),
    JsonLaw.enumLaw("every launcher error round-trips", launcherErrors),
    JsonLaw.enumLaw("every publish error round-trips", publishErrors),
    JsonLaw.enumLaw("every transport fault round-trips", transports),
    JsonLaw.enumLaw("every jar error round-trips", jarErrors),
    JsonLaw.enumLaw("every jar io round-trips", jarIos),
    JsonLaw.enumLaw("every assembly error round-trips", assemblyErrors),
    JsonLaw.enumLaw("every lazy val error round-trips", lazyErrors),
    JsonLaw.enumLaw("every lazy val frame round-trips", lazyFrames),
    JsonLaw.enumLaw("every child error round-trips", childErrors),
    JsonLaw.enumLaw("every toolchain error round-trips", toolchainErrors),
    JsonLaw.enumLaw("every survey error round-trips", surveyErrors),
    JsonLaw.enumLaw("every remote error round-trips", remoteErrors),
    JsonLaw.enumLaw("every cache exchange round-trips", exchanges),
    JsonLaw.enumLaw("every cache error round-trips", cacheErrors),
    JsonLaw.enumLaw("every namespace error round-trips", namespaceErrors),
    JsonLaw.enumLaw("every node error round-trips", nodeErrors),
    JsonLaw.enumLaw("every node miss round-trips", nodeMisses),
    JsonLaw.enumLaw("every node version error round-trips", nodeVersionErrors),
    JsonLaw.enumLaw("every node reject round-trips", nodeRejects),
    JsonLaw.enumLaw("every node override round-trips", nodeOverrideErrors),
    JsonLaw.enumLaw("every node path error round-trips", nodePathErrors),
    JsonLaw.enumLaw("every runtime location error round-trips", locationErrors),
    JsonLaw.enumLaw("every jdk error round-trips", jdkErrors),
    JsonLaw.enumLaw("every release error round-trips", releaseErrors),
    JsonLaw.enumLaw("every jdk reject round-trips", rejects),
    JsonLaw.enumLaw("every toolchain override round-trips", overrideErrors),
    JsonLaw.enumLaw("every clang miss round-trips", clangMisses),
    JsonLaw.enumLaw("every clang text round-trips", clangTexts),
    JsonLaw.enumLaw("every native origin round-trips", nativeOrigins),
    JsonLaw.enumLaw("every native error round-trips", nativeErrors),
    JsonLaw.enumLaw("every native suite error round-trips", nativeSuiteErrors),
    JsonLaw.enumLaw("every link error round-trips", linkErrors),
    JsonLaw.enumLaw("every class shape error round-trips", shapeErrors),
    JsonLaw.enumLaw("every js link error round-trips", jsLinkErrors),
    JsonLaw.enumLaw("every js run error round-trips", jsRunErrors),
    JsonLaw.enumLaw("every js suite error round-trips", jsSuiteErrors),
    JsonLaw.enumLaw("every platform tool error round-trips", platformTools),
    JsonLaw.enumLaw("every select error round-trips, and an unknown project keeps its neighbors", selectErrors),
    JsonLaw.enumLaw("every watch error round-trips", watchErrors),
    JsonLaw.enumLaw("every version error round-trips", versionErrors),
    JsonLaw.enumLaw("every session error round-trips", sessionErrors),
    JsonLaw.enumLaw("every registry error round-trips", registryErrors),
    JsonLaw.enumLaw("every refusal round-trips", refusals),
    JsonLaw.enumLaw("every task error round-trips", taskErrors),
    JsonLaw.enumLaw("every commit sha error round-trips", commitErrors),
    JsonLaw.enumLaw("every branch error round-trips", branchErrors),
    JsonLaw.enumLaw("every pull request error round-trips", pullErrors),
    JsonLaw.enumLaw("every project name error round-trips", nameErrors),
    JsonLaw.enumLaw("every scala version error round-trips", scalaErrors),
    JsonLaw.enumLaw("every ship name error round-trips", shipNameErrors),
    JsonLaw.enumLaw("every ship number error round-trips", shipNumberErrors),
    JsonLaw.enumLaw("every dependency error round-trips", depErrors),
    JsonLaw.enumLaw("every parameter error round-trips", paramErrors),
    JsonLaw.enumLaw("every flag error round-trips", flagErrors),
    JsonLaw.enumLaw("every sha256 error round-trips", shaErrors),
    JsonLaw.enumLaw("every suite pattern error round-trips", patternErrors),
    JsonLaw.enumLaw("every classifier error round-trips", classifierErrors),
    JsonLaw.enumLaw("every repository url error round-trips", repoUrlErrors),
    JsonLaw.enumLaw("every packed main error round-trips", packMainErrors),
    JsonLaw.enumLaw("every tag relation error round-trips", tagErrors),
    JsonLaw.enumLaw("every suite name error round-trips", suiteNameErrors),
    JsonLaw.enumLaw("every dev name error round-trips", devNameErrors),
    JsonLaw.enumLaw("every snapshot revision error round-trips", snapshotErrors),
    JsonLaw.enumLaw("every platform error round-trips", platformErrors),
    JsonLaw.enumLaw("every stamp error round-trips", stampErrors),
    JsonLaw.enumLaw("every quick error round-trips", quickErrors),
    JsonLaw.values("a tool root keeps its origin", List(toolRoot)),
    JsonLaw.values("a plugin coordinate keeps a published dep and a source path", coordinates),
    JsonLaw.values("a reached survey keeps its jdk home, its runtime identity, and its node binary", List(reached)),
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
end CodecLawSpec
