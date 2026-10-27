package histos.bsp

import histos.{Platform, PlatformError, ScalaVersion, ScalaVersionError}

import java.nio.file.Path

import zio.json.*
import zio.json.ast.Json
import zio.test.*

object BspTargetSpec extends ZIOSpecDefault:
  private val repo    = Path.of("/repo")
  private val version = ScalaVersion("3.10.0")
  private val modules = Set("app")

  def spec = suite("bsp target")(
    test("a target uri is a version, a platform, and a scope"):
      val main        = Bsp.mainUri(repo, "app", version, Platform.Jvm)
      val suite       = Bsp.testUri(repo, "app", version, Platform.Js)
      val badVersion  = main.replace(version.value, "nope")
      val badPlatform = main.replace("platform=jvm", "platform=nope")
      val badScope    = main.replace("scope=main", "scope=other")
      val missing     = main.replace(s"scala=${version.value}&", "")
      val unknown     = Bsp.mainUri(repo, "missing", version, Platform.Jvm)
      val unknownBad  = unknown.replace(version.value, "nope")
      val both        = badVersion.replace("platform=jvm", "platform=nope")
      assertTrue(
        Bsp.target(repo, main, modules) == Bsp.TargetRef.Main("app", version, Platform.Jvm),
        Bsp.target(repo, suite, modules) == Bsp.TargetRef.Test("app", version, Platform.Js),
        Bsp.target(repo, Bsp.buildUri(repo), Set.empty) == Bsp.TargetRef.Build,
        Bsp.target(repo, badVersion, modules) == Bsp.TargetRef.BadVersion(ScalaVersionError.NotFull("nope")),
        Bsp.target(repo, main.replace(version.value, ""), modules) == Bsp.TargetRef.BadVersion(ScalaVersionError.Empty),
        Bsp.target(repo, badPlatform, modules) == Bsp.TargetRef.BadPlatform(PlatformError.Unknown("nope")),
        Bsp.target(repo, badScope, modules) == Bsp.TargetRef.BadScope(Bsp.ScopeFault.Unknown("other")),
        Bsp.target(repo, missing, modules) == Bsp.TargetRef.Unknown,
        Bsp.target(repo, unknown, modules) == Bsp.TargetRef.Unknown,
        Bsp.target(repo, unknownBad, modules) == Bsp.TargetRef.Unknown,
        Bsp.target(repo, both, modules) == Bsp.TargetRef.BadVersion(ScalaVersionError.NotFull("nope")),
        badVersion != main,
        missing != main,
      )
    ,
    test("a main class field fault is rendered at the codec"):
      val required = Rpc.ScalaMainClass.FieldFault.Required("class")
      val strings  = Rpc.ScalaMainClass.FieldFault.NotStrings("arguments")
      val obj      = Rpc.ScalaMainClass.FieldFault.NotObject
      val missing  = Json.Obj().as[Rpc.ScalaMainClass]
      val numbers  = Json
        .Obj(
          "class"      -> Json.Str("demo.Main"),
          "arguments"  -> Json.Arr(Json.Num(1)),
          "jvmOptions" -> Json.Arr(),
        )
        .as[Rpc.ScalaMainClass]
      val text = Json.Str("demo").as[Rpc.ScalaMainClass]
      val good = Rpc.ScalaMainClass("demo.Main", List("a"), List("-X"))
      assertTrue(
        missing == Left(rendered(required.message)),
        numbers == Left(rendered(strings.message)),
        text == Left(rendered(obj.message)),
        good.toJson.fromJson[Rpc.ScalaMainClass] == Right(good),
      ),
  )

  private def rendered(message: String): String =
    s"($message)"
end BspTargetSpec
