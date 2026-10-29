package histos.js

import histos.zinc.{
  CompileCall,
  CompileError,
  CompileOutput,
  CompileProblem,
  CompileRequest,
  JsLibraryMiss,
  JvmCompile,
  ScalaSeries,
}
import histos.{Dep, DepKind, JsEnv, JsVersion, Platform, ScalaVersion, ToolCatalog}
import histos.resolve.{LockedTools, ToolRoot, ToolRequest, ToolId, ToolRole, ToolVariant, Repositories, Transitivity}

import java.nio.file.Path

import zio.{IO, ZIO}

/** Scala.js compile for the Scala 3 compiler's own backend. The library replaces `scala-library` on the user classpath.
  */
object ScalaJs:
  /** The Scala 3 backend flag. The module's own flags stay on the request. */
  private val backend = "-scalajs"

  /** The jars the linker reads `.sjsir` from. The compiler uses the same list as its library classpath.
    */
  def libraries(
      version: ScalaVersion,
      revision: Option[JsVersion] = None,
  ): ZIO[LockedTools, CompileError, List[Path]] =
    library(version, revision)

  def compile(
      request: CompileRequest,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => (),
      revision: Option[JsVersion] = None,
  ): ZIO[LockedTools & histos.cache.ContentStore, CompileError, CompileOutput] =
    prepared(request, extra, hear, revision).flatMap: (updated, libraries, flags) =>
      JvmCompile.compileWith(updated, libraries, Nil, flags, hear)

  /** The Scala.js library and flags, ready for a cache check. This does not compile. */
  private[histos] def stage(
      request: CompileRequest,
      extra: List[String] = Nil,
      hear: CompileProblem => Unit = _ => (),
      revision: Option[JsVersion] = None,
  ): ZIO[LockedTools, CompileError, CompileCall] =
    prepared(request, extra, hear, revision).flatMap: (updated, libraries, flags) =>
      JvmCompile.stage(updated, Nil, flags, hear, Some(libraries))

  private def prepared(
      request: CompileRequest,
      extra: List[String],
      hear: CompileProblem => Unit,
      revision: Option[JsVersion],
  ): ZIO[LockedTools, CompileError, (CompileRequest, List[Path], List[String])] =
    if request.toolchain.platform != Platform.Js then
      ZIO.fail(CompileError.UnsupportedPlatform(request.toolchain.platform))
    else
      ScalaSeries.of(request.toolchain.version) match
        case Some(ScalaSeries.Scala3) =>
          input(request.toolchain.version, revision).map: selected =>
            (
              request.copy(compilationTools = request.compilationTools :+ selected),
              selected.artifacts.map(_.path),
              List(backend) ++ extra,
            )
        case _ =>
          ZIO.fail(CompileError.UnsupportedScala(request.toolchain.version))

  private def input(
      version: ScalaVersion,
      revision: Option[JsVersion],
  ): ZIO[LockedTools, CompileError, histos.resolve.ToolInput] =
    ZIO.serviceWithZIO[LockedTools]: tools =>
      val request = histos.js.JsRequests.libraries(
        version,
        revision.getOrElse(tools.catalog.js.revision),
        tools.repositories,
        tools.catalog,
      )
      tools.selected(request).mapError(CompileError.Resolved(_))

  private def library(
      version: ScalaVersion,
      revision: Option[JsVersion],
  ): ZIO[LockedTools, CompileError, List[Path]] =
    input(version, revision).map(_.artifacts.map(_.path))
end ScalaJs

object JsRequests:
  def libraries(
      version: ScalaVersion,
      revision: JsVersion,
      repos: Repositories,
      catalog: ToolCatalog,
  ): ToolRequest =
    val single = Transitivity.Single
    val roots  = List(
      ToolRoot.of(
        Dep.trusted(
          "org.scala-lang",
          "scala-library",
          version.value,
          DepKind.Java,
        ),
        version,
        Platform.Jvm,
        single,
        None,
      ),
      ToolRoot.of(
        Dep.trusted(
          "org.scala-lang",
          "scala3-library",
          version.value,
          DepKind.Binary,
        ),
        version,
        Platform.Jvm,
        single,
        None,
      ),
      ToolRoot.of(
        Dep.trusted(
          "org.scala-lang",
          "scala3-library",
          version.value,
          DepKind.Binary,
        ),
        version,
        Platform.Js,
        single,
        None,
      ),
      ToolRoot.of(
        Dep.trusted(
          "org.scala-js",
          "scalajs-library",
          revision.value,
          DepKind.Binary,
        ),
        catalog.js.bridgeScala,
        Platform.Jvm,
        single,
        None,
      ),
      ToolRoot.of(
        Dep.trusted(
          "org.scala-js",
          "scalajs-scalalib",
          version.value,
          DepKind.Binary,
        ),
        catalog.js.bridgeScala,
        Platform.Jvm,
        single,
        None,
      ),
      ToolRoot.of(
        Dep.trusted(
          "org.scala-js",
          "scalajs-javalib",
          revision.value,
          DepKind.Java,
        ),
        version,
        Platform.Jvm,
        single,
        None,
      ),
    )
    ToolRequest(
      ToolId(ToolRole.JsLibraries, version, ToolVariant.Js(revision)),
      roots,
      repos,
    )

  def compiler(env: JsEnv, catalog: ToolCatalog): ScalaVersion = env match
    case JsEnv.Firefox            => catalog.js.browserScala
    case JsEnv.Node | JsEnv.Jsdom => catalog.js.bridgeScala

  def environment(
      env: JsEnv,
      catalog: ToolCatalog,
      repos: Repositories,
  ): ToolRequest =
    val version                        = compiler(env, catalog)
    val (org, name, revision, variant) = env match
      case JsEnv.Node =>
        (
          "org.scala-js",
          "scalajs-env-nodejs",
          catalog.js.node,
          ToolVariant.Node,
        )
      case JsEnv.Jsdom =>
        (
          "org.scala-js",
          "scalajs-env-jsdom-nodejs",
          catalog.js.jsdom,
          ToolVariant.Jsdom,
        )
      case JsEnv.Firefox =>
        (
          "rocks.earlyeffect",
          "chekhov-jsenv",
          catalog.js.chekhov,
          ToolVariant.Firefox,
        )
    ToolRequest(
      ToolId(ToolRole.JsEnvironment, version, variant),
      List(
        ToolRoot.of(
          Dep.trusted(org, name, revision.value, DepKind.Binary),
          version,
          Platform.Jvm,
          Transitivity.Closure,
          None,
        )
      ),
      repos,
    )

  def linker(
      revision: JsVersion,
      version: ScalaVersion,
      repos: Repositories,
  ): ToolRequest =
    ToolRequest(
      ToolId(ToolRole.JsLinker, version, ToolVariant.Js(revision)),
      List(
        ToolRoot.of(
          Dep.trusted(
            "org.scala-js",
            "scalajs-linker",
            revision.value,
            DepKind.Binary,
          ),
          version,
          Platform.Jvm,
          Transitivity.Closure,
          None,
        )
      ),
      repos,
    )

  def suite(
      revision: JsVersion,
      env: JsEnv,
      catalog: ToolCatalog,
      repos: Repositories,
  ): ToolRequest =
    val version = compiler(env, catalog)
    val roots   = environment(env, catalog, repos).roots ++ List(
      "scalajs-linker",
      "scalajs-sbt-test-adapter",
    ).map(name =>
      ToolRoot.of(
        Dep.trusted("org.scala-js", name, revision.value, DepKind.Binary),
        version,
        Platform.Jvm,
        Transitivity.Closure,
        None,
      )
    )
    // Environment roots participate in drift, so the same linker revision can serve different environments.
    ToolRequest(
      ToolId(ToolRole.JsSuite, version, ToolVariant.Suite(revision, env)),
      roots,
      repos,
    )

  def testBridge(
      revision: JsVersion,
      catalog: ToolCatalog,
      repos: Repositories,
  ): ToolRequest =
    val version = catalog.js.bridgeScala
    ToolRequest(
      ToolId(ToolRole.JsTestBridge, version, ToolVariant.Js(revision)),
      List(
        ToolRoot.of(
          Dep.trusted(
            "org.scala-js",
            "scalajs-test-bridge",
            revision.value,
            DepKind.Binary,
          ),
          version,
          Platform.Jvm,
          Transitivity.Single,
          None,
        )
      ),
      repos,
    )
end JsRequests
