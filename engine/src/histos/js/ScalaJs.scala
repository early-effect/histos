package histos.js

import histos.zinc.{CompileError, CompileOutput, CompileRequest, JsLibraryMiss, JvmCompile, ScalaSeries}
import histos.{Platform, ScalaVersion}

import java.nio.file.Path

import zio.{IO, ZIO}

/** Scala.js compile for the Scala 3 compiler's own backend. The library replaces `scala-library` on the user classpath. */
object ScalaJs:
  private val Javalib: String = "1.22.0"

  /** The jars the linker reads `.sjsir` from. The compiler uses the same list as its library classpath. */
  def libraries(version: ScalaVersion): IO[CompileError, List[Path]] =
    library(version)

  def compile(request: CompileRequest, hear: String => Unit = _ => ()): IO[CompileError, CompileOutput] =
    if request.toolchain.platform != Platform.Js then ZIO.fail(CompileError.UnsupportedPlatform(request.toolchain.platform))
    else
      ScalaSeries.of(request.toolchain.version) match
        case Some(ScalaSeries.Scala3) =>
          library(request.toolchain.version).flatMap: jars =>
            JvmCompile.compileWith(request, jars, Nil, List("-scalajs"), hear)
        case _ => ZIO.fail(CompileError.UnsupportedScala(request.toolchain.version))

  private def library(version: ScalaVersion): IO[CompileError, List[Path]] =
    ZIO.attemptBlockingInterrupt(fetch(version)).mapError(_ => CompileError.JsLibrary(JsLibraryMiss.Unavailable)).flatMap(ZIO.fromEither)

  /** The JS standard library only. The published poms also point at the JVM library, which crashes `-scalajs`. */
  private def fetch(version: ScalaVersion): Either[CompileError, List[Path]] =
    val wanted = List(
      ("org.scala-lang", "scala-library", version.value),
      ("org.scala-lang", "scala3-library_3", version.value),
      ("org.scala-lang", "scala3-library_sjs1_3", version.value),
      ("org.scala-js", "scalajs-library_2.13", Javalib),
      ("org.scala-js", "scalajs-scalalib_2.13", version.value),
      ("org.scala-js", "scalajs-javalib", Javalib),
    )
    val dependencies = wanted.map: (org, name, revision) =>
      coursier.Dependency(
        coursier.Module(coursier.Organization(org), coursier.ModuleName(name)),
        coursier.version.VersionConstraint(revision),
      ).copy(transitive = false)
    val got = coursier.Fetch(coursier.cache.FileCache())(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .withRepositories(Seq(coursier.Repositories.central))
      .addDependencies(dependencies*)
      .eitherResult()
    got match
      case Left(_) => Left(CompileError.JsLibrary(JsLibraryMiss.Unavailable))
      case Right(result) =>
        val jars = result.detailedArtifacts0.toList.flatMap: (_, publication, _, file) =>
          val classified = publication match
            case Right(published) => published.classifier.nonEmpty
            case Left(variant)    => variant.classifier.nonEmpty
          if file.getName.endsWith(".jar") && !classified then List(file.toPath) else Nil
        val ordered = wanted.zipWithIndex.flatMap:
          case ((_, name, _), index) =>
            jars.find(path => Option(path.getFileName).exists(_.toString.startsWith(name))).map(index -> _)
        if ordered.length != wanted.length then Left(CompileError.JsLibrary(JsLibraryMiss.Empty))
        else Right(ordered.sortBy(_._1).map(_._2))
