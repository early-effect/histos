package histos.zinc

import histos.*
import histos.jdk.Sha256Digest
import histos.js.JsRequests
import histos.resolve.{ClasspathScope, DependencyLock, LockedArtifact, LockedCell, Repository}

import java.nio.charset.StandardCharsets

import zio.{Chunk, ZIO}
import zio.test.*

object JsClasspathSpec extends ZIOSpecDefault:
  private val version  = scala"3.10.1"
  private val revision = JsVersion("1.23.0")

  def spec = suite("requested JS runtime classpath")(
    test("library roots select the requested versions and retain the published 2.13 suffix") {
      val roots = JsRequests.libraryRoots(version, revision)
      assertTrue(
        roots.map(root => root.name -> root.revision) == List(
          "scala-library"         -> "3.10.1",
          "scala3-library_3"      -> "3.10.1",
          "scala3-library_sjs1_3" -> "3.10.1",
          "scalajs-library_2.13"  -> "1.23.0",
          "scalajs-scalalib_2.13" -> "3.10.1",
          "scalajs-javalib"       -> "1.23.0",
        )
      )
    },
    test("dependency runtime duplicates contribute neither jars nor compile hashes") {
      val defined   = module("app", Js)(version)
      val roots     = JsRequests.libraryRoots(version, revision)
      val libraries = roots.flatMap: root =>
        val previous = if root.revision == version.value then "3.9.0" else "1.22.0"
        List(
          artifact(root.organization, root.name, previous),
          artifact(root.organization, root.name, root.revision),
        )
      val dependency = artifact("dev.zio", "zio_sjs1_3", "2.1.27")
      val unrelated  = artifact("example", "scalajs-library_2.13", "1.0.0")
      val artifacts  = libraries ++ List(dependency, unrelated)
      ZIO
        .fromEither(Projects(defined))
        .map: graph =>
          val modules  = graph.projects.collect { case module: Module => module }
          val retained = artifacts.filterNot(artifact =>
            ScalacClasspath.toolLibrary(artifact.organization, artifact.name, version, Platform.Js)
          )
          assertTrue(
            retained == List(dependency, unrelated),
            modules.size == 1,
            modules.forall: module =>
              val lock = DependencyLock(
                List(
                  LockedCell(
                    module.name,
                    version,
                    Platform.Js,
                    Nil,
                    artifacts,
                    Nil,
                  )
                )
              )
              ScalacClasspath
                .lockHashes(lock, module, Nil, version, Platform.Js, ClasspathScope.Compile)
                .contains(List(dependency.sha256, unrelated.sha256)),
          )
    },
  )

  private def artifact(organization: String, name: String, revision: String): LockedArtifact =
    LockedArtifact(
      organization,
      name,
      revision,
      Sha256Digest.of(Chunk.fromArray(s"$organization:$name:$revision".getBytes(StandardCharsets.UTF_8))),
      Repository.Central,
    )
end JsClasspathSpec
