package histos.plugin

import histos.ProjectPath
import histos.suite.HistosSpec

import scala.compiletime.testing.typeChecks

import zio.test.*

object FilePathSpec extends HistosSpec:
  private val safe = List(
    "vendor/escape-string-regexp@5.0.0.js",
    "assets/@scope/client+v1.js",
    "notes/front page.html",
    "données/日本語.txt",
    "123/.config/$Entry.class",
  )
  private val invalid = List(
    "",
    "/absolute",
    "a//b",
    "a/./b",
    "a/../b",
    "a/b/",
    "a\\b",
    "C:/b",
    "C:b",
    "prefix:entry",
    "a/\u0000b",
    "a/\nb",
    "a/\tb",
    "a/\u007fb",
  )

  private val tests = suite("safe file paths")(
    test("repository and sealed archive paths accept safe filenames without identifier restrictions") {
      val repository = List(
        RepoPath("vendor/escape-string-regexp@5.0.0.js"),
        RepoPath("notes/front page.html"),
        RepoPath("données/日本語.txt"),
      )
      val archive = List(
        ArtifactPath("vendor/escape-string-regexp@5.0.0.js"),
        ArtifactPath("notes/front page.html"),
        ArtifactPath("données/日本語.txt"),
      )
      assertTrue(
        repository.forall(path => RepoPath.from(path.value) == Right(path)),
        archive.forall(path => ArtifactPath.from(path.value) == Right(path)),
        repository.map(_.value) == archive.map(_.value),
        safe.forall(raw => RepoPath.from(raw).map(_.value) == Right(raw)),
        safe.forall(raw => ArtifactPath.from(raw).map(_.value) == Right(raw)),
        typeChecks("histos.plugin.RepoPath(\"vendor/escape-string-regexp@5.0.0.js\")"),
        typeChecks("histos.plugin.RepoPath(\"notes/front page.html\")"),
        typeChecks("histos.plugin.RepoPath(\"données/日本語.txt\")"),
        typeChecks("histos.plugin.ArtifactPath(\"assets/@scope/client+v1.js\")"),
        typeChecks("histos.plugin.ArtifactPath(\"notes/front page.html\")"),
        typeChecks("histos.plugin.ArtifactPath(\"données/日本語.txt\")"),
      )
    },
    test("runtime parsers and literal macros refuse escape and control paths") {
      assertTrue(
        invalid.forall(raw => RepoPath.from(raw).isLeft),
        invalid.forall(raw => ArtifactPath.from(raw).isLeft),
        !typeChecks("histos.plugin.RepoPath(\"a/../b\")"),
        !typeChecks("histos.plugin.RepoPath(\"C:/b\")"),
        !typeChecks("histos.plugin.RepoPath(\"a/\\\\b\")"),
        !typeChecks("histos.plugin.RepoPath(\"a/\\nb\")"),
        !typeChecks("histos.plugin.ArtifactPath(\"a/../b\")"),
        !typeChecks("histos.plugin.ArtifactPath(\"C:/b\")"),
        !typeChecks("histos.plugin.ArtifactPath(\"a/\\\\b\")"),
        !typeChecks("histos.plugin.ArtifactPath(\"a/\\nb\")"),
      )
    },
    test("module directories and action identifiers retain their own grammars") {
      assertTrue(
        safe.forall(raw => ProjectPath.from(raw).isLeft),
        ActionName.from("actions/build").isRight,
        ActionName.from("actions/build@1").isLeft,
        ActionName.from("actions/build page").isLeft,
        !typeChecks("histos.ProjectPath(\"vendor/client@1\")"),
      )
    },
  )

  def spec = tests
end FilePathSpec
