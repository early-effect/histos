package histos.testrun

import histos.jdk.JdkRuntime
import histos.jdk.Sha256
import histos.js.{JsLink, JsLinkError, JsLinkRequest, JsSuite, JsSuiteRequest, ScalaJs}
import histos.native.{
  NativeCompile,
  NativeLink,
  NativeLinkRequest,
  NativeOrigin,
  NativeSuite,
  NativeSuiteRequest,
  NativeToolchain,
}
import histos.node.{NodeBinary, NodeMiss}
import histos.toolchain.Runtimes
import histos.resolve.{BuildResolvers, ClasspathScope, DependencyLock, Repositories, Resolve, ResolveCell, ResolveError}
import histos.select.SelectError
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileProblem,
  CompileRequest,
  CompileToolchain,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  JvmCompile,

  ScalacFlags,
  SemanticDb,
  Sources,
}
import histos.pack.{MainClass, MainClassError}
import histos.{ClangPin, Dep, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, TestMain, TestProvider}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Ref, UIO, ZIO}
import zio.stream.ZStream

/** A finished cell and the lines its process wrote. */
final case class TestReport(cell: TestedCell, transcript: Transcript)

/** A compile event and a test event on one stream. A test cell starts when its compile finishes.
  */
enum SessionFeed:
  case Compile(feed: histos.zinc.CompileFeed)
  case Test(feed: TestFeed)

/** One test invocation. The callback runs after a cell's tests compile. */
final case class TestRun(
    repo: Path,
    graph: ProjectGraph,
    names: List[ProjectName],
    jdk: JdkRuntime,
    runtimes: Runtimes,
    mode: TestMode,
    patterns: List[SuitePattern] = Nil,
    onTestCompile: (
        ProjectName,
        ScalaVersion,
        Platform,
        histos.zinc.CompileFacts,
    ) => UIO[Unit] = (_, _, _, _) => ZIO.unit,
)
