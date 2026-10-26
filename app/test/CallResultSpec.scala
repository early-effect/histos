package histos.cli

import histos.node.{NodeToolchain, NodeVersion}
import histos.report.{BuildFault, CallResult, WireJson}
import histos.resolve.ResolveError
import histos.server.Call

import java.nio.file.Path

import zio.Chunk
import zio.Config
import zio.json.*
import zio.test.*

import WireJson.given

object CallResultSpec extends ZIOSpecDefault:
  def spec = suite("call result")(
    test("no lock is 8 for compile and 5 for test and run"):
      val fault = CallResult.Fault(BuildFault.Resolve(ResolveError.NoLock(Path.of("lock"))))
      assertTrue(
        CliExit.from(Call.Compile(Nil), fault) == CliExit.CompileFailed,
        CliExit.from(Call.Test(Nil, Nil, false), fault) == CliExit.ResolveFailed,
        CliExit.from(Call.Run("demo", None), fault) == CliExit.ResolveFailed,
        fault.toJson.fromJson[CallResult] == Right(fault),
      )
    ,
    test("a compile that ran and failed stays a report"):
      val ran = CallResult.reported("demo: type mismatch")
      assertTrue(
        CliExit.from(Call.Compile(Nil), ran) == CliExit.CompileFailed,
        CliExit.from(Call.Test(Nil, Nil, false), ran) == CliExit.TestFailed,
        CliExit.from(Call.Publish("repo", Nil), ran) == CliExit.PublishFailed,
      )
    ,
    test("invalid config data round trips"):
      val error: Config.Error = Config.Error.InvalidData(Chunk("scala"), "not a version")
      assertTrue(error.toJson.fromJson[Config.Error] == Right(error))
    ,
    test("a node floor and a probed node version round trip"):
      val floor  = NodeToolchain.Floor
      val probed = NodeVersion.from("25.2.0")
      assertTrue(
        floor.toJson.fromJson[NodeVersion] == Right(floor),
        probed.flatMap(version => version.toJson.fromJson[NodeVersion]) == probed,
        "nope".fromJson[NodeVersion].isLeft,
        "0".fromJson[NodeVersion].isLeft,
        "25.0".fromJson[NodeVersion].isLeft,
      ),
  )
