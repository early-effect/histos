package histos.bsp

import zio.json.*
import zio.json.ast.Json
import zio.test.*

object RpcLawSpec extends ZIOSpecDefault:
  def spec = suite("rpc laws")(
    test("an id is an int, a string, or null"):
      val rows = List(
        "1"       -> Right(Rpc.RpcId.Num(1)),
        "\"ada\"" -> Right(Rpc.RpcId.Text("ada")),
        "null"    -> Right(Rpc.RpcId.Absent),
      )
      val bad = rows.collect { case (raw, want) if raw.fromJson[Rpc.RpcId] != want => raw }
      assertTrue(
        bad == Nil,
        "2147483648".fromJson[Rpc.RpcId] == Left(shown("bsp id is not an int")),
        "true".fromJson[Rpc.RpcId] == Left(shown("bsp id is not a string or a number")),
        "null".fromJson[Rpc.NullResult] == Right(Rpc.NullResult()),
        "{}".fromJson[Rpc.NullResult] == Left(shown("expected null")),
      )
    ,
    test("a main class uses the class field and rejects a bad shape"):
      val main = Rpc.ScalaMainClass("demo.Main", List("a"), List("-X"))
      val text = main.toJson
      assertTrue(
        text.fromJson[Rpc.ScalaMainClass] == Right(main),
        text.contains("\"class\""),
        !text.contains("className"),
        """{"arguments":[],"jvmOptions":[]}""".fromJson[Rpc.ScalaMainClass] == Left(shown("class is required")),
        "\"nope\"".fromJson[Rpc.ScalaMainClass] == Left(shown("a main class is an object")),
        """{"class":"m","arguments":[1],"jvmOptions":[]}""".fromJson[Rpc.ScalaMainClass] ==
          Left(shown("arguments is a list of strings")),
        """{"class":"m","arguments":[],"jvmOptions":[true]}""".fromJson[Rpc.ScalaMainClass] ==
          Left(shown("jvmOptions is a list of strings")),
      )
    ,
    test("targets and compile params are missing or rejected before they are read"):
      val targets  = json("""{"targets":[{"uri":"a"},{"uri":"b"}]}""")
      val compile  = json("""{"targets":[{"uri":"a"}]}""")
      val rejected = json("\"x\"")
      assertTrue(
        Rpc.targets(None) == Left(Rpc.ParamsDecode.Missing),
        Rpc.compileParams(None) == Left(Rpc.ParamsDecode.Missing),
        targets match
          case Some(value) => Rpc.targets(Some(value)) == Right(List("a", "b"))
          case None        => false
        ,
        compile match
          case Some(value) =>
            Rpc.compileParams(Some(value)) == Right(Rpc.CompileParams(List(Rpc.TargetId("a")), None))
          case None => false
        ,
        rejected match
          case Some(value) =>
            Rpc.targets(Some(value)) == Left(Rpc.ParamsDecode.Rejected) &&
            Rpc.compileParams(Some(value)) == Left(Rpc.ParamsDecode.Rejected)
          case None => false,
      )
    ,
    test("cancel accepts a string or number id and rejects null"):
      val number = json("""{"id":4}""")
      val text   = json("""{"id":"ada"}""")
      val absent = json("""{"id":null}""")
      val bare   = json("\"x\"")
      val empty  = json("{}")
      assertTrue(
        Rpc.cancelId(None) == Left(Rpc.ParamsDecode.Missing),
        number match
          case Some(value) => Rpc.cancelId(Some(value)) == Right(Rpc.RpcId.Num(4))
          case None        => false
        ,
        text match
          case Some(value) => Rpc.cancelId(Some(value)) == Right(Rpc.RpcId.Text("ada"))
          case None        => false
        ,
        absent match
          case Some(value) => Rpc.cancelId(Some(value)) == Left(Rpc.ParamsDecode.Rejected)
          case None        => false
        ,
        bare match
          case Some(value) => Rpc.cancelId(Some(value)) == Left(Rpc.ParamsDecode.Rejected)
          case None        => false
        ,
        empty match
          case Some(value) => Rpc.cancelId(Some(value)) == Left(Rpc.ParamsDecode.Rejected)
          case None        => false
        ,
        json("null") match
          case Some(value) => Rpc.cancelId(Some(value)) == Left(Rpc.ParamsDecode.Rejected)
          case None        => false,
      )
    ,
    test("a reply and a note are the json-rpc 2.0 documents a client reads, with the incoming id"):
      val failed = Rpc.fail(Rpc.RpcId.Num(3), -32601, "bsp does not handle nope")
      val ok     = Rpc.ok(Rpc.RpcId.Absent, Rpc.NullResult())
      val noted  = Rpc.note("window/logMessage", Rpc.LogMessage(1, "hi"))
      assertTrue(
        failed == """{"jsonrpc":"2.0","id":3,"error":{"code":-32601,"message":"bsp does not handle nope"}}""",
        ok == """{"jsonrpc":"2.0","id":null,"result":null}""",
        noted == """{"jsonrpc":"2.0","method":"window/logMessage","params":{"type":1,"message":"hi"}}""",
        Rpc.idOf(Rpc.Incoming(Some("build/exit"), None, None)) == Rpc.RpcId.Absent,
        Rpc.idOf(Rpc.Incoming(None, Some(Rpc.RpcId.Num(3)), None)) == Rpc.RpcId.Num(3),
      ),
  )

  private def json(raw: String): Option[Json] =
    raw.fromJson[Json].toOption

  /** `fromJson` renders a codec message in parentheses. */
  private def shown(message: String): String =
    s"($message)"
end RpcLawSpec
