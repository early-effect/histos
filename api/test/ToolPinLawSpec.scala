package histos

import zio.test.*

object ToolPinLawSpec extends ZIOSpecDefault:
  private val counts = Gen.int(-2, 362)

  def spec = suite("tool pin laws")(
    test("a full version round-trips, and a series is not a pin"):
      val full   = "3.10.0"
      val build  = "3.10.0-RC1"
      val native = "0.5.13-20261030-eb3f3b8-SNAPSHOT"
      assertTrue(
        ScalaVersion.from(full).map(_.value) == Right(full),
        ScalaVersion.from(build).map(_.value) == Right(build),
        ScalaVersion.from("") == Left(ScalaVersionError.Empty),
        ScalaVersion.from("3") == Left(ScalaVersionError.NotFull("3")),
        ScalaVersion.from("2.13") == Left(ScalaVersionError.NotFull("2.13")),
        JsVersion.from(full).map(_.value) == Right(full),
        JsVersion.from("") == Left(JsVersionError.Empty),
        JsVersion.from("1.22") == Left(JsVersionError.NotExact("1.22")),
        ToolRevision.from(full).map(_.value) == Right(full),
        ToolRevision.from("") == Left(ToolRevisionError.Empty),
        ToolRevision.from("21") == Left(ToolRevisionError.NotExact("21")),
        NativeVersion.from(full).map(_.value) == Right(full),
        NativeVersion.from("0.5.13-SNAPSHOT").map(_.value) == Right("0.5.13-SNAPSHOT"),
        NativeVersion.from(native).map(_.value) == Right(native),
        NativeVersion.from("") == Left(NativeVersionError.Empty),
        NativeVersion.from("0.5") == Left(NativeVersionError.Bad("0.5")),
      )
    ,
    test("a clang major and a node floor are positive, and a job timeout is 1 to 360"):
      check(counts): raw =>
        val positive = raw > 0
        val minutes  = 1 <= raw && raw <= 360
        assertTrue(
          ClangMajor.from(raw).map(_.value) == (if positive then Right(raw) else Left(ClangError.NotPositive(raw))),
          NodeFloor.from(raw).map(_.value) == (if positive then Right(raw) else Left(NodeFloorError.NotPositive(raw))),
          JobTimeout.admits(raw) == minutes,
          JobTimeout.from(raw) == (
            if minutes then Right(JobTimeout.Minutes(raw)) else Left(JobTimeoutError(raw))
          ),
        )
    ,
    test("the default timeout is 30 minutes, and from never returns unbounded"):
      assertTrue(
        JobTimeout.default == JobTimeout.Minutes(30),
        JobTimeout.ceiling == 360,
        JobTimeout.from(1) == Right(JobTimeout.Minutes(1)),
        JobTimeout.from(360) == Right(JobTimeout.Minutes(360)),
        JobTimeout.from(0) == Left(JobTimeoutError(0)),
        JobTimeout.from(361) == Left(JobTimeoutError(361)),
      )
    ,
    test("a node path is an absolute binary named node"):
      assertTrue(
        NodePath.from("/usr/bin/node").map(_.value) == Right("/usr/bin/node"),
        NodePath.from("  /usr/bin/node  ").map(_.value) == Right("/usr/bin/node"),
        NodePath.from("/usr/bin/../bin/node").map(_.value) == Right("/usr/bin/node"),
        NodePath.from("") == Left(NodePathError.Empty),
        NodePath.from("   ") == Left(NodePathError.Empty),
        NodePath.from("/node").map(_.value) == Right("/node"),
        NodePath.from("node") == Left(NodePathError.Relative("node")),
        NodePath.from("./node") == Left(NodePathError.Relative("./node")),
        NodePath.from("/usr/bin/npm") == Left(NodePathError.NotBinary("/usr/bin/npm")),
        NodePath.from("/usr/bin/Node") == Left(NodePathError.NotBinary("/usr/bin/Node")),
        NodePath.from("\u0000") == Left(NodePathError.Invalid("\u0000")),
      )
    ,
    test("platform, promote, js, and module tokens are closed"):
      val platforms = List(Platform.Jvm -> "jvm", Platform.Js -> "js", Platform.Native -> "native")
      val promotes  = List(Promote.Rebuild -> "rebuild", Promote.CopyIndex -> "copy-index")
      val envs      = List(JsEnv.Node -> "node", JsEnv.Jsdom -> "jsdom", JsEnv.Firefox -> "firefox")
      val kinds     = List(
        JsModuleKind.NoModule -> "nomodule",
        JsModuleKind.ESModule -> "esmodule",
        JsModuleKind.CommonJS -> "commonjs",
      )
      assertTrue(
        platforms.forall((platform, token) => platform.token == token && Platform.from(token) == Right(platform)),
        Platform.from("scala") == Left(PlatformError.Unknown("scala")),
        promotes.forall((promote, token) => promote.token == token && Promote.from(token) == Right(promote)),
        Promote.from("copy") == Left(PromoteError.Unknown("copy")),
        envs.forall((env, token) => env.token == token),
        kinds.forall((kind, token) => kind.token == token),
      )
    ,
    test("scalac flags render their canonical name and their argv, in call order"):
      val flags = List(
        ScalacFlag.EncodingUtf8        -> ("encoding-utf8", List("-encoding", "UTF-8")),
        ScalacFlag.MaxInlines          -> ("max-inlines", List("-Xmax-inlines:128")),
        ScalacFlag.Deprecation         -> ("deprecation", List("-deprecation")),
        ScalacFlag.Feature             -> ("feature", List("-feature")),
        ScalacFlag.UnusedAll           -> ("unused-all", List("-Wunused:all")),
        ScalacFlag.ImplicitConversions -> ("implicit-conversions", List("-language:implicitConversions")),
      )
      val combined = ScalacFlags.of(ScalacFlag.Feature, ScalacFlag.Deprecation)
      assertTrue(
        flags.forall: (flag, expected) =>
          val (canonical, argv) = expected
          ScalacFlags.of(flag).canonical == canonical && ScalacFlags.of(flag).argv == argv
        ,
        combined.canonical == "feature,deprecation",
        combined.argv == List("-feature", "-deprecation"),
        ScalacFlags.empty.canonical == "",
        ScalacFlags.empty.argv.isEmpty,
      )
    ,
    test("a coverage floor is 0 to 100, and argv follows the flag order"):
      val floors = CoverageFloors.parse(
        FloorInput(
          statements = Some(0),
          branches = Some(100),
          fileStatements = None,
          fileBranches = Some(1),
          packageStatements = None,
          packageBranches = None,
        )
      )
      val high = CoverageFloors.parse(FloorInput(statements = Some(101)))
      val low  = CoverageFloors.parse(FloorInput(branches = Some(-1)))
      assertTrue(
        floors.map(_.argv) == Right(List("--min-stmt", "0", "--min-branch", "100", "--min-branch-file", "1")),
        high == Left(CoverageFloorError.OutOfRange(FloorName.Statements, BigInt(101))),
        low == Left(CoverageFloorError.OutOfRange(FloorName.Branches, BigInt(-1))),
        FloorName.values.map(_.token).forall(FloorName.known),
        !FloorName.known("--min-line"),
        CoverageFloors.parse(FloorInput()).map(_.argv) == Right(Nil),
      ),
  )
end ToolPinLawSpec
