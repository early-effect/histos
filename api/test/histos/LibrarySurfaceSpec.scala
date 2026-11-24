package histos

import zio.test.*

/** The module surface a library build writes: extra sources, native libraries, one native test binary, and a verify
  * lane.
  */
object LibrarySurfaceSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.1"

  private val core = module("core", Jvm, Native)(scala3).clang(ClangMajor(21))

  private val wire = module("wire", Jvm, Native)(scala3).clang(ClangMajor(21))

  def spec = suite("library surface")(
    test("extra source roots stay after the module directory and a repeated path is one") {
      val shared = path"shared/src"
      val built  = core.sources(shared, shared).sources(path"more/src")
      val graph  = Projects(built)
      assertTrue(
        graph.map(_.projects.collect { case module: Module => module.sources }) ==
          Right(List(List(shared, path"more/src")))
      )
    },
    test("a native module names the libraries it links") {
      val built = core.link(NativeLibrary.Ssl, NativeLibrary.Crypto)
      val graph = Projects(built)
      assertTrue(
        graph.map(_.projects.collect { case module: Module => module.libraries }) ==
          Right(List(Set(NativeLibrary.Ssl, NativeLibrary.Crypto)))
      )
    },
    test("a native test aggregate names its members and is not a ship") {
      val tests = module("nativeTests", Native)(scala3)
        .clang(ClangMajor(21))
        .testAggregate(core, wire)
      val row   = Ship("demo").next(0, 1, 0)(tests)
      val graph = Projects(ships(row), core, wire, tests)
      assertTrue(
        graph.isLeft,
        graph.swap.exists(_.isInstanceOf[BuildError.TestAggregatePublished]),
      )
    },
    test("a test aggregate refuses a member that does not build native") {
      val jvm   = module("app", Jvm)(scala3)
      val tests = module("nativeTests", Native)(scala3).clang(ClangMajor(21)).testAggregate(jvm)
      val graph = Projects(jvm, tests)
      assertTrue(graph.swap.exists(_.isInstanceOf[BuildError.TestAggregatePlatform]))
    },
    test("a verify lane round-trips and the default lane is the suite") {
      val app   = module("app", Jvm)(scala3).lane(TestLane.Verify)
      val graph = Projects(app, core)
      val lanes = graph.map(_.projects.collect { case module: Module => module.name.value -> module.lane })
      assertTrue(lanes == Right(List("app" -> TestLane.Verify, "core" -> TestLane.Suite)))
    },
    test("a system property is a name and a value") {
      assertTrue(
        SystemProperty.from("chekhov.browsers=firefox").map(_.argument) == Right("-Dchekhov.browsers=firefox"),
        SystemProperty.from("has space=no").isLeft,
      )
    },
  )
end LibrarySurfaceSpec
