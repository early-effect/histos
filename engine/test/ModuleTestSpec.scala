package histos.testrun

import histos.node.{NodeMiss, NodeToolchain}
import histos.resolve.{BuildResolvers, Resolve}
import histos.toolchain.Runtimes
import histos.zinc.GraphCompile
import histos.{
  ClangMajor,
  Dep,
  FrameworkClass,
  Js,
  JsEnv,
  Jvm,
  Native,
  ProjectName,
  Projects,
  Requires,
  ScalaVersion,
  TestMachine,
  TestMain,
  TestProvider,
  Tool,
  module
}
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, System, ZIO, durationInt}
import zio.test.*

private[testrun] object ModuleTestSupport:
  private val scala3 = ScalaVersion("3.10.0")

  private[testrun] val subprocess = suite("subprocess")(
    test(
      "a named main runs against main classes, and a module with nothing to run is not a pass"
    ):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          built <- ZIO
            .attempt {
              val core = module("core", Jvm)(scala3)
              val app = module("app", Jvm)(scala3)
                .dependsOn(core)
                .test(TestMain("demo.Check"))
              write(
                root,
                "core/src/main/scala/demo/Core.scala",
                "package demo\n\nobject Core:\n  def n: Int = 1\n"
              )
              write(
                root,
                "app/src/main/scala/demo/App.scala",
                "package demo\n\nobject App:\n  def x: Int = Core.n\n"
              )
              write(root, "app/test/scala/demo/Check.scala", checkPasses)
              write(
                root,
                "extra/src/main/scala/demo/Extra.scala",
                "package demo\n\nobject Extra:\n  def n: Int = 1\n"
              )
              write(root, "extra/test/place.txt", "not scala")
              write(
                root,
                ".histos/dependencies.lock",
                "cell core 3.10.0 jvm\ncell app 3.10.0 jvm\ncell extra 3.10.0 jvm\n"
              )
              Projects(core, module("extra", Jvm)(scala3), app)
            }
            .flatMap(graph => ZIO.fromEither(graph))
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          named <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, List("app"), jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          mainEntries <- ZIO.attempt(
            entries(root.resolve(".histos/classes/app/3.10.0/jvm"))
          )
          testEntries <- ZIO.attempt(
            entries(root.resolve(".histos/test-classes/app/3.10.0/jvm"))
          )
        yield assertTrue(
          cells.map(report =>
            (report.cell.module.value, report.cell.status.label)
          ) == List(
            ("app", "passed"),
            ("core", "no tests"),
            ("extra", "no tests")
          ),
          named.map(report =>
            (report.cell.module.value, report.cell.status.label)
          ) == List(("app", "passed")),
          !mainEntries.exists(_.endsWith("Check.class")),
          testEntries.exists(_.endsWith("Check.class"))
        )
    ,
    test("src/test is the suite when the module has no main"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          _ <- ZIO.attempt {
            write(root, "e2e/src/test/scala/demo/Check.scala", onlyPasses)
            write(root, ".histos/dependencies.lock", "cell e2e 3.10.0 jvm\n")
          }
          built <- ZIO.fromEither(
            Projects(module("e2e", Jvm)(scala3).test(TestMain("demo.Check")))
          )
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          mainEntries <- ZIO.attempt(
            entries(root.resolve(".histos/classes/e2e/3.10.0/jvm"))
          )
          testEntries <- ZIO.attempt(
            entries(root.resolve(".histos/test-classes/e2e/3.10.0/jvm"))
          )
        yield assertTrue(
          cells.map(report =>
            (report.cell.module.value, report.cell.status.label)
          ) == List(("e2e", "passed")),
          mainEntries.isEmpty,
          testEntries.exists(_.endsWith("Check.class"))
        )
    ,
    test(
      "a named js main uses only its overridden linker revision and chosen Node"
    ):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          probe = module("probe", Js)(scala3)
            .jsVersion(histos.JsVersion("1.22.0"))
            .test(TestMain("demo.Check"))
          built <- ZIO.fromEither(
            Projects.tools(
              histos.ToolCatalog.default.copy(js =
                histos.ToolCatalog.default.js
                  .copy(revision = histos.JsVersion("1.21.0"))
              )
            )(probe)
          )
          _ <- ZIO.attempt {
            write(root, "probe/src/test/scala/demo/Check.scala", hiPasses)
            write(root, ".histos/dependencies.lock", "cell probe 3.10.0 js\n")
          }
          chosen <- TestMachine.chosenNode(root)
          other = root.resolve("path-node.log")
          _ <- TestMachine.fakeNode(root.resolve("elsewhere"), other)
          env <- System.envs
          path = (root.resolve("elsewhere").resolve("bin").toString :: env
            .get("PATH")
            .toList).mkString(java.io.File.pathSeparator)
          here <- Runtimes.read(TestMachine.at(root, env + ("PATH" -> path)))
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          held <- histos.resolve.Resolve.readLock(root)
          tools <- ZIO.service[histos.resolve.LockedTools]
          olderRequests = List(
            histos.js.JsRequests.linker(
              histos.JsVersion("1.21.0"),
              tools.catalog.js.bridgeScala,
              tools.repositories
            ),
            histos.js.JsRequests.libraries(
              scala3,
              histos.JsVersion("1.21.0"),
              tools.repositories,
              tools.catalog
            )
          )
          older <- ZIO.foreach(olderRequests)(request =>
            tools.resolve
              .resolveTool(request)
              .map(classpath =>
                histos.resolve.LockedTool(
                  request.id,
                  request.roots,
                  request.repositories.all,
                  classpath.artifacts.map(artifact =>
                    histos.resolve.LockedArtifact(
                      artifact.organization,
                      artifact.name,
                      artifact.revision,
                      artifact.sha256,
                      artifact.repository,
                      artifact.classifier
                    )
                  ),
                  classpath.evictions
                )
              )
          )
          _ <- Workspace.writeString(
            histos.resolve.Resolve.lockPath(root),
            histos.resolve.DependencyLock.render(
              held.copy(tools = held.tools ++ older)
            )
          )
          fetched <- zio.Ref.make(List.empty[histos.resolve.LockedArtifact])
          resolver = new Resolve:
            def resolveTool(request: histos.resolve.ToolRequest) =
              tools.resolve.resolveTool(request)
            def resolve(cell: histos.resolve.ResolveCell) =
              tools.resolve.resolve(cell)
            def fetch(artifact: histos.resolve.LockedArtifact) =
              fetched.update(_ :+ artifact) *> tools.resolve.fetch(artifact)
            def resolvePlugin(
                dependency: Dep,
                repositories: histos.resolve.Repositories
            ) = tools.resolve.resolvePlugin(dependency, repositories)
            def sources(
                organization: String,
                name: String,
                revision: String,
                repository: histos.resolve.Repository
            ) = tools.resolve.sources(organization, name, revision, repository)
          coexisting <- ModuleTest
            .run(TestRun(root, built, Nil, jdk, here))
            .provide(zio.ZLayer.succeed(resolver))
          consumed <- fetched.get
          ran <- TestMachine.launches(chosen)
          skipped <- TestMachine.launches(other)
          compiled <- ZIO.attempt(
            entries(root.resolve(".histos/test-classes/probe/3.10.0/js"))
          )
          mainEntries <- ZIO.attempt(
            entries(root.resolve(".histos/classes/probe/3.10.0/js"))
          )
        yield assertTrue(
          held.tools
            .filter(_.id.role == histos.resolve.ToolRole.JsLinker)
            .map(_.id.variant) == List(
            histos.resolve.ToolVariant.Js(histos.JsVersion("1.22.0"))
          ),
          placed(cells) == List(("probe", scala3, Js, TestStatus.Passed)),
          placed(coexisting) == placed(cells),
          consumed
            .filter(artifact =>
              artifact.organization == "org.scala-js" && artifact.name
                .startsWith("scalajs-linker_")
            )
            .map(_.revision)
            .toSet == Set("1.22.0"),
          cells.map(text).exists(_.contains("hi")),
          ran.size == 2,
          skipped.isEmpty,
          compiled.exists(_.endsWith(".sjsir")),
          mainEntries.isEmpty
        )
    @@ Requires(Tool.Node),
    test("a js main with no Node to choose is a typed failure before the link"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          probe = module("probe", Js)(scala3).test(TestMain("demo.Check"))
          built <- ZIO.fromEither(Projects(probe))
          _ <- ZIO.attempt {
            write(root, "probe/src/test/scala/demo/Check.scala", hiPasses)
            write(root, ".histos/dependencies.lock", "cell probe 3.10.0 js\n")
          }
          here <- Runtimes.read(TestMachine.at(root, Map.empty))
          out <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
          name <- ZIO.fromEither(ProjectName.from("probe"))
          linked <- ZIO.attempt(Files.exists(root.resolve(".histos/link")))
        yield assertTrue(
          out == Left(
            ModuleTestError.NoNode(
              name,
              scala3,
              NodeMiss.Absent(NodeToolchain.Floor)
            )
          ),
          !linked
        )
    ,
    test("a named native main links and runs the binary"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          probe = module("probe", Native)(ScalaVersion("3.9.0"))
            .clang(ClangMajor(21))
            .test(TestMain("demo.Check"))
          built <- ZIO.fromEither(Projects(probe))
          _ <- ZIO.attempt {
            write(root, "probe/src/test/scala/demo/Check.scala", hiPasses)
            write(
              root,
              ".histos/dependencies.lock",
              "cell probe 3.9.0 native\n"
            )
          }
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          compiled <- ZIO.attempt(
            entries(root.resolve(".histos/test-classes/probe/3.9.0/native"))
          )
        yield assertTrue(
          placed(cells) == List(
            ("probe", ScalaVersion("3.9.0"), Native, TestStatus.Passed)
          ),
          cells.map(text).exists(_.contains("hi")),
          compiled.exists(_.endsWith(".nir"))
        )
    @@ Requires(Tool.Clang(ClangMajor(21))),
    test("a single module uses test at the repository root"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, "test/scala/demo/Check.scala", helloPasses)
            write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
          }
          built <- ZIO.fromEither(
            Projects(module("core", Jvm)(scala3).test(TestMain("demo.Check")))
          )
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue:
          cells.map(report =>
            (
              report.cell.module.value,
              report.cell.version.value,
              report.cell.status.label
            )
          ) == List(("core", "3.10.0", "passed"))
    ,
    test("a non-zero main is a failed cell and its output is kept"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, "test/scala/demo/Check.scala", helloFails)
            write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
          }
          built <- ZIO.fromEither(
            Projects(module("core", Jvm)(scala3).test(TestMain("demo.Check")))
          )
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue(
          cells.map(report =>
            (report.cell.version.value, report.cell.status)
          ) == List(
            ("3.10.0", TestStatus.Failed(1))
          ),
          cells.exists(_.transcript.outText.text.contains("nope"))
        )
  )

  private[testrun] val providers = suite("provider")(
    test("ZIO Test is the default provider, and a failing spec is not a pass"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          zioDep = Dep("dev.zio::zio-test-sbt:2.1.27")
          core = module("core", Jvm)(scala3).testDependencies(zioDep)
          bad = module("bad", Jvm)(scala3)
            .testProvider(TestProvider.Zio)
            .testDependencies(zioDep)
          built <- ZIO.fromEither(Projects(core, bad))
          _ <- ZIO.attempt {
            write(
              root,
              "core/src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, "core/test/scala/demo/Check.scala", passSpec)
            write(
              root,
              "bad/src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, "bad/test/scala/demo/Check.scala", failSpec)
          }
          _ <- Resolve
            .update(root, built, BuildResolvers(built.resolvers))
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue(
          cells.map(report =>
            (report.cell.module.value, report.cell.status)
          ) == List(
            ("bad", TestStatus.Failed(1)),
            ("core", TestStatus.Passed)
          ),
          cells.headOption.map(text).exists(_.contains("nope")),
          cells.lastOption.exists(_.transcript.outText.text.contains("adds"))
        )
    ,
    test("a suite pattern runs one provider suite"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          zioDep = Dep("dev.zio::zio-test-sbt:2.1.27")
          core = module("core", Jvm)(scala3).testDependencies(zioDep)
          built <- ZIO.fromEither(Projects(core))
          _ <- ZIO.attempt {
            write(
              root,
              "core/src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(
              root,
              "core/test/scala/demo/FooA.scala",
              zioNamed("FooA", "alpha")
            )
            write(
              root,
              "core/test/scala/demo/FooB.scala",
              zioNamed("FooB", "beta")
            )
          }
          _ <- Resolve
            .update(root, built, BuildResolvers(built.resolvers))
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          pattern <- ZIO.fromEither(SuitePattern.from("demo.FooA"))
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here, List(pattern)))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue(
          cells.map(_.cell.status) == List(TestStatus.Passed),
          cells.map(text).exists(_.contains("alpha")),
          !cells.map(text).exists(_.contains("beta"))
        )
    ,
    test("a js provider links the Scala.js bridge and runs on node"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          iface = Dep("org.scala-js:scalajs-test-interface_2.13:1.22.0")
          provider <- ZIO.fromEither(
            TestProvider(
              FrameworkClass("demo.ProbeFramework"),
              "org.scala-js",
              "scalajs-test-interface_2.13"
            )
          )
          probe = module("probe", Js)(scala3)
            .testProvider(provider)
            .testDependencies(iface)
          built <- ZIO.fromEither(Projects(probe))
          _ <- ZIO.attempt(
            write(
              root,
              "probe/src/test/scala/demo/ProbeFramework.scala",
              probeFramework
            )
          )
          _ <- Resolve
            .update(root, built, BuildResolvers(built.resolvers))
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          chosen <- TestMachine.chosenNode(root)
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          ran <- TestMachine.launches(chosen)
        yield assertTrue(
          placed(cells) == List(("probe", scala3, Js, TestStatus.Passed)),
          cells.map(text).exists(_.contains("hi")),
          ran.nonEmpty
        )
    @@ Requires(Tool.Node),
    test("a js provider runs under jsdom"):
      ZIO.scoped(jsProvider(JsEnv.Jsdom, installJsdom = true))
    @@ Requires(Tool.Node, Tool.Npm),
    test("a js provider runs under firefox"):
      ZIO.scoped(jsProvider(JsEnv.Firefox, installJsdom = false))
    @@ Requires(Tool.Node, Tool.Firefox),
    test("a native provider links TestMain and runs the binary"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          iface = Dep("org.scala-native::test-interface:0.5.12")
          provider <- ZIO.fromEither(
            TestProvider(
              FrameworkClass("demo.ProbeFramework"),
              "org.scala-native",
              "test-interface"
            )
          )
          probe = module("probe", Native)(ScalaVersion("3.9.0"))
            .clang(ClangMajor(21))
            .testProvider(provider)
            .testDependencies(iface)
          built <- ZIO.fromEither(Projects(probe))
          _ <- ZIO.attempt(
            write(
              root,
              "probe/src/test/scala/demo/ProbeFramework.scala",
              nativeProbe
            )
          )
          _ <- Resolve
            .update(root, built, BuildResolvers(built.resolvers))
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          here <- TestMachine.runtimes(root)
          cells <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue(
          placed(cells) == List(
            ("probe", ScalaVersion("3.9.0"), Native, TestStatus.Passed)
          ),
          cells.map(text).exists(_.contains("hi"))
        )
    @@ Requires(Tool.Clang(ClangMajor(21)))
  )

  private[testrun] val refused = suite("refused before the subprocess")(
    test("test sources without a provider library are refused"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, "test/scala/demo/Check.scala", helloPasses)
            write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
          }
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          here <- TestMachine.runtimes(root)
          out <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
        yield assertTrue:
          out match
            case Left(ModuleTestError.ProviderDependency(module, provider)) =>
              module.value == "core" && provider == TestProvider.Zio
            case _ => false
    ,
    test("a suite pattern runs the matching mains"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          fooA = module("a", Jvm)(scala3).test(TestMain("demo.FooA"))
          fooB = module("b", Jvm)(scala3).test(TestMain("demo.FooB"))
          built <- ZIO.fromEither(Projects(fooA, fooB))
          _ <- ZIO.attempt {
            write(
              root,
              "a/src/test/scala/demo/FooA.scala",
              mainNamed("FooA", "alpha")
            )
            write(
              root,
              "b/src/test/scala/demo/FooB.scala",
              mainNamed("FooB", "beta")
            )
            write(
              root,
              ".histos/dependencies.lock",
              "cell a 3.10.0 jvm\ncell b 3.10.0 jvm\n"
            )
          }
          bothPattern <- ZIO.fromEither(SuitePattern.from("demo.Foo*"))
          onePattern <- ZIO.fromEither(SuitePattern.from("demo.FooA"))
          here <- TestMachine.runtimes(root)
          both <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here, List(bothPattern)))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          one <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here, List(onePattern)))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          said = (reports: List[TestReport]) =>
            reports
              .map(report =>
                s"${report.cell.module.value}:${report.cell.status.label}:${text(report)}"
              )
              .mkString(" ")
        yield assertTrue(
          said(both).contains("alpha") && said(both).contains("beta"),
          said(one).contains("alpha") && !said(one).contains("beta")
        )
    ,
    test("a suite pattern that matches nothing fails"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("a", Jvm)(scala3).test(TestMain("demo.FooA"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(
              root,
              "a/src/test/scala/demo/FooA.scala",
              mainNamed("FooA", "alpha")
            )
            write(root, ".histos/dependencies.lock", "cell a 3.10.0 jvm\n")
          }
          pattern <- ZIO.fromEither(SuitePattern.from("demo.Missing"))
          here <- TestMachine.runtimes(root)
          out <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here, List(pattern)))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
        yield assertTrue:
          out match
            case Left(ModuleTestError.NoneMatched(patterns)) =>
              patterns.map(_.value) == List("demo.Missing")
            case _ => false
    ,
    test("a named main without test sources is refused"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
          }
          built <- ZIO.fromEither(
            Projects(module("core", Jvm)(scala3).test(TestMain("demo.Check")))
          )
          here <- TestMachine.runtimes(root)
          out <- histos.ToolFixture
            .at(root, built)(
              ModuleTest
                .run(TestRun(root, built, Nil, jdk, here))
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
            .either
        yield assertTrue:
          out match
            case Left(ModuleTestError.MissingSources(module, main)) =>
              module.value == "core" && main.value == "demo.Check"
            case _ => false
  )

  private def jsProvider(env: JsEnv, installJsdom: Boolean) =
    for
      jdk <- GraphCompile.jdkHere
      root <- temp
      iface = Dep("org.scala-js:scalajs-test-interface_2.13:1.22.0")
      provider <- ZIO.fromEither(
        TestProvider(
          FrameworkClass("demo.ProbeFramework"),
          "org.scala-js",
          "scalajs-test-interface_2.13"
        )
      )
      probe = module("probe", Js)(scala3)
        .jsEnv(env)
        .testProvider(provider)
        .testDependencies(iface)
      built <- ZIO.fromEither(Projects(probe))
      _ <- ZIO.attempt(
        write(
          root,
          "probe/src/test/scala/demo/ProbeFramework.scala",
          probeFramework
        )
      )
      _ <- ZIO.when(installJsdom)(npm(root, "jsdom@27"))
      _ <- Resolve
        .update(root, built, BuildResolvers(built.resolvers))
        .provideSomeLayer[histos.resolve.LockedTools](
          histos.ToolFixture.resolver
        )
      chosen <- TestMachine.chosenNode(root)
      here <- TestMachine.runtimes(root)
      cells <- histos.ToolFixture
        .at(root, built)(
          ModuleTest
            .run(TestRun(root, built, Nil, jdk, here))
        )
        .provideSomeLayer[histos.resolve.LockedTools](
          histos.ToolFixture.resolver
        )
      ran <- TestMachine.launches(chosen)
    yield assertTrue(
      placed(cells) == List(("probe", scala3, Js, TestStatus.Passed)),
      cells.map(text).exists(_.contains("hi")),
      ran.nonEmpty
    )

  /** jsdom's node process resolves `require("jsdom")` by walking up from the
    * work directory.
    */
  private def npm(root: Path, pkg: String): ZIO[Any, Throwable, Unit] =
    ZIO
      .attemptBlocking {
        val process =
          new ProcessBuilder("npm", "install", pkg, "--prefix", root.toString)
            .directory(root.toFile)
            .redirectErrorStream(true)
            .start()
        val text = new String(
          process.getInputStream.readAllBytes,
          java.nio.charset.StandardCharsets.UTF_8
        )
        val code = process.waitFor()
        if code == 0 then Right(()) else Left(text)
      }
      .flatMap(result =>
        ZIO.fromEither(result).mapError(text => new IllegalStateException(text))
      )

  private val nativeProbe =
    """package demo
      |
      |import sbt.testing.*
      |
      |import scala.scalanative.reflect.annotation.EnableReflectiveInstantiation
      |
      |@EnableReflectiveInstantiation
      |final class ProbeFramework extends Framework:
      |  def name(): String = "probe"
      |  def fingerprints(): Array[Fingerprint] = Array(ProbeFingerprint)
      |  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
      |    new ProbeRunner
      |  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: String => Unit): Runner =
      |    new ProbeRunner
      |
      |object ProbeFingerprint extends SubclassFingerprint:
      |  def isModule(): Boolean = true
      |  def superclassName(): String = "demo.Probe"
      |  def requireNoArgConstructor(): Boolean = false
      |
      |abstract class Probe
      |
      |object Check extends Probe:
      |  def result: String = "hi"
      |
      |final class ProbeRunner extends Runner:
      |  def args: Array[String] = Array.empty
      |  def remoteArgs(): Array[String] = Array.empty
      |  def tasks(defs: Array[TaskDef]): Array[Task] =
      |    defs.map(d => new ProbeTask(d))
      |  def done(): String = ""
      |  def receiveMessage(msg: String): Option[String] = None
      |  def serializeTask(task: Task, serializer: TaskDef => String): String =
      |    serializer(task.taskDef())
      |  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
      |    new ProbeTask(deserializer(task))
      |
      |final class ProbeTask(d: TaskDef) extends Task:
      |  def taskDef(): TaskDef = d
      |  def tags(): Array[String] = Array.empty
      |  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
      |    loggers.foreach(_.info(Check.result))
      |    handler.handle(new Event:
      |      def fullyQualifiedName(): String = d.fullyQualifiedName()
      |      def fingerprint(): Fingerprint = d.fingerprint()
      |      def selector(): Selector = new SuiteSelector
      |      def status(): Status = Status.Success
      |      def throwable(): OptionalThrowable = new OptionalThrowable()
      |      def duration(): Long = 0L
      |    )
      |    Array.empty
      |""".stripMargin

  private val probeFramework =
    """package demo
      |
      |import sbt.testing.*
      |
      |import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
      |
      |@EnableReflectiveInstantiation
      |final class ProbeFramework extends Framework:
      |  def name(): String = "probe"
      |  def fingerprints(): Array[Fingerprint] = Array(ProbeFingerprint)
      |  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
      |    new ProbeRunner
      |  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: String => Unit): Runner =
      |    new ProbeRunner
      |
      |object ProbeFingerprint extends SubclassFingerprint:
      |  def isModule(): Boolean = true
      |  def superclassName(): String = "demo.Probe"
      |  def requireNoArgConstructor(): Boolean = false
      |
      |abstract class Probe
      |
      |object Check extends Probe:
      |  def result: String = "hi"
      |
      |final class ProbeRunner extends Runner:
      |  def args(): Array[String] = Array.empty
      |  def remoteArgs(): Array[String] = Array.empty
      |  def tasks(defs: Array[TaskDef]): Array[Task] =
      |    defs.map(d => new ProbeTask(d))
      |  def done(): String = ""
      |  def receiveMessage(msg: String): Option[String] = None
      |  def serializeTask(task: Task, serializer: TaskDef => String): String =
      |    serializer(task.taskDef())
      |  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
      |    new ProbeTask(deserializer(task))
      |
      |final class ProbeTask(d: TaskDef) extends Task:
      |  def taskDef(): TaskDef = d
      |  def tags(): Array[String] = Array.empty
      |  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
      |    report(handler, loggers)
      |    Array.empty
      |  def execute(handler: EventHandler, loggers: Array[Logger], continuation: Array[Task] => Unit): Unit =
      |    report(handler, loggers)
      |    continuation(Array.empty)
      |  private def report(handler: EventHandler, loggers: Array[Logger]): Unit =
      |    loggers.foreach(_.info(Check.result))
      |    handler.handle(new Event:
      |      def fullyQualifiedName(): String = d.fullyQualifiedName()
      |      def fingerprint(): Fingerprint = d.fingerprint()
      |      def selector(): Selector = new SuiteSelector
      |      def status(): Status = Status.Success
      |      def throwable(): OptionalThrowable = new OptionalThrowable()
      |      def duration(): Long = 0L
      |    )
      |""".stripMargin

  private val passSpec =
    """import zio.test.*
      |
      |object Check extends ZIOSpecDefault:
      |  def spec = suite("check")(
      |    test("adds") {
      |      assertTrue(1 + 1 == 2)
      |    }
      |  )
      |""".stripMargin

  private val failSpec =
    """import zio.test.*
      |
      |object Check extends ZIOSpecDefault:
      |  def spec = suite("check")(
      |    test("nope") {
      |      assertTrue(1 == 2)
      |    }
      |  )
      |""".stripMargin

  private def mainNamed(name: String, word: String): String =
    s"""package demo
       |
       |object $name:
       |  def main(args: Array[String]): Unit =
       |    println("$word")
       |""".stripMargin

  private def zioNamed(name: String, word: String): String =
    s"""package demo
       |
       |import zio.test.*
       |
       |object $name extends ZIOSpecDefault:
       |  def spec = suite("$word")(
       |    test("ran") {
       |      assertTrue(true)
       |    }
       |  )
       |""".stripMargin

  private val helloPasses =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    if demo.Hello.greet != "hi" then System.exit(2)
      |""".stripMargin

  private val helloFails =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    System.out.println("nope")
      |    System.exit(1)
      |""".stripMargin

  private val hiPasses =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("hi")
      |""".stripMargin

  private val onlyPasses =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit = ()
      |""".stripMargin

  private val checkPasses =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    if App.x != 1 then System.exit(2)
      |""".stripMargin

  private def entries(classes: Path): List[String] =
    if !Files.isDirectory(classes) then Nil
    else
      val walk = Files.walk(classes)
      try
        walk.iterator.asScala
          .filter(Files.isRegularFile(_))
          .map(path => classes.relativize(path).toString.replace('\\', '/'))
          .toList
          .sorted
      finally walk.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace
      .work("histos-test-run")
      .mapError(error => new java.io.IOException(error.message))

  private[testrun] val quick = suite("quick")(
    test(
      "quick stamps include selected JDK content but ignore unrelated tools"
    ):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).test(TestMain("demo.Foo"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(
              root,
              "app/src/test/scala/demo/Foo.scala",
              onlyPasses.replace("Check", "Foo")
            )
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          first <- feeds(root, built, Some(jdk))
          second <- feeds(root, built, Some(jdk))
          changed = jdk.copy(identity =
            histos.toolchain.RuntimeIdentity.Local(
              jdk.home,
              histos.jdk.Sha256.of(zio.Chunk.single(17.toByte))
            )
          )
          third <- feeds(root, built, Some(changed))
          lock <- histos.resolve.Resolve.readLock(root)
          unused = histos.resolve.LockedTool(
            histos.resolve.ToolId(
              histos.resolve.ToolRole.JsEnvironment,
              scala3,
              histos.resolve.ToolVariant.Firefox
            ),
            Nil,
            List(histos.resolve.Repository.Central),
            Nil,
            Nil
          )
          _ <- Workspace.writeString(
            histos.resolve.Resolve.lockPath(root),
            histos.resolve.DependencyLock.render(
              lock.copy(tools = lock.tools :+ unused)
            )
          )
          fourth <- feeds(root, built, Some(changed))
        yield assertTrue(
          cached(first).isEmpty,
          cached(second) == List("demo.Foo"),
          cached(third).isEmpty,
          cached(fourth) == List("demo.Foo"),
          first.exists {
            case TestFeed.Done(cell) => cell.status == TestStatus.Passed;
            case _                   => false
          },
          third.exists {
            case TestFeed.Done(cell) => cell.status == TestStatus.Passed;
            case _                   => false
          }
        )
    ,
    test("a cached JVM success cannot bypass a corrupt selected compiler"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).test(TestMain("demo.Foo"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(
              root,
              "app/src/test/scala/demo/Foo.scala",
              onlyPasses.replace("Check", "Foo")
            )
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          _ <- feeds(root, built, Some(jdk))
          cachedRun <- feeds(root, built, Some(jdk))
          lock <- histos.resolve.Resolve.readLock(root)
          corrupted = lock.copy(tools =
            lock.tools.map(cell =>
              if cell.id.role == histos.resolve.ToolRole.Compiler && cell.id.scalaVersion == scala3
              then
                cell.copy(artifacts =
                  cell.artifacts.map(
                    _.copy(sha256 = histos.jdk.Sha256.of(zio.Chunk.empty))
                  )
                )
              else cell
            )
          )
          _ <- Workspace.writeString(
            histos.resolve.Resolve.lockPath(root),
            histos.resolve.DependencyLock.render(corrupted)
          )
          runtimes <- TestMachine.runtimes(root)
          name <- ZIO.fromEither(ProjectName.from(app.name))
          result <- ModuleTest
            .suites(
              TestRun(root, built, Nil, jdk, runtimes, Nil, quick = true),
              List(
                histos.zinc.CompiledCell(
                  name,
                  scala3,
                  Jvm,
                  GraphCompile.classesDir(root, name, scala3),
                  0
                )
              )
            )
            .runCollect
            .either
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
        yield assertTrue(
          cached(cachedRun) == List("demo.Foo"),
          result.left.toOption.exists {
            case ModuleTestError.Lock(
                  histos.resolve.ResolveError.Checksum(_, _, _)
                ) =>
              true
            case _ => false
          }
        )
    ,
    test(
      "testQuick reruns a suite when a referenced class changes and skips it otherwise"
    ):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).test(TestMain("demo.Foo"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/main/scala/demo/Lib.scala", lib("alpha"))
            write(
              root,
              "app/src/main/scala/demo/Other.scala",
              libNamed("Other", "other")
            )
            write(root, "app/src/test/scala/demo/Foo.scala", callsLib)
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          first <- feeds(root, built)
          second <- feeds(root, built)
          _ <- ZIO.attempt(
            write(
              root,
              "app/src/main/scala/demo/Other.scala",
              libNamed("Other", "changed")
            )
          )
          third <- feeds(root, built)
          _ <- ZIO.attempt(
            write(root, "app/src/main/scala/demo/Lib.scala", lib("beta"))
          )
          fourth <- feeds(root, built)
        yield assertTrue(
          out(first).contains("alpha") && cached(first).isEmpty,
          cached(second) == List("demo.Foo") && !out(second).contains("alpha"),
          cached(third) == List("demo.Foo") && !out(third).contains("alpha"),
          out(fourth).contains("beta") && cached(fourth).isEmpty
        )
    ,
    test("a full run stores the fingerprint the next quick run hits"):
      ZIO.scoped:
        for
          root <- temp
          app = module("app", Jvm)(scala3).test(TestMain("demo.Foo"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/main/scala/demo/Lib.scala", lib("alpha"))
            write(root, "app/src/test/scala/demo/Foo.scala", callsLib)
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          first <- feeds(root, built, quick = false)
          second <- feeds(root, built)
        yield assertTrue(
          cached(first).isEmpty && out(first).contains("alpha"),
          cached(second) == List("demo.Foo") && !out(second).contains("alpha")
        )
    ,
    test("a testOnly success is a hit and the other suite still runs"):
      ZIO.scoped:
        for
          root <- temp
          zioDep = Dep("dev.zio::zio-test-sbt:2.1.27")
          app = module("app", Jvm)(scala3).testDependencies(zioDep)
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(
              root,
              "app/src/test/scala/demo/Foo.scala",
              zioNamed("Foo", "alpha")
            )
            write(
              root,
              "app/src/test/scala/demo/Bar.scala",
              zioNamed("Bar", "beta")
            )
          }
          _ <- Resolve
            .update(root, built, BuildResolvers(built.resolvers))
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          pattern <- ZIO.fromEither(SuitePattern.from("demo.Foo"))
          selected <- feeds(
            root,
            built,
            patterns = List(pattern),
            quick = false
          )
          again <- feeds(root, built)
        yield assertTrue(
          cached(selected).isEmpty && out(selected).contains("alpha") && !out(
            selected
          ).contains("beta"),
          cached(again) == List("demo.Foo") && out(again).contains(
            "beta"
          ) && !out(again).contains("alpha")
        )
    ,
    test("a passing ZIO suite is cached on the second quick run"):
      ZIO.scoped:
        for
          root <- temp
          zioDep = Dep("dev.zio::zio-test-sbt:2.1.27")
          app = module("app", Jvm)(scala3).testDependencies(zioDep)
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt(
            write(
              root,
              "app/src/test/scala/demo/Foo.scala",
              zioNamed("Foo", "alpha")
            )
          )
          _ <- Resolve
            .update(root, built, BuildResolvers(built.resolvers))
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          first <- feeds(root, built)
          second <- feeds(root, built)
        yield assertTrue(
          cached(first).isEmpty && out(first).contains("ran"),
          cached(second) == List("demo.Foo") && !out(second).contains("ran")
        )
    ,
    test("a failed suite is not a quick success"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).test(TestMain("demo.Foo"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
            write(root, "app/src/test/scala/demo/Foo.scala", fails)
            write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
          }
          first <- feeds(root, built)
          second <- feeds(root, built)
        yield assertTrue(
          failed(first) && failed(second) && cached(first).isEmpty && cached(
            second
          ).isEmpty
        )
  )

  private def feeds(
      root: Path,
      built: histos.ProjectGraph,
      selected: Option[histos.jdk.JdkRuntime] = None,
      patterns: List[SuitePattern] = Nil,
      quick: Boolean = true
  ) =
    GraphCompile.jdkHere.flatMap: actual =>
      val jdk = selected.getOrElse(actual)
      TestMachine
        .runtimes(root)
        .flatMap(here =>
          histos.ToolFixture
            .streamAt(root, built)(
              ModuleTest
                .console(
                  TestRun(root, built, Nil, jdk, here, patterns, quick)
                )
            )
            .runCollect
        )
        .provideSomeLayer[histos.resolve.LockedTools](
          histos.ToolFixture.resolver
        )

  private def out(feeds: zio.Chunk[TestFeed]): String =
    feeds.collect { case TestFeed.Out(text) => text }.mkString("\n")

  private def cached(feeds: zio.Chunk[TestFeed]): List[String] =
    feeds.collect { case TestFeed.Cached(_, _, _, suite) => suite }.toList

  private def failed(feeds: zio.Chunk[TestFeed]): Boolean =
    feeds.collect {
      case TestFeed.Done(TestedCell(_, _, _, TestStatus.Failed(exit))) =>
        exit
    }.nonEmpty

  private def placed(
      reports: List[TestReport]
  ): List[(String, ScalaVersion, histos.Platform, TestStatus)] =
    reports.map(report =>
      (
        report.cell.module.value,
        report.cell.version,
        report.cell.platform,
        report.cell.status
      )
    )

  /** Stdout, then stderr. */
  private def text(report: TestReport): String =
    report.transcript.outText.text + report.transcript.errText.text

  private def lib(word: String): String = libNamed("Lib", word)

  private def libNamed(name: String, word: String): String =
    s"""package demo
       |
       |object $name:
       |  def word: String = "$word"
       |""".stripMargin

  private val callsLib =
    """package demo
      |
      |object Foo:
      |  def main(args: Array[String]): Unit =
      |    println(Lib.word)
      |""".stripMargin

  private val fails =
    """package demo
      |
      |object Foo:
      |  def main(args: Array[String]): Unit =
      |    println("nope")
      |    System.exit(1)
      |""".stripMargin

/** One running compile per core. More than that and each test sits past a
  * minute.
  */
private[testrun] def moduleTests[R, E](group: Spec[R, E]): Spec[R, E] =
  suite("module test")(group) @@
    TestAspect.parallelN(
      java.lang.Runtime.getRuntime.availableProcessors.max(1)
    ) @@
    TestAspect.withLiveSystem @@
    TestAspect.timeout(4.minutes)

object ModuleTestJvmSpec extends ZIOSpecDefault:
  def spec = moduleTests(ModuleTestSupport.subprocess)
    .provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.layer(
        histos.Projects(
          histos.module("jvm", histos.Jvm)(histos.ScalaVersion("3.10.0")),
          histos.module("js", histos.Js)(histos.ScalaVersion("3.10.0")),
          histos
            .module("native", histos.Native)(histos.ScalaVersion("3.9.0"))
            .clang(histos.ClangMajor(21))
        )
      )
    )

object ModuleTestProviderSpec extends ZIOSpecDefault:
  def spec = moduleTests(ModuleTestSupport.providers)
    .provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.layer(
        histos.Projects(
          histos.module("jvm", histos.Jvm)(histos.ScalaVersion("3.10.0")),
          histos.module("node", histos.Js)(histos.ScalaVersion("3.10.0")),
          histos
            .module("jsdom", histos.Js)(histos.ScalaVersion("3.10.0"))
            .jsEnv(histos.JsEnv.Jsdom),
          histos
            .module("firefox", histos.Js)(histos.ScalaVersion("3.10.0"))
            .jsEnv(histos.JsEnv.Firefox),
          histos
            .module("native", histos.Native)(histos.ScalaVersion("3.9.0"))
            .clang(histos.ClangMajor(21))
        )
      )
    )

object ModuleTestRefusedSpec extends ZIOSpecDefault:
  def spec = moduleTests(ModuleTestSupport.refused)
    .provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.jvm(histos.ScalaVersion("3.10.0"))
    )

object ModuleTestQuickSpec extends ZIOSpecDefault:
  def spec = moduleTests(ModuleTestSupport.quick)
    .provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.jvm(histos.ScalaVersion("3.10.0"))
    )
