package histos.testrun

import histos.jdk.Sha256
import histos.native.ClassShapes
import histos.process.{ChildEvent, ProcessExit}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{Dep, DepKind, FrameworkClass, Module, Platform, ProjectName, ProviderLibrary, ScalaVersion, SourcePos, TestProvider}

import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Label, Opcodes}

import zio.{Chunk, ZIO}
import zio.test.*

object SuiteLawSpec extends ZIOSpecDefault:
  private val version   = ScalaVersion.trusted("3.10.0")
  private val module    = ProjectName.trusted("core")
  private val namedSuite = SuiteName("demo.nested.A")

  def spec = suite("suite laws")(
    suite("a pattern matches the names it claims")(
      test("an empty pattern list allows every name, including the empty name"):
        assertTrue(SuiteMatch.allows(Nil, ""), SuiteMatch.allows(Nil, "demo.A"))
      ,
      test("a star is any run of characters, a question is one character, and a dot is a dot"):
        patterns("demo.*", "A?", "A.B", "Foo", "Foo$") match
          case Left(error)                                            => assertTrue(error == SuitePatternError.Empty)
          case Right(List(star, one, dotted, plain, dollar))          =>
            assertTrue(
              SuiteMatch.allows(List(star), "demo.A.B"),
              !SuiteMatch.allows(List(star), "other.A"),
              SuiteMatch.allows(List(one), "AB"),
              !SuiteMatch.allows(List(one), "A"),
              !SuiteMatch.allows(List(one), "ABC"),
              !SuiteMatch.allows(List(dotted), "AxB"),
              SuiteMatch.allows(List(dotted), "A.B"),
              SuiteMatch.allows(List(plain), "Foo$"),
              !SuiteMatch.allows(List(plain), "Foo$$"),
              SuiteMatch.allows(List(dollar), "Foo$"),
              !SuiteMatch.allows(List(dollar), "Foo"),
            )
          case Right(other)                                           => assertTrue(other == Nil)
      ,
      test("an empty pattern and a blank pattern stay typed failures"):
        assertTrue(
          SuitePattern.from("") == Left(SuitePatternError.Empty),
          SuitePattern.from("a b") == Left(SuitePatternError.Blank("a b")),
          SuitePattern.from("demo.*").map(_.value) == Right("demo.*"),
        )
      ,
      test("the child paste anchors each pattern and escapes a dollar"):
        patterns("demo.A", "Foo$") match
          case Left(error)              => assertTrue(error == SuitePatternError.Empty)
          case Right(List(dotted, dollar)) =>
            val pasted = SuiteMatch.embed(List(dotted, dollar))
            assertTrue(
              pasted.contains("^demo\\\\.A$"),
              pasted.contains("^Foo\\\\$$"),
              pasted.contains("suitePatterns.length == 0"),
            )
          case Right(other)             => assertTrue(other == Nil)
      ,
    ),
    suite("suite names are the modules a provider would run")(
      test("reachability, a cycle, a module flag, and a dropped dollar decide the list"):
        val spec = "zio.test.ZIOSpecAbstract"
        val demo = ClassShapes.Shape("demo.Foo$", true, List("demo.Mid$"))
        val mid  = ClassShapes.Shape("demo.Mid$", true, List(spec))
        val plain = ClassShapes.Shape("demo.Plain", false, List(spec))
        val child = ClassShapes.Shape("subprocess.Hidden$", true, List(spec))
        val anon  = ClassShapes.Shape("demo.Foo$$anon", true, List(spec))
        val loopA = ClassShapes.Shape("demo.A$", true, List("demo.B$"))
        val loopB = ClassShapes.Shape("demo.B$", true, List("demo.A$"))
        val lost  = ClassShapes.Shape("demo.Lost$", true, List("missing.Gone"))
        val world = List(demo, mid, plain, child, anon, loopA, loopB, lost).map(shape => shape.name -> shape).toMap
        val marks = List(SuiteNames.Mark(spec, module = true))
        assertTrue(
          SuiteNames.choose(List(demo, mid, plain, child, anon, loopA, lost), world, marks) == List("demo.Foo", "demo.Mid"),
        )
      ,
      test("a missing directory is no suites, and only sjsir files are unseen"):
        Workspace.work("histos-suite-names").flatMap: root =>
          val missing = root.resolve("missing")
          val scripts = root.resolve("scripts")
          val mixed   = root.resolve("mixed")
          val classes = root.resolve("classes")
          for
            absent <- SuiteNames.discover(missing, Nil, "zio.test.sbt.ZTestFramework")
            _      <- Workspace.createDirectories(scripts)
            _      <- Workspace.writeString(scripts.resolve("demo.sjsir"), "")
            unseen <- SuiteNames.discover(scripts, Nil, "zio.test.sbt.ZTestFramework")
            _      <- Workspace.createDirectories(mixed)
            _      <- writeBytes(mixed.resolve("demo/Bar.class"), emptyClass("demo/Bar", "java/lang/Object"))
            _      <- Workspace.writeString(mixed.resolve("demo.sjsir"), "")
            ignored <- SuiteNames.discover(mixed, Nil, "zio.test.sbt.ZTestFramework")
            _      <- Workspace.createDirectories(classes)
            _      <- writeBytes(classes.resolve("demo/Foo$.class"), emptyClass("demo/Foo$", "zio/test/ZIOSpecAbstract"))
            _      <- writeBytes(classes.resolve("demo/Mid$.class"), emptyClass("demo/Mid$", "zio/test/ZIOSpecAbstract"))
            _      <- writeBytes(classes.resolve("demo/Child$.class"), emptyClass("demo/Child$", "demo/Mid$"))
            _      <- writeBytes(classes.resolve("demo/Plain.class"), emptyClass("demo/Plain", "zio/test/ZIOSpecAbstract"))
            _      <- writeBytes(
              classes.resolve("subprocess/Hidden$.class"),
              emptyClass("subprocess/Hidden$", "zio/test/ZIOSpecAbstract"),
            )
            found   <- SuiteNames.discover(classes, Nil, "zio.test.sbt.ZTestFramework")
            unnamed <- SuiteNames.discover(classes, Nil, "other.Framework")
          yield assertTrue(
            absent == SuiteNames.Found.Suites(Nil),
            unseen == SuiteNames.Found.Unseen,
            ignored == SuiteNames.Found.Suites(Nil),
            found == SuiteNames.Found.Suites(List("demo.Child", "demo.Foo", "demo.Mid")),
            unnamed == SuiteNames.Found.Suites(Nil),
          )
      ,
    ),
    suite("a provider hears the status it claims")(
      test("exit status, an unmatched pattern, and a provider with nothing to run are different results"):
        val name = ProjectName.trusted("core")
        for
          passed    <- SuiteLaunch.hear(name, version, Platform.Jvm, Some(TestProvider.Zio), ChildEvent.Exited(ProcessExit(0)))
          failed    <- SuiteLaunch.hear(name, version, Platform.Jvm, None, ChildEvent.Exited(ProcessExit(7)))
          unmatched <- SuiteLaunch.hear(
            name,
            version,
            Platform.Jvm,
            Some(TestProvider.Zio),
            ChildEvent.Exited(ProcessExit(ProviderRunner.NoneMatched)),
          )
          none <- SuiteLaunch
            .hear(name, version, Platform.Jvm, Some(TestProvider.Zio), ChildEvent.Exited(ProcessExit(ProviderRunner.NoneFound)))
            .either
          plainNone <- SuiteLaunch.hear(name, version, Platform.Jvm, None, ChildEvent.Exited(ProcessExit(ProviderRunner.NoneFound)))
          plainMiss <- SuiteLaunch.hear(
            name,
            version,
            Platform.Jvm,
            None,
            ChildEvent.Exited(ProcessExit(ProviderRunner.NoneMatched)),
          )
          out <- SuiteLaunch.hear(name, version, Platform.Jvm, None, ChildEvent.Out("line"))
          err <- SuiteLaunch.hear(name, version, Platform.Js, None, ChildEvent.Err("bad"))
        yield assertTrue(
          passed == TestFeed.Done(TestedCell(name, version, Platform.Jvm, TestStatus.Passed)),
          failed == TestFeed.Done(TestedCell(name, version, Platform.Jvm, TestStatus.Failed(7))),
          unmatched == TestFeed.Done(TestedCell(name, version, Platform.Jvm, TestStatus.Unmatched)),
          none == Left(ModuleTestError.NoneDiscovered(name, TestProvider.Zio)),
          plainNone == TestFeed.Done(TestedCell(name, version, Platform.Jvm, TestStatus.Failed(ProviderRunner.NoneFound))),
          plainMiss == TestFeed.Done(TestedCell(name, version, Platform.Jvm, TestStatus.Failed(ProviderRunner.NoneMatched))),
          out == TestFeed.Out(name, version, Platform.Jvm, "line"),
          err == TestFeed.Err(name, version, Platform.Js, "bad"),
        )
      ,
      test("exact keeps a real pattern and drops an empty or blank one"):
        patterns("demo.A", "demo.*") match
          case Left(error)           => assertTrue(error == SuitePatternError.Empty)
          case Right(List(exact, star)) =>
            assertTrue(SuiteLaunch.exact(List("", " ", "demo.A", "a b", "demo.*")) == List(exact, star))
          case Right(other)          => assertTrue(other == Nil)
      ,
      test("the pattern environment is the count and one variable per pattern"):
        patterns("demo.A", "demo.*") match
          case Left(error)              => assertTrue(error == SuitePatternError.Empty)
          case Right(List(exact, star)) =>
            val note = ProviderNote(TestProvider.Zio.framework.value, Path.of("classes"), List(exact, star))
            val env  = note.environment
            val none = ProviderNote(TestProvider.Zio.framework.value, Path.of("classes"), Nil).environment
            assertTrue(
              env.get(ProviderRunner.PatternCount).contains("2"),
              env.get(s"${ProviderRunner.PatternPrefix}0").contains("demo.A"),
              env.get(s"${ProviderRunner.PatternPrefix}1").contains("demo.*"),
              env.get(s"${ProviderRunner.PatternPrefix}2") == None,
              env.get(ProviderRunner.Framework).contains(TestProvider.Zio.framework.value),
              none.get(ProviderRunner.PatternCount).contains("0"),
              none.get(s"${ProviderRunner.PatternPrefix}0") == None,
            )
          case Right(other)             => assertTrue(other == Nil)
      ,
      test("a class binary drops a non-class, a dollar-dollar name, and an empty relative name"):
        Workspace.work("histos-suite-binary").flatMap: root =>
          val source = root.resolve("Demo.scala")
          val anon   = root.resolve("demo/Foo$$anon.class")
          val real   = root.resolve("demo/A.class")
          for
            _ <- Workspace.writeString(source, "")
            _ <- writeBytes(anon, Array.emptyByteArray)
            _ <- writeBytes(real, Array.emptyByteArray)
          yield assertTrue(
            SuiteLaunch.classBinary(root, source) == None,
            SuiteLaunch.classBinary(root, anon) == None,
            SuiteLaunch.classBinary(root, real).contains("demo.A"),
            SuiteLaunch.classBinary(root, root) == None,
          )
      ,
      test("an empty pattern does not read, a missing directory stays open, and a class must match"):
        Workspace.work("histos-suite-narrow").flatMap: root =>
          val classes = root.resolve("classes")
          val missing = root.resolve("missing")
          patterns("demo.A", "demo.Missing") match
            case Left(error) => ZIO.succeed(assertTrue(error == SuitePatternError.Empty))
            case Right(List(hit, miss)) =>
              for
                open    <- SuiteLaunch.narrowed(missing, Nil)
                absent  <- SuiteLaunch.narrowed(missing, List(hit))
                _       <- Workspace.createDirectories(classes)
                _       <- writeBytes(classes.resolve("demo/Foo$$anon.class"), Array.emptyByteArray)
                hidden  <- SuiteLaunch.narrowed(classes, List(miss))
                _       <- writeBytes(classes.resolve("other/B.class"), Array.emptyByteArray)
                skipped <- SuiteLaunch.narrowed(classes, List(hit))
                _       <- writeBytes(classes.resolve("demo/A.class"), Array.emptyByteArray)
                kept    <- SuiteLaunch.narrowed(classes, List(hit))
              yield assertTrue(open, absent, hidden, !skipped, kept)
            case Right(other) => ZIO.succeed(assertTrue(other == Nil))
      ,
      test("satisfaction is the provider organization and name"):
        val library = Dep.trusted("dev.zio", "zio-test-sbt", "0", DepKind.Binary)
        val wrong   = Dep.trusted("dev.zio", "zio-test", "0", DepKind.Binary)
        val empty   = TestProvider.trusted(FrameworkClass.trusted("demo.Framework"), ProviderLibrary.trusted("", "zio-test-sbt"))
        assertTrue(
          SuiteLaunch.satisfied(mod(List(library)), TestProvider.Zio),
          !SuiteLaunch.satisfied(mod(List(wrong)), TestProvider.Zio),
          !SuiteLaunch.satisfied(mod(Nil), TestProvider.Zio),
          !SuiteLaunch.satisfied(mod(List(library)), empty),
        )
      ,
    ),
    suite("quick records and test paths")(
      test("the first path is kept, and a repeated path is not a second source"):
        val first  = Path.of("/a")
        val second = Path.of("/b")
        assertTrue(TestCompile.distinct(List(first, second, first, second)) == List(first, second))
      ,
      test("the module test directory wins, and the repository test is only the single-module layout"):
        Workspace.work("histos-test-base").flatMap: root =>
          val named = root.resolve("core").resolve("test")
          val shared = root.resolve("test")
          val one    = mod(Nil)
          for
            absent <- TestCompile.testBase(root, one, List(one))
            _      <- Workspace.createDirectories(shared)
            repo   <- TestCompile.testBase(root, one, List(one))
            many   <- TestCompile.testBase(root, one, List(one, mod(Nil)))
            _      <- Workspace.createDirectories(named)
            local  <- TestCompile.testBase(root, one, List(one, mod(Nil)))
          yield assertTrue(absent == None, repo.contains(shared), many == None, local.contains(named))
      ,
      test("test classes live under the test-classes cell"):
        val repo = Path.of("/repo")
        assertTrue(
          TestCompile.testClasses(repo, module, version, Platform.Js) ==
            repo.resolve(".histos").resolve("test-classes").resolve("core").resolve("3.10.0").resolve("js")
        )
      ,
      test("a missing record is a miss, the same digest is a hit, and a bad record is not a miss"):
        Workspace.work("histos-quick").flatMap: root =>
          val digest = Sha256.of(Chunk.fromArray("ok".getBytes))
          val other  = Sha256.of(Chunk.fromArray("no".getBytes))
          val path   = QuickRecord.file(root, module, version, Platform.Jvm, namedSuite)
          for
            missing <- QuickRecord.hit(root, module, version, Platform.Jvm, namedSuite, digest)
            _       <- QuickRecord.save(root, module, version, Platform.Jvm, namedSuite, digest)
            hit     <- QuickRecord.hit(root, module, version, Platform.Jvm, namedSuite, digest)
            moved   <- QuickRecord.hit(root, module, version, Platform.Jvm, namedSuite, other)
            _       <- QuickRecord.clear(root, module, version, Platform.Jvm, namedSuite)
            cleared <- QuickRecord.hit(root, module, version, Platform.Jvm, namedSuite, digest)
            _       <- Workspace.writeString(path, "nope\n")
            shape   <- QuickRecord.hit(root, module, version, Platform.Jvm, namedSuite, digest).either
            _       <- Workspace.writeString(path, "success zzz\n")
            hashed  <- QuickRecord.hit(root, module, version, Platform.Jvm, namedSuite, digest).either
            hashOk = hashed match
              case Left(QuickError.Hash(found, _)) => found == path
              case _                               => false
          yield assertTrue(
            !missing,
            hit,
            !moved,
            !cleared,
            path == root.resolve(".histos/test-quick/core/3.10.0/jvm/demo/nested/A"),
            shape == Left(QuickError.Shape(path)),
            hashOk,
          )
      ,
    ),
    suite("a suite stamp is the referenced closure")(
      test("a line number moves the stamp, an unreferenced class does not, and a missing suite is none"):
        Workspace.work("histos-stamp").flatMap: root =>
          val classes = root.resolve("classes")
          val cover   = SuiteStamp.Cover(Nil, Nil, "21", Nil, Nil)
          val suite   = classes.resolve("demo/A.class")
          val used    = classes.resolve("demo/Used.class")
          val alias   = classes.resolve("demo/Alias.class")
          val module  = classes.resolve("demo/Alias$.class")
          val other   = classes.resolve("demo/Other.class")
          val bad     = classes.resolve("demo/Bad.class")
          for
            _       <- writeBytes(suite, referenced("demo/A", List("demo/Used", "demo/Alias$"), 1))
            _       <- writeBytes(used, referenced("demo/Used", Nil, 1))
            _       <- writeBytes(alias, referenced("demo/Alias", Nil, 1))
            _       <- writeBytes(module, referenced("demo/Alias$", Nil, 1))
            _       <- writeBytes(other, referenced("demo/Other", Nil, 1))
            first   <- SuiteStamp.digest("demo.A", List(classes), cover)
            many    <- SuiteStamp.digests(List("demo.A", "missing"), List(classes), cover)
            _       <- writeBytes(suite, referenced("demo/A", List("demo/Used", "demo/Alias$"), 9))
            lines   <- SuiteStamp.digest("demo.A", List(classes), cover)
            _       <- writeBytes(suite, referenced("demo/A", List("demo/Used", "demo/Alias$"), 1))
            _       <- writeBytes(other, referenced("demo/Other", Nil, 40))
            idle    <- SuiteStamp.digest("demo.A", List(classes), cover)
            _       <- writeBytes(used, referenced("demo/Used", Nil, 4))
            called  <- SuiteStamp.digest("demo.A", List(classes), cover)
            _       <- writeBytes(used, referenced("demo/Used", Nil, 1))
            _       <- writeBytes(alias, referenced("demo/Alias", Nil, 5))
            pulled  <- SuiteStamp.digest("demo.A", List(classes), cover)
            flags   <- SuiteStamp.digest("demo.A", List(classes), cover.copy(flags = List("b", "a")))
            flipped <- SuiteStamp.digest("demo.A", List(classes), cover.copy(flags = List("a", "b")))
            jars    <- SuiteStamp.digest("demo.A", List(classes), cover.copy(jars = List("b", "a")))
            jarFlip <- SuiteStamp.digest("demo.A", List(classes), cover.copy(jars = List("a", "b")))
            jdk     <- SuiteStamp.digest("demo.A", List(classes), cover.copy(jdk = "17"))
            _       <- writeBytes(bad, Array(1, 2, 3))
            broken  <- SuiteStamp.digest("demo.A", List(classes), cover).either
          yield assertTrue(
            first.isDefined,
            lines != first,
            idle == first,
            called != first,
            pulled != first,
            flags != flipped,
            jars == jarFlip,
            jdk != first,
            many.get("missing") == Some(None),
            many.get("demo.A") == first.map(Some(_)),
            broken == Left(StampError.Class(bad)),
          )
      ,
    ),
  )

  private def patterns(raw: String*): Either[SuitePatternError, List[SuitePattern]] =
    raw.toList.foldLeft[Either[SuitePatternError, List[SuitePattern]]](Right(Nil)):
      case (Left(error), _)    => Left(error)
      case (Right(found), one) => SuitePattern.from(one).map(found :+ _)

  private def mod(tests: List[Dep]): Module =
    Module(
      module,
      histos.Platforms(Platform.Jvm, Nil),
      histos.ScalaVersions(version, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      testDependencies = tests,
    )

  private def emptyClass(name: String, superName: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, null, superName, null)
    writer.visitEnd()
    writer.toByteArray

  private def referenced(name: String, owners: List[String], line: Int): Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", null)
    val method = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "n", "()V", null, null)
    method.visitCode()
    val start = new Label
    method.visitLabel(start)
    method.visitLineNumber(line, start)
    owners.foreach(owner => method.visitMethodInsn(Opcodes.INVOKESTATIC, owner, "n", "()V", false))
    method.visitInsn(Opcodes.RETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit
end SuiteLawSpec
