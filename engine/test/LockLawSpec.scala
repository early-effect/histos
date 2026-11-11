package histos.resolve

import histos.jdk.Sha256
import histos.plugin.LinkStrategy
import histos.{DepKind, Platform, ProjectName, ScalaVersion}

import zio.test.*

object LockLawSpec extends ZIOSpecDefault:
  private val hex    = "ab" * 32
  private val other  = "cd" * 32
  private val sha    = Sha256.trusted(hex)
  private val module = ProjectName.trusted("core")
  private val scala3 = ScalaVersion.trusted("3.3.7")
  private val text   =
    s"""cell core 3.3.7 jvm
       |direct org.example demo 1.0.0 java
       |artifact org.example demo 1.0.0 $hex central
       |""".stripMargin

  def spec = suite("lock laws")(
    test("a synthetic lock parses, renders, and parses back to the same cells"):
      DependencyLock.parse(text) match
        case Left(_)     => assertTrue(false)
        case Right(lock) =>
          val again = DependencyLock.parse(DependencyLock.render(lock))
          assertTrue(
            again == Right(lock),
            lock.cells.length == 1,
            lock.plugins.isEmpty,
            lock.tools.isEmpty,
          )
    ,
    test("comments and blank lines are not cells, and an empty file is empty"):
      val noted =
        s"""# pin
           |
           |$text""".stripMargin
      assertTrue(
        DependencyLock.parse(noted) == DependencyLock.parse(text),
        DependencyLock.parse("") == Left(LockError.Empty),
        DependencyLock.parse("# only\n\n") == Left(LockError.Empty),
      )
    ,
    test("a changed checksum is a different lock"):
      val tampered = text.replace(hex, "b" + hex.drop(1))
      (DependencyLock.parse(text), DependencyLock.parse(tampered)) match
        case (Right(locked), Right(changed)) =>
          val left  = shaOf(locked)
          val right = shaOf(changed)
          assertTrue(locked != changed, left != right, DependencyLock.render(locked) != DependencyLock.render(changed))
        case _ => assertTrue(false)
    ,
    test("a short direct is a missing direct, and a direct with no cell is a missing cell"):
      val short = "cell core 3.3.7 jvm\ndirect org.example\n"
      val loose = "direct org.example demo 1.0.0 java\n"
      assertTrue(
        DependencyLock.parse(short) == Left(LockError.Missing(LockField.Direct)),
        DependencyLock.parse(loose) == Left(LockError.Missing(LockField.Cell)),
      )
    ,
    test("a bad sha, a repeated cell, an api link, and a source on a module are typed lock errors"):
      val bad     = s"cell core 3.3.7 jvm\nartifact org.example demo 1.0.0 nope central\n"
      val twice   = "cell core 3.3.7 jvm\ncell core 3.3.7 jvm\n"
      val api     = "plugin org.example demo 1.0.0 api\n"
      val sourced = s"cell core 3.3.7 jvm\nsource org.example demo 1.0.0 $hex central\n"
      val early   = "evict org.example demo 1.0.0 1.0.1\n"
      assertTrue(
        DependencyLock.parse(bad) == Left(LockError.BadSha256("nope")),
        DependencyLock.parse(twice) == Left(
          LockError.DuplicateCell(module, scala3, Platform.Jvm, ClasspathScope.Compile)
        ),
        DependencyLock.parse(api) == Left(LockError.BadLine("plugin org.example demo 1.0.0 api")),
        LinkToken.render(LinkStrategy.Api) == "api",
        LinkToken.parse("api").isEmpty,
        LinkToken.parse("exact-binary").contains(LinkStrategy.ExactBinary),
        DependencyLock.parse(sourced) == Left(LockError.BadLine(s"source org.example demo 1.0.0 $hex central")),
        DependencyLock.parse(early) == Left(LockError.Missing(LockField.Cell)),
      )
    ,
    test("an eviction with whitespace is not written back"):
      val eviction = Eviction("org.example", "demo", "1.0.0", "1 2")
      val lock     = DependencyLock(
        List(LockedCell(module, scala3, Platform.Jvm, Nil, Nil, List(eviction)))
      )
      assertTrue(
        !Eviction.printable(eviction),
        DependencyLock.withEvictions("kept", lock) == "kept",
        !DependencyLock.render(lock).contains("evict"),
      )
    ,
    test("a test cell and a tool cell round-trip, and a source before a plugin is a missing plugin"):
      val testCell =
        s"""test core 3.3.7 js
           |direct org.example demo 1.0.0 binary
           |artifact org.example demo 1.0.0 $hex central
           |""".stripMargin
      val tool =
        s"""tool compiler 3.3.7 jvm
           |root org.example demo 1.0.0 closure central
           |artifact org.example demo 1.0.0 $hex central
           |""".stripMargin
      val source = s"source org.example demo 1.0.0 $hex central\n"
      (DependencyLock.parse(testCell), DependencyLock.parse(tool)) match
        case (Right(tests), Right(tools)) =>
          assertTrue(
            DependencyLock.parse(DependencyLock.render(tests)) == Right(tests),
            tests.cells.exists(_.scope == ClasspathScope.Test),
            DependencyLock.parse(DependencyLock.render(tools)) == Right(tools),
            tools.tools.length == 1,
            DependencyLock.parse(source) == Left(LockError.Missing(LockField.Plugin)),
          )
        case _ => assertTrue(false)
    ,
    test("exclude and series belong to the preceding direct, and a direct without them stays empty"):
      val marked =
        s"""cell core 3.3.7 jvm
           |direct org.example demo 1.0.0 binary
           |exclude org.example other
           |exclude com.acme gone
           |series 2
           |artifact org.example demo 1.0.0 $hex central
           |""".stripMargin
      val javaSeries = "cell core 3.3.7 jvm\ndirect org.example demo 1.0.0 java\nseries 2\n"
      val twice      = "cell core 3.3.7 jvm\ndirect org.example demo 1.0.0 binary\nseries 2\nseries 1\n"
      val loose      = "exclude org.example other\n"
      val early      = "cell core 3.3.7 jvm\nexclude org.example other\n"
      val badOrg     = "cell core 3.3.7 jvm\ndirect org.example demo 1.0.0 java\nexclude 1bad other\n"
      val badSeries  = "cell core 3.3.7 jvm\ndirect org.example demo 1.0.0 binary\nseries 2.x\n"
      val bare       = DependencyLock.parse(text)
      val parsed     = DependencyLock.parse(marked)
      (bare, parsed) match
        case (Right(plain), Right(lock)) =>
          val empty = plain.cells.flatMap(_.directs)
          val held  = lock.cells.flatMap(_.directs)
          assertTrue(
            empty.map(direct => (direct.exclusions, direct.sbtSeries)) == List((Nil, None)),
            held.map(direct =>
              (
                direct.exclusions.map(item => (item.organization.value, item.artifact.value)),
                direct.sbtSeries.map(_.value),
              )
            ) == List((List(("org.example", "other"), ("com.acme", "gone")), Some("2"))),
            DependencyLock.parse(DependencyLock.render(lock)) == Right(lock),
            DependencyLock.parse(javaSeries) == Left(LockError.BadLine("series 2")),
            DependencyLock.parse(twice) == Left(LockError.BadLine("series 1")),
            DependencyLock.parse(loose) == Left(LockError.Missing(LockField.Direct)),
            DependencyLock.parse(early) == Left(LockError.BadLine("exclude org.example other")),
            DependencyLock.parse(badOrg) == Left(LockError.BadLine("exclude 1bad other")),
            DependencyLock.parse(badSeries) == Left(LockError.BadLine("series 2.x")),
          )
        case _ => assertTrue(false),
  )

  private def shaOf(lock: DependencyLock): Option[Sha256] =
    lock.cells match
      case cell :: Nil =>
        cell.artifacts match
          case artifact :: Nil => Some(artifact.sha256)
          case _               => None
      case _ => None
end LockLawSpec
