package histos.resolve

import histos.jdk.Sha256
import histos.plugin.LinkStrategy
import histos.{ArtifactBase, DepExclusion, DepKind, Organization, Platform, ProjectName, SbtSeries, ScalaVersion}

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

  private val token    = Gen.alphaNumericStringBounded(1, 6)
  private val hexDigit = Gen.elements("0123456789abcdef".toList*)
  private val sha256   = Gen.stringN(64)(hexDigit).map(Sha256.trusted)
  private val repo     =
    Gen
      .elements("central", "ivy-local", "maven:https://repo.example/maven2/")
      .map(Repository.parse)
      .collect:
        case Right(found) => found
  private val version  = Gen.elements(ScalaVersion("3.3.7"), ScalaVersion("2.13.18"))
  private val eviction = (token <*> token <*> token <*> token).map(Eviction(_, _, _, _))
  private val excluded = Gen.elements(
    DepExclusion(Organization("org.example"), ArtifactBase("other")),
    DepExclusion(Organization("com.acme"), ArtifactBase("gone")),
  )

  private val direct =
    for
      organization <- token
      name         <- token
      revision     <- token
      kind         <- Gen.elements(DepKind.Java, DepKind.Binary, DepKind.Full)
      exclusions   <- Gen.listOfBounded(0, 2)(excluded)
      series       <- Gen.option(Gen.elements(SbtSeries("1"), SbtSeries("2")))
    yield LockedDirect(organization, name, revision, kind, exclusions, series.filter(_ => kind == DepKind.Binary))

  private val artifact =
    for
      organization <- token
      name         <- token
      revision     <- token
      sha          <- sha256
      repository   <- repo
      classifier   <- Gen.option(Gen.elements(JarClassifier("jdk8"), JarClassifier("shaded")))
    yield LockedArtifact(organization, name, revision, sha, repository, classifier)

  private val cell =
    for
      module    <- Gen.elements(ProjectName("core"), ProjectName("app"))
      at        <- version
      platform  <- Gen.fromIterable(Platform.values)
      scope     <- Gen.fromIterable(ClasspathScope.values)
      directs   <- Gen.listOfBounded(0, 2)(direct)
      artifacts <- Gen.listOfBounded(0, 2)(artifact)
      evictions <- Gen.listOfBounded(0, 2)(eviction)
    yield LockedCell(module, at, platform, directs, artifacts, evictions, scope)

  private val plugin =
    for
      organization <- token
      name         <- token
      revision     <- token
      link         <- Gen.elements(LinkStrategy.ExactBinary, LinkStrategy.Source, LinkStrategy.BinaryOrSource)
      directs      <- Gen.listOfBounded(0, 2)(direct)
      artifacts    <- Gen.listOfBounded(0, 2)(artifact)
      source       <- artifact
    yield LockedPlugin(
      organization,
      name,
      revision,
      link,
      directs,
      artifacts,
      Option.when(link != LinkStrategy.ExactBinary)(source),
    )

  private val tool =
    for
      role    <- Gen.fromIterable(ToolRole.values)
      at      <- version
      variant <- Gen.elements(ToolVariant.Jvm, ToolVariant.Node, ToolVariant.Jsdom, ToolVariant.Firefox)
      roots   <- Gen.listOfBounded(0, 2)(
        (token <*> token <*> token <*> Gen.fromIterable(Transitivity.values) <*> Gen.option(repo))
          .map(ToolRoot(_, _, _, _, _))
      )
      resolvers <- Gen.listOfBounded(0, 2)(repo)
      artifacts <- Gen.listOfBounded(0, 2)(artifact)
      evictions <- Gen.listOfBounded(0, 2)(eviction)
    yield LockedTool(ToolId(role, at, variant), roots, resolvers, artifacts, evictions)

  private val lock =
    for
      cells   <- Gen.listOfBounded(1, 3)(cell)
      plugins <- Gen.listOfBounded(0, 2)(plugin)
      tools   <- Gen.listOfBounded(0, 2)(tool)
    yield DependencyLock(
      cells.distinctBy(found => (found.module, found.scalaVersion, found.platform, found.scope)),
      plugins.distinctBy(found => (found.organization, found.name, found.revision)),
      tools.distinctBy(_.id),
    )

  def spec = suite("lock laws")(
    test("a lock with cells, plugin rows, tool rows, and evictions parses back from its rendering"):
      check(lock): written =>
        assertTrue(DependencyLock.parse(DependencyLock.render(written)) == Right(written))
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
    test("a test header is a test cell, a tool header is a tool row, and a source before a plugin is a missing plugin"):
      val testCell = s"test core 3.3.7 js\nartifact org.example demo 1.0.0 $hex central\n"
      val tool     = s"tool compiler 3.3.7 jvm\nroot org.example demo 1.0.0 closure central\n"
      val source   = s"source org.example demo 1.0.0 $hex central\n"
      assertTrue(
        DependencyLock.parse(testCell).map(_.cells.map(_.scope)) == Right(List(ClasspathScope.Test)),
        DependencyLock.parse(tool).map(_.tools.map(_.id)) ==
          Right(List(ToolId(ToolRole.Compiler, scala3, ToolVariant.Jvm))),
        DependencyLock.parse(source) == Left(LockError.Missing(LockField.Plugin)),
      )
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
