package histos

import zio.ZIO
import zio.test.*

/** Laws read off the graph and the parsers. A closure is the named modules and their upstreams. A parser is its
  * predicate.
  */
object AlgebraSpec extends SuiteClock:
  private val scala3 = ScalaVersion("3.10.0")
  private val labels = List("a", "b", "c", "d")
  private val full   = """\d+\.\d+\.\d+([-+][0-9A-Za-z.-]+)?""".r

  def spec = suite("algebra")(
    test("closure is the named modules and their upstreams, in module order"):
      check(graphs, Gen.int(0, 31)): (shape, rootBits) =>
        val (labels, edges) = shape
        ZIO
          .fromEither(modules(labels, edges))
          .map: found =>
            val roots = rootSet(labels, rootBits)
            val once  = Module.closure(found, roots)
            val twice = Module.closure(found, once.map(_.name).toSet)
            assertTrue(
              once.map(_.name.value) == expected(found, roots),
              twice.map(_.name) == once.map(_.name),
            )
    ,
    test("a project name is a letter, then letters, digits, underscores, or dashes"):
      check(projectNames): raw =>
        val parsed = ProjectName.from(raw)
        if raw.isEmpty then assertTrue(parsed == Left(ProjectNameError.Empty))
        else if projectName(raw) then assertTrue(parsed.map(_.value) == Right(raw))
        else assertTrue(parsed == Left(ProjectNameError.Bad(raw)), ProjectNameError.Bad(raw).message.contains(raw))
    ,
    test("a scala version is major.minor.patch, and anything else is the text that was offered"):
      check(revisions): raw =>
        val parsed = ScalaVersion.from(raw)
        if raw.isEmpty then assertTrue(parsed == Left(ScalaVersionError.Empty))
        else if full.matches(raw) then assertTrue(parsed.map(_.value) == Right(raw))
        else
          assertTrue(
            parsed == Left(ScalaVersionError.NotFull(raw)),
            ScalaVersionError.NotFull(raw).message.contains(raw),
          )
    ,
    test("a javascript toolchain revision is exact, and anything else is the text that was offered"):
      check(revisions): raw =>
        val parsed = JsVersion.from(raw)
        if raw.isEmpty then assertTrue(parsed == Left(JsVersionError.Empty))
        else if full.matches(raw) then assertTrue(parsed.map(_.value) == Right(raw))
        else
          assertTrue(
            parsed == Left(JsVersionError.NotExact(raw)),
            JsVersionError.NotExact(raw).message.contains(raw),
          )
    ,
    test("a platform is jvm, js, or native"):
      check(platformTokens): raw =>
        val parsed = Platform.from(raw)
        raw match
          case "jvm"    => assertTrue(parsed == Right(Platform.Jvm))
          case "js"     => assertTrue(parsed == Right(Platform.Js))
          case "native" => assertTrue(parsed == Right(Platform.Native))
          case other    =>
            assertTrue(
              parsed == Left(PlatformError.Unknown(other)),
              PlatformError.Unknown(other).message.contains(other),
            )
    ,
    test("a clang major is a positive integer, and anything else is the integer that was offered"):
      check(Gen.int): raw =>
        val parsed = ClangMajor.from(raw)
        if raw > 0 then assertTrue(parsed.map(_.value) == Right(raw))
        else
          assertTrue(
            parsed == Left(ClangError.NotPositive(raw)),
            ClangError.NotPositive(raw).message.contains(raw.toString),
          ),
  )

  private val graphs: Gen[Any, (List[String], List[(String, String)])] =
    for
      mask <- Gen.int(1, 15)
      bits <- Gen.int(0, (1 << 12) - 1)
    yield
      val chosen = labels.zipWithIndex.collect:
        case (label, index) if (mask & (1 << index)) != 0 => label
      val pairs = chosen.zipWithIndex.flatMap: (from, fromIndex) =>
        chosen.zipWithIndex.collect:
          case (to, toIndex) if toIndex < fromIndex => from -> to
      val edges = pairs.zipWithIndex.collect:
        case (edge, index) if (bits & (1 << index)) != 0 => edge
      (chosen, edges)

  private def modules(labels: List[String], edges: List[(String, String)]): Either[BuildError, List[Module]] =
    val bare   = labels.map(label => label -> module(label, Jvm)(scala3)).toMap
    val linked = labels.flatMap: label =>
      bare
        .get(label)
        .map: item =>
          val upstream = edges.flatMap:
            case (from, to) if from == label => bare.get(to).toList
            case _                           => Nil
          upstream.foldLeft(item)((acc, dep) => acc.dependsOn(dep))
    Projects(linked*).map(_.projects.collect { case item: Module => item })

  private def rootSet(labels: List[String], bits: Int): Set[ProjectName] =
    val chosen = labels.zipWithIndex.flatMap: (label, index) =>
      if (bits & (1 << index)) != 0 then named(label) else Nil
    (chosen ++ named("missing")).toSet

  private val nameBody =
    Gen.elements('a', 'b', 'c', 'A', 'Z', '1', '_', '-')

  private val projectNames: Gen[Any, String] =
    Gen.oneOf(
      Gen.string,
      for
        head <- Gen.elements('a', 'b', 'c', 'A', 'Z')
        tail <- Gen.listOfBounded(0, 8)(nameBody)
      yield (head :: tail).mkString,
    )

  private val numbered =
    for
      major <- Gen.int(0, 9)
      minor <- Gen.int(0, 9)
      patch <- Gen.int(0, 9)
    yield s"$major.$minor.$patch"

  private val revisions: Gen[Any, String] =
    Gen.oneOf(numbered, Gen.string)

  private val platformTokens: Gen[Any, String] =
    Gen.oneOf(Gen.elements("jvm", "js", "native"), Gen.string)

  private def projectName(raw: String): Boolean =
    raw.headOption match
      case Some(head) =>
        head.isLetter && raw.forall(char => char.isLetterOrDigit || char == '_' || char == '-')
      case None => false

  private def named(raw: String): List[ProjectName] =
    ProjectName.from(raw) match
      case Right(name) => List(name)
      case Left(_)     => Nil

  private def expected(modules: List[Module], roots: Set[ProjectName]): List[String] =
    val closed = reachable(modules, roots)
    modules.map(_.name).filter(closed.contains).map(_.value)

  private def reachable(modules: List[Module], roots: Set[ProjectName]): Set[ProjectName] =
    val known = modules.map(module => module.name -> module.upstreamNames).toMap
    def grow(have: Set[ProjectName]): Set[ProjectName] =
      val extra = have.flatMap(name => known.get(name).toList.flatten)
      if extra.subsetOf(have) then have else grow(have ++ extra)
    grow(roots.intersect(known.keySet))
end AlgebraSpec
