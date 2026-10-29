package histos

import histos.jdk.{Sha256, Sha256Error}
import histos.js.JsKey
import histos.pack.MainClass as BinaryName
import histos.publish.{Bundle, BundleError, BundlePath, Staged}
import histos.select.{SelectError, Selected, Selection}
import histos.testrun.{SuitePattern, SuitePatternError}
import histos.zinc.{ScalaSeries, SourceFilter}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, ZIO}
import zio.test.*

/** Laws read off the key, the source filter, the suite glob, the bundle refusal, and selection. */
object AlgebraSpec extends SuiteClock:
  private val scala3  = ScalaVersion("3.10.0")
  private val labels  = List("a", "b", "c", "d")
  private val at      = SourcePos("histos.scala", 1, 1)
  private val root    = ProjectName("root")
  private val inner   = ProjectName("inner")
  private val missing = ProjectName("missing")

  def spec = suite("algebra")(
    digests,
    filters,
    globs,
    bundles,
    selection,
    linkKeys,
  )

  private def digests =
    suite("sha256")(
      test("a digest is 64 hex digits, and anything else is the text that was offered"):
        check(offered): raw =>
          val parsed = Sha256.from(raw)
          if hex(raw) then
            val normalized = raw.trim.toLowerCase(java.util.Locale.ROOT)
            assertTrue(parsed.map(_.value) == Right(normalized), Sha256.from(normalized) == parsed)
          else assertTrue(parsed == Left(Sha256Error.Bad(raw)), Sha256Error.Bad(raw).message.contains(raw))
      ,
      test("hashing bytes is sha256, and the same bytes hash the same"):
        check(Gen.listOfBounded(0, 48)(Gen.int(0, 255))): ints =>
          val bytes = Chunk.fromIterable(ints.map(_.toByte))
          val hash  = Sha256.of(bytes)
          assertTrue(
            hash == Sha256.of(bytes),
            Sha256.from(hash.value) == Right(hash),
            hash.value.length == 64,
          )
      ,
      test("no bytes hash to the empty sha256"):
        assertTrue(Sha256.of(Chunk.empty).value == emptySha),
    )

  private def filters =
    suite("source filters")(
      test("a directory filter allows the platform, the series, or the exact version it names"):
        check(Gen.elements(2, 3), Gen.int(0, 30), Gen.int(0, 30), Gen.int(0, 30), platforms):
          (major, minor, patch, otherPatch, platform) =>
            version(s"$major.$minor.$patch") match
              case Left(_)      => assertTrue(false)
              case Right(exact) =>
                version(s"$major.$minor.$otherPatch") match
                  case Left(_)        => assertTrue(false)
                  case Right(sibling) =>
                    val series = if major == 3 then ScalaSeries.Scala3 else ScalaSeries.Scala2(minor)
                    assertTrue(
                      SourceFilter.allows(SourceFilter.PlatformDir(platform), exact, platform),
                      !SourceFilter.allows(SourceFilter.PlatformDir(other(platform)), exact, platform),
                      SourceFilter.allows(SourceFilter.SeriesDir(series), exact, platform),
                      SourceFilter.allows(SourceFilter.ExactDir(exact), exact, platform),
                      SourceFilter.allows(SourceFilter.ExactDir(exact), sibling, platform) == (patch == otherPatch),
                      SourceFilter.parse(platform.token).contains(SourceFilter.PlatformDir(platform)),
                    )
      ,
      test("a directory name is a filter only when it is a platform, a series, or a full version"):
        check(Gen.string): name =>
          assertTrue(SourceFilter.parse(name) == namedFilter(name)),
    )

  private def globs =
    suite("suite globs")(
      test("no pattern allows every suite name"):
        check(Gen.string): name =>
          assertTrue(SuitePattern.allows(Nil, name))
      ,
      test("a pattern is a glob, and a trailing module dollar is the same suite"):
        check(patterns, suiteNames): (raw, name) =>
          SuitePattern.from(raw) match
            case Left(_)        => assertTrue(false)
            case Right(pattern) =>
              assertTrue(
                pattern.value == raw,
                SuitePattern.allows(List(pattern), name) == (glob(raw, shown(name)) || glob(raw, name)),
              )
      ,
      test("an empty pattern is empty, and whitespace is the text that was offered"):
        check(Gen.string): raw =>
          SuitePattern.from(raw) match
            case Left(SuitePatternError.Empty) =>
              assertTrue(raw.isEmpty, SuitePatternError.Empty.message.nonEmpty)
            case Left(SuitePatternError.Blank(found)) =>
              assertTrue(found == raw, raw.exists(_.isWhitespace), SuitePatternError.Blank(found).message.contains(raw))
            case Right(pattern) =>
              assertTrue(pattern.value == raw, raw.nonEmpty, !raw.exists(_.isWhitespace)),
    )

  private def bundles =
    suite("bundle refusals")(
      test("a bundle path is relative, and an escape is the text that was offered"):
        check(paths): raw =>
          val name = raw.replace('\\', '/')
          BundlePath.from(raw) match
            case Right(path) => assertTrue(legal(name), path.name == name)
            case Left(error) => assertTrue(!legal(name), error == BundleError.Escaped(raw))
      ,
      test("an empty stage, a negative size, a duplicate name, or a total over the cap is refused"):
        check(Gen.int(1, 8)): extra =>
          val path = BundlePath.from("com/example/a.jar")
          path match
            case Left(error) => ZIO.succeed(assertTrue(error == BundleError.Empty))
            case Right(jar)  =>
              val again = Staged(jar, 1L, ZIO.dieMessage("unread"))
              val one   = Staged(jar, 1L, ZIO.dieMessage("unread"))
              for
                empty    <- refuse(Chunk.empty, BundleError.Empty)
                negative <- refuse(
                  Chunk(Staged(jar, -extra.toLong, ZIO.dieMessage("unread"))),
                  BundleError.Size(-extra.toLong),
                )
                duplicate <- refuse(Chunk(one, again), BundleError.Duplicate(jar.name))
                over = Bundle.Cap + extra.toLong
                huge <- refuse(Chunk(Staged(jar, over, ZIO.dieMessage("unread"))), BundleError.TooLarge(over))
              yield empty && negative && duplicate && huge,
    )

  private def selection =
    suite("selection")(
      test("empty names are every module, one name is that module, and an aggregate is its module children"):
        check(moduleSets, Gen.int(0, 15)): (chosen, childBits) =>
          ZIO
            .fromEither(graphOf(chosen))
            .map: graph =>
              val modules  = graph.projects.collect { case item: Module => item }
              val children = modules.zipWithIndex.collect:
                case (item, index) if (childBits & (1 << index)) != 0 => item
              val widened = ProjectGraph.trusted(graph.projects :+ Aggregate(root, children.map(_.name), at))
              val nested  = ProjectGraph.trusted(
                graph.projects ++ List(Aggregate(inner, Nil, at), Aggregate(root, List(inner), at))
              )
              val named = modules.forall: item =>
                Selection.select(graph, List(item.name)) == Right(Selected(List(item), Set.empty)) &&
                  Selection.select(graph, List(item.name, item.name)) == Left(SelectError.Duplicate(item.name)) &&
                  SelectError.Duplicate(item.name).message.contains(item.name.value)
              assertTrue(
                modules.nonEmpty,
                named,
                Selection.select(graph, Nil) == Right(Selected(modules, Set.empty)),
                Selection.select(graph, List(missing)) == Left(SelectError.Unknown(missing)),
                SelectError.Unknown(missing).message.contains(missing.value),
                Selection.select(widened, List(root)) == Right(Selected(children, children.map(_.name).toSet)),
                Selection.select(widened, List(root, root)) == Left(SelectError.Duplicate(root)),
                SelectError.Duplicate(root).message.contains(root.value),
                Selection.select(nested, List(root)) == Left(SelectError.Unknown(inner)),
                SelectError.Unknown(inner).message.contains(inner.value),
              )
    )

  private def linkKeys =
    suite("link keys")(
      test("a link key is the digest of the version, the linker, the kind, the main, and the product digests"):
        check(scalaVersions, jsVersions, kinds, mains, productDigests): (version, linker, kind, main, products) =>
          val text = linkText(version, linker, kind, main, products)
          assertTrue(
            JsKey.of(version, linker, kind, main, products) ==
              Sha256.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))
          )
    )

  private val emptySha = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  private val hexDigits =
    Gen.elements(
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F',
    )

  private val offered: Gen[Any, String] =
    Gen.oneOf(
      Gen.listOfN(64)(hexDigits).map(_.mkString),
      Gen.listOfBounded(0, 80)(hexDigits).map(chars => s"  ${chars.mkString} "),
      Gen.string,
    )

  private val platforms =
    Gen.elements(Platform.Jvm, Platform.Js, Platform.Native)

  private val patternChars =
    Gen.elements('a', 'b', 'c', '*', '?', '.', '+')

  private val patterns =
    Gen.listOfBounded(1, 5)(patternChars).map(_.mkString)

  private val nameChars =
    Gen.elements('a', 'b', 'c', 'x', '$')

  private val suiteNames =
    Gen.listOfBounded(0, 6)(nameChars).map(_.mkString)

  private val pathParts =
    Gen.elements("a", "b", ".", "..", "", "c")

  private val paths: Gen[Any, String] =
    Gen.oneOf(
      Gen.listOfBounded(0, 4)(pathParts).map(_.mkString("/")),
      Gen.listOfBounded(0, 4)(pathParts).map(_.mkString("\\")),
      Gen.string,
    )

  private def hex(raw: String): Boolean =
    val text = raw.trim.toLowerCase(java.util.Locale.ROOT)
    text.length == 64 && text.forall(digit => (digit >= '0' && digit <= '9') || (digit >= 'a' && digit <= 'f'))

  private def version(raw: String): Either[ScalaVersionError, ScalaVersion] =
    ScalaVersion.from(raw)

  private def other(platform: Platform): Platform =
    platform match
      case Platform.Jvm    => Platform.Js
      case Platform.Js     => Platform.Native
      case Platform.Native => Platform.Jvm

  private def namedFilter(name: String): Option[SourceFilter] =
    name match
      case "jvm" | "scalajvm"       => Some(SourceFilter.PlatformDir(Platform.Jvm))
      case "js" | "scalajs"         => Some(SourceFilter.PlatformDir(Platform.Js))
      case "native" | "scalanative" => Some(SourceFilter.PlatformDir(Platform.Native))
      case "scala-3"                => Some(SourceFilter.SeriesDir(ScalaSeries.Scala3))
      case other                    =>
        val prefix = "scala-"
        if !other.startsWith(prefix) then None
        else
          val rest = other.drop(prefix.length)
          if rest.startsWith("2.") && rest.drop(2).toIntOption.nonEmpty && !rest.drop(2).contains('.') then
            rest.drop(2).toIntOption.map(minor => SourceFilter.SeriesDir(ScalaSeries.Scala2(minor)))
          else
            ScalaVersion.from(rest) match
              case Right(exact) => Some(SourceFilter.ExactDir(exact))
              case Left(_)      => None

  private def shown(name: String): String =
    if name.endsWith("$") && !name.endsWith("$$") then name.dropRight(1) else name

  private def glob(pattern: String, name: String): Boolean =
    def step(left: List[Char], right: List[Char]): Boolean =
      (left, right) match
        case (Nil, Nil)       => true
        case ('*' :: rest, _) =>
          step(rest, right) || (right match
            case Nil       => false
            case _ :: more => step(left, more))
        case ('?' :: rest, _ :: more)                     => step(rest, more)
        case (char :: rest, head :: more) if char == head => step(rest, more)
        case _                                            => false
    step(pattern.toList, name.toList)

  private def legal(name: String): Boolean =
    name.nonEmpty &&
      !name.startsWith("/") &&
      name.split("/", -1).forall(part => part.nonEmpty && part != "." && part != "..")

  private val sink = Path.of("/tmp/histos-algebra-unused.zip")

  private val moduleSets: Gen[Any, List[String]] =
    Gen
      .int(1, 15)
      .map: mask =>
        labels.zipWithIndex.collect:
          case (label, index) if (mask & (1 << index)) != 0 => label

  private val revisions =
    for
      major <- Gen.int(0, 4)
      minor <- Gen.int(0, 4)
      patch <- Gen.int(0, 4)
    yield s"$major.$minor.$patch"

  private val scalaVersions =
    revisions.map(ScalaVersion.trusted)

  private val jsVersions =
    revisions.map(JsVersion.trusted)

  private val kinds =
    Gen.elements(JsModuleKind.NoModule, JsModuleKind.ESModule, JsModuleKind.CommonJS)

  private val mains =
    Gen.elements("demo.Hello", "demo.Other", "a", "a.B").map(BinaryName.trusted)

  private val productDigests =
    Gen.listOfBounded(0, 4)(
      Gen.listOfBounded(0, 8)(Gen.int(0, 255)).map(ints => Sha256.of(Chunk.fromIterable(ints.map(_.toByte))))
    )

  private def graphOf(labels: List[String]): Either[BuildError, ProjectGraph] =
    Projects(labels.map(label => module(label, Jvm)(scala3))*)

  private def linkText(
      version: ScalaVersion,
      linker: JsVersion,
      kind: JsModuleKind,
      main: BinaryName,
      products: List[Sha256],
  ): String =
    s"""${version.value}
       |${linker.value}
       |${kind.token}
       |${main.binary}
       |products
       |${products.map(_.value).mkString("\n")}
       |""".stripMargin

  private def refuse(files: Chunk[Staged], error: BundleError) =
    for
      got <- Bundle.zip(files, sink).either
      _   <- ZIO.attemptBlocking(Files.deleteIfExists(sink)).ignore
    yield assertTrue(got == Left(error))
end AlgebraSpec
