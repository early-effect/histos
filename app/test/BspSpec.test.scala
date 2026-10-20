package histos.bsp

import histos.cli.{Cli, Command}
import histos.resolve.Resolve
import histos.zinc.GraphCompile
import histos.{BuildError, Js, Jvm, Module, ProjectGraph, ProjectName, Projects, ScalaVersion, module}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.json.ast.Json
import zio.{Ref, Scope, ZIO, durationInt}
import zio.test.*

object BspSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val scala2 = ScalaVersion("2.13.16")

  private enum FixtureFault:
    case Shape, Discovery

    def message: String = this match
      case Shape     => "the bsp result did not have the expected shape"
      case Discovery => "no .bsp/histos.json under HISTOS_REPO"

  private val protocol = suite("protocol")(
    test("bsp is a command"):
      Cli.parse(List("bsp")).map(parsed => assertTrue(parsed == Command.Bsp)),
    test("discovery names ./histos bsp"):
      discovery.map: json =>
        assertTrue(
          string(json, "name").contains(Bsp.ServerName),
          string(json, "version").contains(Bsp.ServerVersion),
          string(json, "bspVersion").contains(Bsp.BspVersion),
          strings(json, "argv") == List("./histos", "bsp"),
          strings(json, "languages") == List("scala"),
        ),
    test("a frame round-trips payloads, including a newline"):
      val first = "{\"method\":\"build/initialized\"}\r\n"
      val second = "{\"id\":1}"
      for
        bytes <- framed(first, second)
        back  <- readAll(new ByteArrayInputStream(bytes))
      yield assertTrue(back == List(first, second)),
    test("a frame needs content-length and crlf, and a clean end is no message"):
      val typed = "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}"
      val bare = "Content-Length: 2\n\n{}"
      val short = "Content-Length: 5\r\n\r\nab"
      for
        missing <- Frame.read(stream(typed)).either
        lf      <- Frame.read(stream(bare)).either
        cut     <- Frame.read(stream(short)).either
        empty   <- Frame.read(new ByteArrayInputStream(Array.emptyByteArray))
      yield assertTrue(
        missing == Left(Frame.FrameError.MissingLength),
        lf == Left(Frame.FrameError.Truncated),
        cut == Left(Frame.FrameError.Truncated),
        empty.isEmpty,
      ),
    test("requests wait for initialize, and exit follows shutdown"):
      val run =
        for
          early    <- opened
          quiet    <- early.answer(notify("build/initialized"))
          denied   <- early.answer(request("workspace/buildTargets", 1))
          stopped  <- early.answer(notify("build/exit"))
          bad      <- early.answer("[1]")
          junk     <- opened.flatMap(_.answer("not json"))
          orderly  <- opened
          init     <- orderly.answer(request("build/initialize", 1))
          _        <- orderly.answer(notify("build/initialized"))
          unknown  <- orderly.answer(request("buildTarget/test", 2))
          down     <- orderly.answer(request("build/shutdown", 3))
          done     <- orderly.answer(notify("build/exit"))
        yield assertTrue(
          quiet == Bsp.Answer.Quiet,
          code(denied).contains(-32002),
          message(denied).exists(_.contains("not initialized")),
          stopped == Bsp.Answer.Stop(1),
          code(bad).contains(-32600),
          code(junk).contains(-32700),
          resultOf(init).flatMap(string(_, "displayName")).contains(Bsp.ServerName),
          resultOf(init).flatMap(string(_, "bspVersion")).contains(Bsp.BspVersion),
          caps(init).flatMap(field(_, "compileProvider")).isDefined,
          caps(init).flatMap(field(_, "testProvider")).isEmpty,
          caps(init).flatMap(field(_, "runProvider")).isEmpty,
          code(unknown).contains(-32601),
          message(unknown).exists(_.contains("buildTarget/test")),
          resultOf(down).contains(Json.Null),
          done == Bsp.Answer.Stop(0),
        )
      run.provide(Resolve.live(Path.of(java.lang.System.getProperty("java.io.tmpdir")).resolve("histos-bsp-spec"))),
  )

  private val client = suite("client")(
    test("metals compiles the scala 3 cell and leaves the other version alone"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(scala3, scala2), module("ui", Js)(scala3)))
              _ <- ZIO.attempt {
                     write(root, "demo/src/Hello.scala", "object Hello {\n  def n: Int = 1\n}\n")
                     write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\ncell demo 2.13.16 jvm\n")
                   }
              demo <- moduleName(built, "demo")
              jdk <- GraphCompile.jdkHere
              open <- Bsp.open(root, built, jdk)
              _ <- open.answer(request("build/initialize", 1))
              listed <- open.answer(request("workspace/buildTargets", 2))
              uri <- one(listed, "3.10.0")
              sources <- open.answer(targeted("buildTarget/sources", 3, uri))
              options <- open.answer(targeted("buildTarget/scalacOptions", 4, uri))
              compiled <- open.answer(targeted("buildTarget/compile", 5, uri, Some("metals-1")))
              classes = GraphCompile.classesDir(root, demo, scala3)
              other = GraphCompile.classesDir(root, demo, scala2)
              source = root.resolve("demo").resolve("src").resolve("Hello.scala").toAbsolutePath.normalize
              found <- ZIO.attempt(entries(classes))
              otherGone <- ZIO.attempt(!Files.exists(other))
              flagsOn = scalacFlags(options)
            yield assertTrue(
              displayNames(listed) == List("demo", "demo", "histos.scala"),
              versions(listed) == List("2.13.16", "3.10.0", "3.10.0"),
              binaries(listed) == List("2.13", "3", "3"),
              platforms(listed) == List(1, 1, 1),
              tags(listed) == List(List("library"), List("library"), List("library")),
              jars(listed, "3.10.0").exists(_.contains("scala-library")),
              jars(listed, "3.10.0").exists(_.contains("scala3-compiler")),
              jars(listed, "2.13.16").exists(_.contains("scala-library")),
              jars(listed, "2.13.16").exists(_.contains("scala-compiler")),
              flags(listed, "canCompile") == List(true, true, true),
              flags(listed, "canTest") == List(false, false, false),
              flags(listed, "canRun") == List(false, false, false),
              uri.contains("platform=jvm"),
              uri.contains("scope=main"),
              sourcePaths(sources) == List(source),
              sourceKinds(sources) == List(1),
              generated(sources) == List(false),
              classpath(options).exists(_.contains("scala-library")),
              flagsOn.contains("-Xsemanticdb"),
              flagsOn.contains("-sourceroot"),
              classDirs(options) == List(classes.toAbsolutePath.normalize),
              status(compiled).contains(1),
              origin(compiled).contains("metals-1"),
              code(compiled).isEmpty,
              found.contains("Hello$.class"),
              found.contains("Hello.class"),
              found.contains("Hello.tasty"),
              found.exists(_.contains("semanticdb")),
              otherGone,
            )
          program.provide(Resolve.live(root.resolve("coursier"))),
    test("an unknown target and a missing lock are protocol errors"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(scala3)))
              jdk <- GraphCompile.jdkHere
              open <- Bsp.open(root, built, jdk)
              _ <- open.answer(request("build/initialize", 1))
              listed <- open.answer(request("workspace/buildTargets", 2))
              uri <- one(listed, "3.10.0")
              unknown <- open.answer(targeted("buildTarget/sources", 3, "file:///nope"))
              missing <- open.answer(targeted("buildTarget/scalacOptions", 4, uri))
            yield assertTrue(
              code(unknown).contains(-32602),
              message(unknown).exists(_.contains("unknown build target")),
              code(missing).contains(-32603),
              message(missing).exists(_.contains("no dependency lock")),
            )
          program.provide(Resolve.live(root.resolve("coursier"))),
    test("a compile failure publishes the range and then status 2"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(scala3)))
              _ <- ZIO.attempt {
                     write(root, "demo/src/Hello.scala", "object Hello {\n  def n: Int = missing\n}\n")
                     write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
                   }
              notes <- Ref.make(List.empty[String])
              jdk <- GraphCompile.jdkHere
              open <- Bsp.open(root, built, jdk, Nil, line => notes.update(_ :+ line))
              _ <- open.answer(request("build/initialize", 1))
              listed <- open.answer(request("workspace/buildTargets", 2))
              uri <- one(listed, "3.10.0")
              compiled <- open.answer(targeted("buildTarget/compile", 3, uri))
              seen <- notes.get
            yield assertTrue(
              status(compiled).contains(2),
              seen.exists(_.contains(""""reset":true""")),
              seen.exists(line => line.contains("build/publishDiagnostics") && line.contains("missing") && line.contains(""""line":1""")),
            )
          program.provide(Resolve.live(root.resolve("coursier"))),
    test("a compile failure is status 2"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(scala3)))
              _ <- ZIO.attempt {
                     write(root, "demo/src/Hello.scala", "object Hello {\n  def n: Int = missing\n}\n")
                     write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
                   }
              jdk <- GraphCompile.jdkHere
              open <- Bsp.open(root, built, jdk)
              _ <- open.answer(request("build/initialize", 1))
              listed <- open.answer(request("workspace/buildTargets", 2))
              uri <- one(listed, "3.10.0")
              compiled <- open.answer(targeted("buildTarget/compile", 3, uri))
            yield assertTrue(status(compiled).contains(2), code(compiled).isEmpty)
          program.provide(Resolve.live(root.resolve("coursier"))),
    test("a module with no source root lists no sources"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(scala3)))
              jdk <- GraphCompile.jdkHere
              open <- Bsp.open(root, built, jdk)
              _ <- open.answer(request("build/initialize", 1))
              listed <- open.answer(request("workspace/buildTargets", 2))
              uri <- one(listed, "3.10.0")
              sources <- open.answer(targeted("buildTarget/sources", 3, uri))
            yield assertTrue(sourcePaths(sources).isEmpty, code(sources).isEmpty)
          program.provide(Resolve.live(root.resolve("coursier"))),
  )

  private def opened: ZIO[Any, BuildError | histos.zinc.GraphCompileError, Bsp.Session] =
    for
      built <- ZIO.fromEither(Projects(module("demo", Jvm)(scala3)))
      jdk   <- GraphCompile.jdkHere
      open  <- Bsp.open(Path.of("."), built, jdk)
    yield open

  private def moduleName(graph: ProjectGraph, name: String): ZIO[Any, FixtureFault, ProjectName] =
    graph.projects.collect { case item: Module if item.name.value == name => item.name } match
      case found :: Nil => ZIO.succeed(found)
      case _            => ZIO.fail(FixtureFault.Shape)

  private def one(answer: Bsp.Answer, version: String): ZIO[Any, FixtureFault, String] =
    targetUris(answer).filter(_.contains(s"scala=$version")) match
      case uri :: Nil => ZIO.succeed(uri)
      case _          => ZIO.fail(FixtureFault.Shape)

  private def discovery: ZIO[Any, FixtureFault, Json] =
    val found = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(value => Path.of(value).resolve(".bsp").resolve("histos.json"))
    found match
      case Some(path) =>
        ZIO.attempt(Files.readString(path)).mapError(_ => FixtureFault.Discovery).flatMap: text =>
          ZIO.fromEither(text.fromJson[Json].left.map(_ => FixtureFault.Discovery))
      case None => ZIO.fail(FixtureFault.Discovery)

  private def framed(payloads: String*): ZIO[Any, Frame.FrameError, Array[Byte]] =
    val out = new ByteArrayOutputStream
    ZIO.foreachDiscard(payloads)(payload => Frame.write(out, payload)).as(out.toByteArray)

  private def readAll(in: ByteArrayInputStream): ZIO[Any, Frame.FrameError, List[String]] =
    Frame.read(in).flatMap:
      case None          => ZIO.succeed(Nil)
      case Some(payload) => readAll(in).map(payload :: _)

  private def stream(text: String): ByteArrayInputStream =
    new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8))

  private def request(method: String, id: Int, params: Json = Json.Obj()): String =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id" -> Json.Num(id),
      "method" -> Json.Str(method),
      "params" -> params,
    ).toJson

  private def notify(method: String): String =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "method" -> Json.Str(method)).toJson

  private def targeted(method: String, id: Int, uri: String, origin: Option[String] = None): String =
    val extra = origin.map(value => "originId" -> Json.Str(value)).toList
    request(method, id, Json.Obj((("targets" -> Json.Arr(Json.Obj("uri" -> Json.Str(uri)))) :: extra)*))

  private def parsed(answer: Bsp.Answer): Option[Json] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Json].toOption
      case _                      => None

  private def resultOf(answer: Bsp.Answer): Option[Json] =
    parsed(answer).flatMap(field(_, "result"))

  private def code(answer: Bsp.Answer): Option[Int] =
    parsed(answer).flatMap(field(_, "error")).flatMap(number(_, "code"))

  private def message(answer: Bsp.Answer): Option[String] =
    parsed(answer).flatMap(field(_, "error")).flatMap(string(_, "message"))

  private def caps(answer: Bsp.Answer): Option[Json] =
    resultOf(answer).flatMap(field(_, "capabilities"))

  private def status(answer: Bsp.Answer): Option[Int] =
    resultOf(answer).flatMap(number(_, "statusCode"))

  private def origin(answer: Bsp.Answer): Option[String] =
    resultOf(answer).flatMap(string(_, "originId"))

  private def targets(answer: Bsp.Answer): List[Json] =
    resultOf(answer).flatMap(field(_, "targets")) match
      case Some(Json.Arr(values)) => values.toList
      case _                      => Nil

  private def targetUris(answer: Bsp.Answer): List[String] =
    targets(answer).flatMap(item => field(item, "id").flatMap(string(_, "uri")).toList)

  private def displayNames(answer: Bsp.Answer): List[String] =
    targets(answer).flatMap(item => string(item, "displayName").toList)

  private def versions(answer: Bsp.Answer): List[String] =
    targets(answer).flatMap(item => field(item, "data").flatMap(string(_, "scalaVersion")).toList)

  private def binaries(answer: Bsp.Answer): List[String] =
    targets(answer).flatMap(item => field(item, "data").flatMap(string(_, "scalaBinaryVersion")).toList)

  private def platforms(answer: Bsp.Answer): List[Int] =
    targets(answer).flatMap(item => field(item, "data").flatMap(number(_, "platform")).toList)

  private def languages(answer: Bsp.Answer): List[List[String]] =
    targets(answer).map(item => strings(item, "languageIds"))

  private def tags(answer: Bsp.Answer): List[List[String]] =
    targets(answer).map(item => strings(item, "tags"))

  private def jars(answer: Bsp.Answer, version: String): List[String] =
    targets(answer).filter(item => field(item, "data").flatMap(string(_, "scalaVersion")).contains(version)).flatMap: item =>
      field(item, "data").toList.flatMap(data => strings(data, "jars"))

  private def flags(answer: Bsp.Answer, name: String): List[Boolean] =
    targets(answer).flatMap: item =>
      field(item, "capabilities").flatMap(field(_, name)).collect { case Json.Bool(value) => value }.toList

  private def optionItems(answer: Bsp.Answer): List[Json] =
    resultOf(answer).flatMap(field(_, "items")) match
      case Some(Json.Arr(values)) => values.toList
      case _                      => Nil

  private def scalacFlags(answer: Bsp.Answer): List[String] =
    optionItems(answer).flatMap(item => strings(item, "options"))

  private def classpath(answer: Bsp.Answer): List[String] =
    optionItems(answer).flatMap(item => strings(item, "classpath"))

  private def classDirs(answer: Bsp.Answer): List[Path] =
    optionItems(answer).flatMap(item => string(item, "classDirectory").toList).flatMap(uri => pathOf(uri).toList)

  private def sourceItems(answer: Bsp.Answer): List[Json] =
    optionItems(answer).flatMap: item =>
      field(item, "sources") match
        case Some(Json.Arr(values)) => values.toList
        case _                      => Nil

  private def sourcePaths(answer: Bsp.Answer): List[Path] =
    sourceItems(answer).flatMap(item => string(item, "uri").toList).flatMap(uri => pathOf(uri).toList)

  private def sourceKinds(answer: Bsp.Answer): List[Int] =
    sourceItems(answer).flatMap(item => number(item, "kind").toList)

  private def generated(answer: Bsp.Answer): List[Boolean] =
    sourceItems(answer).flatMap(item => field(item, "generated").collect { case Json.Bool(value) => value }.toList)

  private def pathOf(uri: String): Option[Path] =
    try Some(Path.of(java.net.URI.create(uri)).normalize)
    catch case _: IllegalArgumentException => None

  private def number(json: Json, name: String): Option[Int] =
    field(json, name).collect { case Json.Num(value) => value }.flatMap(exactInt)

  private def exactInt(value: java.math.BigDecimal): Option[Int] =
    try Some(value.intValueExact)
    catch case _: ArithmeticException => None

  private def strings(json: Json, name: String): List[String] =
    field(json, name) match
      case Some(Json.Arr(values)) => values.toList.collect { case Json.Str(text) => text }
      case _                      => Nil

  private def string(json: Json, name: String): Option[String] =
    field(json, name).collect { case Json.Str(value) => value }

  private def field(json: Json, name: String): Option[Json] =
    json match
      case Json.Obj(fields) => fields.collectFirst { case (`name`, value) => value }
      case _                => None

  private def entries(classes: Path): List[String] =
    if !Files.isDirectory(classes) then List(s"missing:${classes.toAbsolutePath}")
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
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-bsp"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("bsp")(protocol, client) @@ TestAspect.sequential @@ TestAspect.timeout(4.minutes)
