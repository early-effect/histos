package histos.version

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import histos.central.{ArtifactId, MavenModule}
import histos.*
import histos.workspace.{IoFailure, Workspace}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import _root_.scala.concurrent.duration.DurationInt

import zio.{IO, Scope, ZIO}
import zio.test.*

object CoursierStablesLawSpec extends ZIOSpecDefault:
  def spec = suite("coursier stables")(
    test("a warm cache refreshes metadata and selects the newly published stable"):
      ZIO.scoped:
        for
          rig    <- hosted
          seeded <- rig.list(publishedModule)
          before <- ZIO.succeed(rig.metadataRequests.get())
          _      <- rig.serve("0.3.1")
          fresh  <- rig.list(publishedModule)
          after  <- ZIO.succeed(rig.metadataRequests.get())
        yield assertTrue(
          ShipBump.choose(seeded).contains(stable(0)),
          ShipBump.choose(fresh).contains(stable(1)),
          before > 0,
          after > before,
        )
    ,
    test("a failed refresh is unanswered, and fresh empty metadata is unpublished"):
      ZIO.scoped:
        for
          rig           <- hosted
          seeded        <- rig.list(publishedModule)
          before        <- ZIO.succeed(rig.metadataRequests.get())
          _             <- rig.refuse
          failed        <- rig.list(publishedModule).either
          after         <- ZIO.succeed(rig.metadataRequests.get())
          _             <- rig.serve()
          empty         <- rig.list(publishedModule)
          finalRequests <- ZIO.succeed(rig.metadataRequests.get())
        yield assertTrue(
          ShipBump.choose(seeded).contains(stable(0)),
          failed == Left(BumpFault.Unanswered("rocks.earlyeffect:histos-api_3")),
          after > before,
          empty == Nil,
          ShipBump.move(ShipNumber.trusted(0, 3, 1), ShipBump.choose(empty), BumpPart.Patch) == ShipMove.Unpublished,
          finalRequests > after,
        )
    ,
    test("bump refreshes a warm cache and rewrites a caught catalog, but keeps an ahead catalog"):
      ZIO.scoped:
        val api   = module("api", Jvm)(scala"3.10.1").artifact(artifact"histos-api")
        val build = Projects.organization(organization"rocks.earlyeffect")(
          ships(Ship("histos").next(0, 3, 1)(api)),
          api,
        )
        val source = """ships(Ship("histos").next(0, 3, 1)(api))
                       |""".stripMargin
        for
          graph  <- ZIO.fromEither(build)
          rig    <- hosted
          seeded <- rig.list(publishedModule)
          _      <- rig.serve("0.3.1")
          moved  <- ShipBumpRun.run(graph, BumpPart.Patch, Nil, source, rig.list)
          _      <- rig.serve("0.3.0")
          ahead  <- ShipBumpRun.run(graph, BumpPart.Patch, Nil, source, rig.list)
        yield assertTrue(
          ShipBump.choose(seeded).contains(stable(0)),
          moved == ShipBumpRun.Report(
            "histos 0.3.1 from maven -> 0.3.2\n",
            Some("""ships(Ship("histos").next(0, 3, 2)(api))
                   |""".stripMargin),
          ),
          ahead == ShipBumpRun.Report("histos 0.3.1 is ahead of 0.3.0 from maven\n", None),
        )
    ,
    test("a maven stable wins, and snapshots and prereleases are not ship numbers"):
      val result = coursier.Versions.Result(
        Seq(
          coursier.LocalRepositories.ivy2Local -> Right(versions("0.2.9", "0.2.9-SNAPSHOT")),
          coursier.Repositories.central        -> Right(versions("0.2.8", "0.2.9-SNAPSHOT", "0.3.0-RC1")),
        )
      )
      val listed = CoursierStables.fromResult("rocks.earlyeffect:histos-api_3", result)
      assertTrue(
        listed.map(ShipBump.choose) == Right(Some(ListedStable(ShipNumber.trusted(0, 2, 8), StablePlace.Remote)))
      )
    ,
    test("ivy2 local answers when maven did not"):
      val result = coursier.Versions.Result(
        Seq(
          coursier.LocalRepositories.ivy2Local -> Right(versions("0.2.9")),
          coursier.Repositories.central        -> Left("not found"),
        )
      )
      val listed = CoursierStables.fromResult("rocks.earlyeffect:histos-api_3", result)
      assertTrue(
        listed.map(ShipBump.choose) == Right(Some(ListedStable(ShipNumber.trusted(0, 2, 9), StablePlace.Local)))
      )
    ,
    test("no repository answering is the fault"):
      val result = coursier.Versions.Result(
        Seq(coursier.Repositories.central -> Left("not found"))
      )
      assertTrue(
        CoursierStables.fromResult("rocks.earlyeffect:histos-api_3", result) ==
          Left(BumpFault.Unanswered("rocks.earlyeffect:histos-api_3"))
      )
    ,
    test("an empty resolver list still asks ivy2 local and central"):
      val repos = CoursierStables.repositories(Nil)
      assertTrue(
        repos.headOption.contains(coursier.LocalRepositories.ivy2Local),
        repos.contains(coursier.Repositories.central),
      ),
  )

  private val publishedModule = MavenModule(Organization("rocks.earlyeffect"), ArtifactId.trusted("histos-api_3"))
  private val metadataPath    = "/rocks/earlyeffect/histos-api_3/maven-metadata.xml"

  private def stable(patch: Int): ListedStable =
    ListedStable(ShipNumber.trusted(0, 3, patch), StablePlace.Remote)

  private enum HostFault:
    case Listen

  private final case class Reply(status: Int, body: String)

  private final case class Rig(
      cache: coursier.cache.FileCache[coursier.util.Task],
      repository: coursier.core.Repository,
      reply: AtomicReference[Reply],
      metadataRequests: AtomicInteger,
  ):
    def list(module: MavenModule): IO[BumpFault, List[ListedStable]] =
      CoursierStables.list(module, cache, Seq(repository))

    def serve(raws: String*): IO[Nothing, Unit] =
      ZIO.succeed(reply.set(Reply(200, metadata(raws.toList))))

    def refuse: IO[Nothing, Unit] =
      ZIO.succeed(reply.set(Reply(403, "metadata refused")))
  end Rig

  private def hosted: ZIO[Scope, IoFailure | HostFault, Rig] =
    for
      root     <- Workspace.work("histos-stables")
      reply    <- ZIO.succeed(new AtomicReference(Reply(200, metadata(List("0.3.0")))))
      requests <- ZIO.succeed(new AtomicInteger())
      server   <- ZIO.acquireRelease(
        ZIO
          .attempt {
            val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
            server.createContext("/", exchange => respond(exchange, reply.get(), requests))
            server.start()
            server
          }
          .mapError(_ => HostFault.Listen)
      )(server => ZIO.succeed(server.stop(0)))
      cache <- ZIO.succeed(
        coursier.cache.FileCache[coursier.util.Task](
          location = root.toFile,
          credentials = Nil,
          cachePolicies = coursier.cache.CacheDefaults.noEnvCachePolicies,
          ttl = Some(24.hours),
        )
      )
    yield Rig(cache, coursier.MavenRepository(s"http://127.0.0.1:${server.getAddress.getPort}"), reply, requests)

  private def metadata(raws: List[String]): String =
    val versions = raws.map(raw => s"<version>$raw</version>").mkString
    s"<metadata><groupId>rocks.earlyeffect</groupId><artifactId>histos-api_3</artifactId><versioning><versions>$versions</versions></versioning></metadata>"

  private def respond(exchange: HttpExchange, current: Reply, requests: AtomicInteger): Unit =
    val path  = exchange.getRequestURI.getPath
    val reply =
      if exchange.getRequestMethod != "GET" then Reply(405, "method")
      else if path == metadataPath then
        requests.incrementAndGet()
        current
      else if path == s"$metadataPath.sha1" then checksum(current, "SHA-1")
      else if path == s"$metadataPath.md5" then checksum(current, "MD5")
      else if path == "/rocks/earlyeffect/histos-api_3/" then Reply(403, "no directory listing")
      else Reply(403, "unhandled request")
    try
      val bytes = reply.body.getBytes(UTF_8)
      exchange.sendResponseHeaders(reply.status, bytes.length.toLong)
      exchange.getResponseBody.write(bytes)
    finally exchange.close()

  private def checksum(reply: Reply, algorithm: String): Reply =
    if reply.status != 200 then reply
    else Reply(200, HexFormat.of().formatHex(MessageDigest.getInstance(algorithm).digest(reply.body.getBytes(UTF_8))))

  private def versions(raws: String*): coursier.core.Versions =
    val parsed = raws.toList.map(coursier.version.Version(_))
    parsed match
      case head :: _ =>
        coursier.core.Versions(head, head, parsed, None)
      case Nil =>
        coursier.core.Versions(coursier.version.Version.zero, coursier.version.Version.zero, Nil, None)
end CoursierStablesLawSpec
