package histos.publish

import histos.*
import histos.central.{Artifact, ArtifactId, Published, Publisher, Revision}
import histos.pack.ClassJar
import histos.supply.Email
import histos.suite.HistosSpec
import histos.version.ShipName
import histos.workspace.Workspace

import java.io.StringReader
import java.nio.file.Files
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathFactory
import org.xml.sax.InputSource

import zio.{Chunk, ZIO}
import zio.test.*

object PomMetadataSpec extends HistosSpec:
  private val version = ScalaVersion("3.10.1")
  private val core    = module("heddle", Jvm)(version)
    .description(PomText("HTTP as an effect. Loom under the floor."))
  private val families = List(
    "heddle"               -> "HTTP as an effect. Loom under the floor.",
    "heddle-brotli"        -> "RFC 7932 brotli encoder for heddle",
    "heddle-oauth"         -> "OAuth2 / OIDC client, resource server, and provider for heddle",
    "heddle-mcp-protocol"  -> "MCP wire types and codecs: JSON-RPC, tools, resources, capabilities",
    "heddle-mcp"           -> "MCP 2026-07-28 server for heddle endpoints",
    "heddle-opentelemetry" -> "OpenTelemetry spans for heddle servers and clients",
    "heddle-mcp-apps" -> "MCP Apps (SEP-1865): sheds of typed grants, ui:// resources, and the policy a host clamps",
    "heddle-mcp-apps-host" -> "MCP Apps host kit: effective policy, CSP, hash pins, audit, consent, and the view pipeline",
    "heddle-mcp-apps-frame" -> "MCP Apps host frame: the relay iframe and its handshake, in the host page",
  )
  private val metadata = Projects
    .organization(Organization("rocks.earlyeffect"))
    .license(License(LicenseName("Apache-2.0"), MetadataUri("http://www.apache.org/licenses/LICENSE-2.0.txt")))
    .developer(
      Developer(
        DeveloperId("russwyte"),
        PomText("Russ White"),
        Email("356303+russwyte@users.noreply.github.com"),
        MetadataUri("https://github.com/russwyte"),
      )
    )
    .scm(
      Scm(
        MetadataUri("https://github.com/early-effect/heddle"),
        ScmConnection("scm:git@github.com:early-effect/heddle.git"),
      )
    )
    .url(MetadataUri("https://github.com/early-effect/heddle"))
    .organizationName(PomText("Early Effect"))
    .organizationHomepage(MetadataUri("https://www.earlyeffect.rocks"))
    .versionScheme(VersionScheme.EarlySemver)
    .description(PomText("organization fallback"))

  def spec = suite("POM metadata")(
    test("written POM XML retains all Heddle metadata for all nine published families") {
      ZIO.scoped {
        for
          repo        <- Workspace.work("histos-pom-metadata")
          definitions <- ZIO.foreach(families.drop(1)) { (name, description) =>
            ZIO.fromEither(PomText.from(description)).map(text => module(name, Jvm)(version).description(text))
          }
          ship = Ship("heddle")
            .next(0, 15, 0)(core, definitions*)
            .description(PomText("ship fallback"))
          graph <- ZIO.fromEither(metadata(ships(ship), (core :: definitions)*))
          marker = repo.resolve("marker.txt")
          _     <- Workspace.writeString(marker, "publication fixture")
          jar   <- ClassJar.writeNamed(List("marker.txt" -> marker), repo.resolve("fixture.jar"))
          bytes <- ZIO.attemptBlocking(Chunk.fromArray(Files.readAllBytes(jar.path)))
          revision = Revision.Release(histos.version.ReleaseVersion(histos.version.ShipNumber.trusted(0, 15, 0)))
          assertions <- ZIO.foreach(families) { (family, description) =>
            for
              name    <- ZIO.fromEither(ArtifactId.from(s"${family}_3"))
              project <- ZIO.fromEither(ProjectName.from(family))
              module  <- ZIO.fromOption(graph.get(project).collect { case value: Module => value })
              document = LocalPublish.document(graph, ShipName("heddle"), module)
              published <- ZIO
                .serviceWithZIO[Publisher](
                  _.publish(
                    Artifact(
                      Organization("rocks.earlyeffect"),
                      name,
                      revision,
                      bytes,
                      document = document,
                      sources = Some(bytes),
                    )
                  )
                )
                .provideLayer(Publisher.local(repo.resolve("maven")))
              pom <- published match
                case Published.Local(_, path, _, _, _) => Workspace.readString(path)
                case Published.Snapshot(_, _)          => ZIO.fail(histos.central.PublishError.EmptyJar)
              fields <- ZIO.attemptBlocking {
                val xml = DocumentBuilderFactory
                  .newInstance()
                  .newDocumentBuilder()
                  .parse(new InputSource(new StringReader(pom)))
                val xpath = XPathFactory.newInstance().newXPath()
                List(
                  "groupId",
                  "artifactId",
                  "version",
                  "name",
                  "description",
                  "url",
                  "licenses/license/name",
                  "licenses/license/url",
                  "licenses/license/distribution",
                  "developers/developer/id",
                  "developers/developer/name",
                  "developers/developer/email",
                  "developers/developer/url",
                  "organization/name",
                  "organization/url",
                  "properties/info.versionScheme",
                  "scm/url",
                  "scm/connection",
                  "scm/developerConnection",
                ).map(path => xpath.evaluate(s"/project/$path", xml))
              }
            yield assertTrue(
              fields == List(
                "rocks.earlyeffect",
                s"${family}_3",
                "0.15.0",
                family,
                description,
                "https://github.com/early-effect/heddle",
                "Apache-2.0",
                "http://www.apache.org/licenses/LICENSE-2.0.txt",
                "repo",
                "russwyte",
                "Russ White",
                "356303+russwyte@users.noreply.github.com",
                "https://github.com/russwyte",
                "Early Effect",
                "https://www.earlyeffect.rocks",
                "early-semver",
                "https://github.com/early-effect/heddle",
                "scm:git@github.com:early-effect/heddle.git",
                "scm:git@github.com:early-effect/heddle.git",
              )
            )
          }
        yield assertions.foldLeft(assertTrue(families.size == 9))(_ && _)
      }
    },
    test("description precedence is module, ship, organization, then absent") {
      val described = module("described", Jvm)(version)
        .description(PomText("module & <text>"))
        .libraryDependencies(dep"com.example:library:1.0.0")
        .artifact(ArtifactBase("heddle-core"))
      val plain = module("plain", Jvm)(version)
      val ship  = Ship("heddle").next(0, 15, 0)(described, plain).description(PomText("ship"))
      for
        graph           <- ZIO.fromEither(metadata(ships(ship), described, plain))
        empty           <- ZIO.fromEither(Projects(plain))
        describedModule <- ZIO.fromOption(graph.get(ProjectName("described")).collect { case value: Module => value })
        plainModule     <- ZIO.fromOption(graph.get(ProjectName("plain")).collect { case value: Module => value })
        name            <- ZIO.fromEither(ArtifactId.from("escaped_3"))
        text            <- ZIO.attemptBlocking {
          val document = LocalPublish
            .document(graph, ShipName("heddle"), describedModule)
            .copy(
              organizationName = Some(PomText("Early & <Effect>")),
              organizationHomepage = Some(MetadataUri("https://example.com/?a=1&b=2")),
            )
          val revision = Revision.Release(histos.version.ReleaseVersion(histos.version.ShipNumber.trusted(0, 15, 0)))
          val pom      = histos.central.Layout.pom(
            Artifact(Organization("rocks.earlyeffect"), name, revision, Chunk.empty, document = document)
          )
          val xml =
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(pom)))
          val xpath = XPathFactory.newInstance().newXPath()
          List("description", "organization/name", "organization/url").map(path =>
            xpath.evaluate(s"/project/$path", xml)
          )
        }
      yield assertTrue(
        text == List("module & <text>", "Early & <Effect>", "https://example.com/?a=1&b=2"),
        LocalPublish.document(graph, ShipName("heddle"), describedModule).name.contains(PomText("heddle-core")),
        LocalPublish.document(graph, ShipName("heddle"), plainModule).name.contains(PomText("heddle-plain")),
        LocalPublish
          .document(graph, ShipName("heddle"), describedModule)
          .description
          .contains(PomText("module & <text>")),
        LocalPublish.document(graph, ShipName("heddle"), plainModule).description.contains(PomText("ship")),
        LocalPublish
          .document(graph, ShipName("other"), plainModule)
          .description
          .contains(PomText("organization fallback")),
        LocalPublish.document(empty, ShipName("other"), plainModule).description.isEmpty,
      )
    },
  )
end PomMetadataSpec
