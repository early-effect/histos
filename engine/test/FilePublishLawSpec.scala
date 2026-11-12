package histos.publish

import histos.version.{CiVersion, Revision, ShipName}
import histos.workspace.Workspace
import histos.{ProjectName, PublishReceipt, PublishRequest, StagedModule, organization, ship}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, ZIO, ZLayer}
import zio.test.*

/** `publishLocal` installs through `LocalStock` into its [[IvyRoot]], in the layout Coursier's `ivy2Local` reads. */
object FilePublishLawSpec extends ZIOSpecDefault:
  private val group    = organization"histos.filelaw"
  private val artifact = "filepub"
  private val number   = ship"1.2.3"
  private val revision = Revision.Ci(CiVersion(number))
  private val rendered = revision.render

  private val jarBytes = Chunk.fromArray(Array[Byte](1, 2, 3, 4))
  private val pomBytes = Chunk.fromArray("<project>one</project>".getBytes(StandardCharsets.UTF_8))

  def spec = suite("file publish laws")(
    test("a -ci install names the coordinate, says it installs, and writes the staged jar and pom under the root"):
      ZIO.scoped:
        Workspace
          .work("histos-file-publish")
          .flatMap: dir =>
            val root = dir.resolve("ivy")
            val jar  = dir.resolve("filepub.jar")
            val pom  = dir.resolve("filepub.pom")
            for
              notes  <- Ref.make(List.empty[String])
              _      <- write(jar, jarBytes)
              _      <- write(pom, pomBytes)
              signed <- ZIO
                .serviceWithZIO[histos.LocalPublish](_.publish(request(dir, jar, pom)))
                .provide(ZLayer.succeed(IvyRoot.at(root)) >>> LocalStock.layer(line => notes.update(_ :+ line)))
              placedJar <- read(placed(root, "jars", "jar"))
              placedPom <- read(placed(root, "poms", "pom"))
              said      <- notes.get
            yield assertTrue(
              signed == PublishReceipt(Chunk(s"${group.value} $artifact $rendered"), Chunk.empty),
              said == List(s"installing $artifact $rendered"),
              placedJar == Some(jarBytes),
              placedPom == Some(pomBytes),
            )
  )

  private def request(dir: Path, jar: Path, pom: Path): PublishRequest =
    PublishRequest(
      revision,
      false,
      dir,
      dir,
      Chunk(
        StagedModule(
          ProjectName("filepub"),
          ShipName("filepub"),
          group,
          artifact,
          rendered,
          jar,
          pom,
          None,
          None,
          Chunk.empty,
        )
      ),
    )

  /** `Pattern.default`: `[organisation]/[module]/[revision]/[type]s/[artifact].[ext]`. */
  private def placed(root: Path, kind: String, ext: String): Path =
    root.resolve(group.value).resolve(artifact).resolve(rendered).resolve(kind).resolve(s"$artifact.$ext")

  private def write(path: Path, body: Chunk[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(Files.write(path, body.toArray)).orDie.unit

  private def read(path: Path): ZIO[Any, Nothing, Option[Chunk[Byte]]] =
    ZIO
      .attemptBlocking:
        if Files.isRegularFile(path) then Some(Chunk.fromArray(Files.readAllBytes(path))) else None
      .orDie
end FilePublishLawSpec
