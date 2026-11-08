package histos.publish

import histos.version.{CiVersion, Revision, ShipName}
import histos.workspace.Workspace
import histos.{ProjectName, PublishReceipt, PublishRequest, StagedModule, organization, ship}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import zio.{Chunk, Ref, ZIO}
import zio.test.*

/** `publishLocal` installs through `LocalStock`. The files Coursier reads are `ivy2Local`, not `~/.m2`. */
object FilePublishLawSpec extends ZIOSpecDefault:
  private val group    = organization"histos.filelaw"
  private val artifact = "filepub"
  private val number   = ship"1.2.3"
  private val revision = Revision.Ci(CiVersion(number))
  private val rendered = revision.render

  private val firstJar  = Chunk.fromArray(Array[Byte](1, 2, 3, 4))
  private val secondJar = Chunk.fromArray(Array[Byte](9, 8, 7, 6, 5))
  private val firstPom  = bytes("<project>one</project>")
  private val secondPom = bytes("<project>two</project>")

  def spec = suite("file publish laws")(
    test(
      "a -ci install writes the staged jar and pom into ivy2 local, and a second install of that coordinate overwrites them"
    ):
      ZIO.scoped:
        Workspace
          .work("histos-file-publish")
          .mapError(fault => histos.PublishFault.Write(fault.path.toString))
          .flatMap: dir =>
            localRoot match
              case None =>
                ZIO.succeed(assertTrue(Option(System.getProperty("user.home")).isDefined))
              case Some(root) =>
                val org = root.resolve(group.value)
                installed(dir, root).ensuring(
                  Workspace
                    .delete(org)
                    .mapError(fault => histos.PublishFault.Write(fault.path.toString))
                    .orElseSucceed(())
                )
  )

  private def installed(dir: Path, root: Path): ZIO[Any, histos.PublishFault, TestResult] =
    val jar     = dir.resolve("filepub.jar")
    val pom     = dir.resolve("filepub.pom")
    val jarPath = placed(root, "jars", "jar")
    val pomPath = placed(root, "poms", "pom")
    val shaPath = jarPath.resolveSibling(s"$artifact.jar.sha1")
    for
      notes <- Ref.make(List.empty[String])
      _ <- Workspace.delete(root.resolve(group.value)).mapError(fault => histos.PublishFault.Write(fault.path.toString))
      _ <- write(jar, firstJar)
      _ <- write(pom, firstPom)
      first <- publish(dir, jar, pom, notes)
      jar1  <- read(jarPath)
      pom1  <- read(pomPath)
      sha1  <- readText(shaPath)
      _     <- write(jar, secondJar)
      _     <- write(pom, secondPom)
      again <- publish(dir, jar, pom, notes)
      jar2  <- read(jarPath)
      pom2  <- read(pomPath)
      sha2  <- readText(shaPath)
      said  <- notes.get
    yield assertTrue(
      first == receipt,
      again == receipt,
      jar1 == Some(firstJar),
      pom1 == Some(firstPom),
      sha1 == Some(fingerprint(firstJar)),
      jar2 == Some(secondJar),
      pom2 == Some(secondPom),
      sha2 == Some(fingerprint(secondJar)),
      sha1 != sha2,
      jarPath != jar,
      pomPath != pom,
      said == List(s"installing $artifact $rendered", s"installing $artifact $rendered"),
    )

  private def receipt: PublishReceipt =
    PublishReceipt(Chunk(s"${group.value} $artifact $rendered"), Chunk.empty)

  private def publish(
      dir: Path,
      jar: Path,
      pom: Path,
      notes: Ref[List[String]],
  ): ZIO[Any, histos.PublishFault, PublishReceipt] =
    ZIO
      .serviceWithZIO[histos.LocalPublish](_.publish(request(dir, jar, pom)))
      .provide(LocalStock.layer(line => notes.update(_ :+ line)))

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

  /** Same home `coursier.LocalRepositories.ivy2Local` uses, then `local`. */
  private def localRoot: Option[Path] =
    val configured =
      Option(System.getProperty("coursier.ivy.home"))
        .filter(_.nonEmpty)
        .orElse(
          Option(System.getProperty("ivy.home")).filter(_.nonEmpty)
        )
    configured
      .orElse(Option(System.getProperty("user.home")).filter(_.nonEmpty).map(home => s"$home/.ivy2"))
      .map(base => Path.of(base).resolve("local"))

  private def write(path: Path, body: Chunk[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(Files.write(path, body.toArray)).orDie.unit

  private def read(path: Path): ZIO[Any, Nothing, Option[Chunk[Byte]]] =
    ZIO
      .attemptBlocking:
        if Files.isRegularFile(path) then Some(Chunk.fromArray(Files.readAllBytes(path))) else None
      .orDie

  private def readText(path: Path): ZIO[Any, Nothing, Option[String]] =
    ZIO
      .attemptBlocking:
        if Files.isRegularFile(path) then Some(Files.readString(path, StandardCharsets.US_ASCII)) else None
      .orDie

  private def fingerprint(body: Chunk[Byte]): String =
    MessageDigest
      .getInstance("SHA-1")
      .digest(body.toArray)
      .map: byte =>
        val value = byte & 0xff
        f"$value%02x"
      .mkString

  private def bytes(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))
end FilePublishLawSpec
