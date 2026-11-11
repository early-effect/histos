package histos.publish

import histos.version.ShipName
import histos.workspace.Workspace
import histos.{ArtifactBase, DepExclusion, Organization, ProjectName, PublishFault, StagedDependency, StagedModule}

import java.nio.file.{Files, Path}
import java.security.MessageDigest

import _root_.scala.jdk.CollectionConverters.*

import zio.{Chunk, ZIO}
import zio.test.*

object IvyLocalLawSpec extends ZIOSpecDefault:
  private val group    = Organization("histos.law")
  private val revision = "0.0.0-law"

  def spec = suite("ivy local laws")(
    test("a missing file is that path, and an install is the jar, the checksums, and the escaped ivy descriptor"):
      Option(System.getProperty("user.home")) match
        case None       => ZIO.succeed(assertTrue(Option(System.getProperty("user.home")).isDefined))
        case Some(home) =>
          val root = Path.of(home, ".ivy2", "local", "histos.law")
          Workspace.work("histos-ivy-local").flatMap { dir =>
            val jar     = dir.resolve("widget.jar")
            val pom     = dir.resolve("widget.pom")
            val sources = dir.resolve("widget-sources.jar")
            val javadoc = dir.resolve("widget-javadoc.jar")
            val bytes   = Array[Byte](1, 2, 3)
            val staged  = module(
              jar,
              pom,
              Some(sources),
              Some(javadoc),
              Chunk(StagedDependency(Organization("org.example"), "a&b", "1.0.0")),
            )
            val program =
              for
                _          <- remove(root)
                missingJar <- IvyLocal.install(staged.copy(jar = dir.resolve("missing.jar"))).either
                _          <- ZIO.attemptBlocking(Files.write(jar, bytes))
                missingPom <- IvyLocal.install(staged.copy(pom = dir.resolve("missing.pom"))).either
                _          <- ZIO.attemptBlocking(Files.write(pom, bytes))
                missingSrc <- IvyLocal.install(staged).either
                _          <- ZIO.attemptBlocking:
                  Files.write(sources, bytes)
                  Files.write(javadoc, bytes)
                _       <- IvyLocal.install(staged)
                written <- ZIO.attemptBlocking(read(root))
                _       <- IvyLocal.install(staged.copy(dependencies = Chunk(excluded)))
                marked  <- ZIO.attemptBlocking(find(root, "ivy.xml").map(path => Files.readString(path)).getOrElse(""))
                _       <- IvyLocal.install(staged.copy(sources = None, javadoc = None, dependencies = Chunk.empty))
                bare    <- ZIO.attemptBlocking(find(root, "ivy.xml").map(path => Files.readString(path)).getOrElse(""))
              yield assertTrue(
                missingJar == Left(PublishFault.Write(dir.resolve("missing.jar").toString)),
                missingPom == Left(PublishFault.Write(dir.resolve("missing.pom").toString)),
                missingSrc == Left(PublishFault.Write(sources.toString)),
                written.jarBytes.contains(bytes.toList),
                written.sha.contains(hex("SHA-1", bytes)),
                written.md5.contains(hex("MD5", bytes)),
                written.text.contains("""organisation="histos.law""""),
                written.text.contains("""module="widget""""),
                written.text.contains(s"""revision="$revision""""),
                written.text.contains("""classifier="sources""""),
                written.text.contains("""classifier="javadoc""""),
                written.text.contains("a&amp;b"),
                written.text.contains("""rev="1.0.0" conf="compile->compile;runtime->runtime"/>"""),
                !written.text.contains("<exclude"),
                marked.contains(
                  """<exclude org="org.codehaus.plexus" name="plexus-archiver" matcher="exact"/>"""
                ),
                !marked.contains("plexus-archiver_3"),
                written.text.contains("""conf name="default" visibility="public" extends="runtime""""),
                !bare.contains("classifier="),
                !bare.contains("<dependencies>"),
              )
            program.ensuring(remove(root))
          }
  )

  private val excluded =
    StagedDependency(
      Organization("org.example"),
      "lib",
      "1.0.0",
      List(DepExclusion(Organization.trusted("org.codehaus.plexus"), ArtifactBase.trusted("plexus-archiver"))),
    )

  private final case class Written(jarBytes: Option[List[Byte]], sha: Option[String], md5: Option[String], text: String)

  private def module(
      jar: Path,
      pom: Path,
      sources: Option[Path],
      javadoc: Option[Path],
      dependencies: Chunk[StagedDependency],
  ): StagedModule =
    StagedModule(
      ProjectName("widget"),
      ShipName("widget"),
      group,
      "widget",
      revision,
      jar,
      pom,
      sources,
      javadoc,
      dependencies,
    )

  private def read(root: Path): Written =
    val ivy     = find(root, "ivy.xml")
    val written = find(root, "widget.jar")
    val parent  = written.flatMap(path => Option(path.getParent))
    Written(
      written.map(path => Files.readAllBytes(path).toList),
      parent.flatMap(dir => find(dir, "widget.jar.sha1")).map(path => Files.readString(path).trim),
      parent.flatMap(dir => find(dir, "widget.jar.md5")).map(path => Files.readString(path).trim),
      ivy.map(path => Files.readString(path)).getOrElse(""),
    )

  private def find(root: Path, name: String): Option[Path] =
    if !Files.exists(root) then None
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.find(path => Option(path.getFileName).exists(_.toString == name))
      finally walk.close()

  private def hex(algorithm: String, bytes: Array[Byte]): String =
    MessageDigest
      .getInstance(algorithm)
      .digest(bytes)
      .map { byte =>
        val value = byte & 0xff
        f"$value%02x"
      }
      .mkString

  private def remove(root: Path) =
    ZIO
      .attemptBlocking:
        if Files.exists(root) then
          val walk  = Files.walk(root)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      .orDie
end IvyLocalLawSpec
