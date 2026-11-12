package histos.publish

import histos.version.ShipName
import histos.workspace.Workspace
import histos.{ArtifactBase, DepExclusion, Organization, ProjectName, PublishFault, StagedDependency, StagedModule}

import java.nio.file.{Files, Path}
import java.security.MessageDigest

import zio.{Chunk, ZIO}
import zio.test.*

/** An install writes the `ivy2Local` layout under its root: `[organisation]/[module]/[revision]/[type]s/[artifact]`. */
object IvyLocalLawSpec extends ZIOSpecDefault:
  private val group    = Organization("histos.law")
  private val revision = "0.0.0-law"

  def spec = suite("ivy local laws")(
    test("a missing file is that path, and an install is the jar, the checksums, and the escaped ivy descriptor"):
      ZIO.scoped:
        Workspace.work("histos-ivy-local").flatMap { dir =>
          val root    = IvyRoot.at(dir.resolve("ivy"))
          val home    = installed(dir)
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
          for
            missingJar <- IvyLocal.install(root, staged.copy(jar = dir.resolve("missing.jar"))).either
            _          <- ZIO.attemptBlocking(Files.write(jar, bytes))
            missingPom <- IvyLocal.install(root, staged.copy(pom = dir.resolve("missing.pom"))).either
            _          <- ZIO.attemptBlocking(Files.write(pom, bytes))
            missingSrc <- IvyLocal.install(root, staged).either
            _          <- ZIO.attemptBlocking:
              Files.write(sources, bytes)
              Files.write(javadoc, bytes)
            _       <- IvyLocal.install(root, staged)
            written <- ZIO.attemptBlocking(read(home))
            extras  <- ZIO.attemptBlocking(
              List("srcs/widget-sources.jar", "docs/widget-javadoc.jar", "poms/widget.pom")
                .filterNot(name => Files.isRegularFile(home.resolve(name)))
            )
            _      <- IvyLocal.install(root, staged.copy(dependencies = Chunk(excluded)))
            marked <- ZIO.attemptBlocking(read(home).text)
            _      <- IvyLocal.install(root, staged.copy(sources = None, javadoc = None, dependencies = Chunk.empty))
            bare   <- ZIO.attemptBlocking(read(home).text)
          yield assertTrue(
            missingJar == Left(PublishFault.Write(dir.resolve("missing.jar").toString)),
            missingPom == Left(PublishFault.Write(dir.resolve("missing.pom").toString)),
            missingSrc == Left(PublishFault.Write(sources.toString)),
            written.jarBytes.contains(bytes.toList),
            written.sha.contains(hex("SHA-1", bytes)),
            written.md5.contains(hex("MD5", bytes)),
            extras == Nil,
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
        }
    ,
    test("a second install of a coordinate replaces the jar, the pom, and the jar checksum"):
      ZIO.scoped:
        Workspace.work("histos-ivy-again").flatMap { dir =>
          val root   = IvyRoot.at(dir.resolve("ivy"))
          val home   = installed(dir)
          val jar    = dir.resolve("widget.jar")
          val pom    = dir.resolve("widget.pom")
          val staged = module(jar, pom, None, None, Chunk.empty)
          val first  = Array[Byte](1, 2, 3, 4)
          val second = Array[Byte](9, 8, 7, 6, 5)
          for
            _      <- ZIO.attemptBlocking(Files.write(jar, first))
            _      <- ZIO.attemptBlocking(Files.writeString(pom, "<project>one</project>"))
            _      <- IvyLocal.install(root, staged)
            once   <- ZIO.attemptBlocking(read(home))
            _      <- ZIO.attemptBlocking(Files.write(jar, second))
            _      <- ZIO.attemptBlocking(Files.writeString(pom, "<project>two</project>"))
            _      <- IvyLocal.install(root, staged)
            twice  <- ZIO.attemptBlocking(read(home))
            pomNow <- ZIO.attemptBlocking(Files.readString(home.resolve("poms").resolve("widget.pom")))
          yield assertTrue(
            once.jarBytes.contains(first.toList),
            once.sha.contains(hex("SHA-1", first)),
            twice.jarBytes.contains(second.toList),
            twice.sha.contains(hex("SHA-1", second)),
            pomNow == "<project>two</project>",
          )
        },
  )

  private val excluded =
    StagedDependency(
      Organization("org.example"),
      "lib",
      "1.0.0",
      List(DepExclusion(Organization.trusted("org.codehaus.plexus"), ArtifactBase.trusted("plexus-archiver"))),
    )

  private final case class Written(jarBytes: Option[List[Byte]], sha: Option[String], md5: Option[String], text: String)

  /** Where `widget` at [[revision]] lands under the root `dir/ivy`. */
  private def installed(dir: Path): Path =
    dir.resolve("ivy").resolve(group.value).resolve("widget").resolve(revision)

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

  private def read(home: Path): Written =
    val jars = home.resolve("jars")
    Written(
      file(jars.resolve("widget.jar")).map(path => Files.readAllBytes(path).toList),
      file(jars.resolve("widget.jar.sha1")).map(path => Files.readString(path).trim),
      file(jars.resolve("widget.jar.md5")).map(path => Files.readString(path).trim),
      file(home.resolve("ivys").resolve("ivy.xml")).map(path => Files.readString(path)).getOrElse(""),
    )

  private def file(path: Path): Option[Path] =
    Option.when(Files.isRegularFile(path))(path)

  private def hex(algorithm: String, bytes: Array[Byte]): String =
    MessageDigest
      .getInstance(algorithm)
      .digest(bytes)
      .map { byte =>
        val value = byte & 0xff
        f"$value%02x"
      }
      .mkString
end IvyLocalLawSpec
