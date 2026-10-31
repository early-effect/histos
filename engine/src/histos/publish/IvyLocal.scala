package histos.publish

import histos.{PublishFault, StagedDependency, StagedModule}

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import zio.{IO, ZIO}

/** Files `coursier.LocalRepositories.ivy2Local` reads. Resolve has no publish method. */
object IvyLocal:
  def install(module: StagedModule): IO[PublishFault, Unit] =
    placements(module).flatMap(files => ZIO.foreachDiscard(files)(write))

  private val repository = coursier.LocalRepositories.ivy2Local

  private final case class Placement(path: Path, bytes: Array[Byte])

  /** Type, extension, and classifier of one ivy artifact. The file name is separate: metadata is `ivy`, not the module.
    */
  private final case class Artifact(tpe: String, ext: String, classifier: Option[String])

  private val jarArtifact =
    Artifact(coursier.core.Type.jar.value, coursier.core.Extension.jar.value, None)

  private val pomArtifact =
    Artifact(coursier.core.Type.pom.value, coursier.core.Extension.pom.value, None)

  /** `Artifacts.defaultTypes` keeps a sources classifier only as `Type.source` (`src`) with a jar extension. */
  private val sourcesArtifact =
    Artifact(
      coursier.core.Type.source.value,
      coursier.core.Extension.jar.value,
      Some(coursier.core.Classifier.sources.value),
    )

  /** Same filter: javadoc is `Type.doc` (`doc`), jar extension, classifier `javadoc`. */
  private val javadocArtifact =
    Artifact(
      coursier.core.Type.doc.value,
      coursier.core.Extension.jar.value,
      Some(coursier.core.Classifier.javadoc.value),
    )

  /** `IvyRepository.find0` asks for `Type.ivy`, artifact `ivy`, `Extension("xml")`, and no classifier. */
  private val metadataArtifact =
    Artifact(coursier.core.Type.ivy.value, coursier.core.Extension("xml").value, None)

  private val metadataName = "ivy"

  private def placements(module: StagedModule): IO[PublishFault, List[Placement]] =
    for
      jarBytes     <- read(module.jar)
      pomBytes     <- read(module.pom)
      sourceBytes  <- readOption(module.sources)
      javadocBytes <- readOption(module.javadoc)
      jar          <- place(module, module.artifact, jarArtifact, jarBytes)
      pom          <- place(module, module.artifact, pomArtifact, pomBytes)
      sources      <- placeOption(module, sourcesArtifact, sourceBytes)
      javadoc      <- placeOption(module, javadocArtifact, javadocBytes)
      metadata     <- place(module, metadataName, metadataArtifact, descriptor(module).getBytes(StandardCharsets.UTF_8))
    yield jar ++ pom ++ sources ++ javadoc ++ metadata

  private def placeOption(
      module: StagedModule,
      artifact: Artifact,
      bytes: Option[Array[Byte]],
  ): IO[PublishFault, List[Placement]] =
    bytes match
      case None       => ZIO.succeed(Nil)
      case Some(body) => place(module, module.artifact, artifact, body)

  private def place(
      module: StagedModule,
      name: String,
      artifact: Artifact,
      bytes: Array[Byte],
  ): IO[PublishFault, List[Placement]] =
    for
      path <- locate(module, name, artifact)
      sums <- checksums(path, bytes)
    yield Placement(path, bytes) :: sums

  /** `withDefaultChecksums` appends `.sha1` and `.md5`. `fromPattern` turns checksums on. */
  private def checksums(path: Path, bytes: Array[Byte]): IO[PublishFault, List[Placement]] =
    if repository.withChecksums then
      for
        sha1 <- sibling(path, ".sha1")
        md5  <- sibling(path, ".md5")
        sha  <- fingerprint("SHA-1", bytes, path)
        md   <- fingerprint("MD5", bytes, path)
      yield List(Placement(sha1, sha), Placement(md5, md))
    else ZIO.succeed(Nil)

  private def locate(module: StagedModule, name: String, artifact: Artifact): IO[PublishFault, Path] =
    val variables =
      Map(
        "organization" -> module.group.value,
        "organisation" -> module.group.value,
        "orgPath"      -> module.group.value.replace('.', '/'),
        "module"       -> module.artifact,
        "revision"     -> module.revision,
        "type"         -> artifact.tpe,
        "artifact"     -> name,
        "ext"          -> artifact.ext,
      ) ++ artifact.classifier.map("classifier" -> _)
    repository.pattern.substituteVariables(variables) match
      case Left(message) => ZIO.fail(PublishFault.Write(message))
      case Right(url)    => ZIO.attempt(Path.of(URI.create(url))).orElseFail(PublishFault.Write(url))

  private def sibling(path: Path, suffix: String): IO[PublishFault, Path] =
    Option(path.getFileName) match
      case Some(name) => ZIO.succeed(path.resolveSibling(name.toString + suffix))
      case None       => ZIO.fail(PublishFault.Write(path.toString))

  private def fingerprint(algorithm: String, bytes: Array[Byte], path: Path): IO[PublishFault, Array[Byte]] =
    ZIO
      .attempt {
        val digest = MessageDigest.getInstance(algorithm).digest(bytes)
        hex(digest).getBytes(StandardCharsets.US_ASCII)
      }
      .mapError(_ => PublishFault.Write(path.toString))

  private def hex(bytes: Array[Byte]): String =
    bytes.map { byte =>
      val value = byte & 0xff
      f"$value%02x"
    }.mkString

  private def read(path: Path): IO[PublishFault, Array[Byte]] =
    ZIO.attemptBlocking(Files.readAllBytes(path)).mapError(_ => PublishFault.Write(path.toString))

  private def readOption(path: Option[Path]): IO[PublishFault, Option[Array[Byte]]] =
    path match
      case None       => ZIO.succeed(None)
      case Some(file) => read(file).map(Some(_))

  private def write(placement: Placement): IO[PublishFault, Unit] =
    ZIO
      .attemptBlocking {
        Option(placement.path.getParent).foreach(Files.createDirectories(_))
        Files.write(placement.path, placement.bytes)
        ()
      }
      .mapError(_ => PublishFault.Write(placement.path.toString))

  /** `default(runtime)` falls back to `default`, so that conf has to extend the confs the dependencies name. */
  private def descriptor(module: StagedModule): String =
    val publications =
      if module.sources.isEmpty && module.javadoc.isEmpty then ""
      else
        val listed =
          artifactElement(module.artifact, jarArtifact) ::
            extra(module.sources, sourcesArtifact, module.artifact) :::
            extra(module.javadoc, javadocArtifact, module.artifact)
        s"<publications>${listed.mkString}</publications>"
    val dependencies =
      if module.dependencies.isEmpty then ""
      else s"<dependencies>${module.dependencies.map(dependency).mkString}</dependencies>"
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<ivy-module version="2.0">
       |<info organisation="${xml(module.group.value)}" module="${xml(module.artifact)}" revision="${xml(
        module.revision
      )}"/>
       |<configurations>
       |<conf name="compile" visibility="public"/>
       |<conf name="runtime" visibility="public" extends="compile"/>
       |<conf name="default" visibility="public" extends="runtime"/>
       |</configurations>
       |$publications
       |$dependencies
       |</ivy-module>
       |""".stripMargin

  private def extra(path: Option[Path], artifact: Artifact, name: String): List[String] =
    path match
      case None    => Nil
      case Some(_) => List(artifactElement(name, artifact))

  private def artifactElement(name: String, artifact: Artifact): String =
    val classified = artifact.classifier.fold("")(value => s""" classifier="${xml(value)}"""")
    s"""<artifact name="${xml(name)}" type="${xml(artifact.tpe)}" ext="${xml(artifact.ext)}"$classified/>"""

  private def dependency(item: StagedDependency): String =
    s"""<dependency org="${xml(item.group.value)}" name="${xml(item.artifact)}" rev="${xml(
        item.revision
      )}" conf="compile->compile;runtime->runtime"/>"""

  private def xml(raw: String): String =
    raw.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&apos;"
      case char => String.valueOf(char)
    }
end IvyLocal
