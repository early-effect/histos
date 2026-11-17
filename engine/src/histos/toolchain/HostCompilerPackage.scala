package histos.toolchain

import histos.jdk.{JdkVersion, Sha256Digest}
import histos.pack.AssemblyFile
import histos.plugin.{ArtifactPath, ContentHash}
import histos.report.WireJson.given
import histos.resolve.ResolvedArtifact
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{Dep, DepKind, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import zio.json.*
import zio.{Chunk, Scope, ZIO}

/** Original compiler jars for a launcher assembly. The snapshot records digests only, and the launcher cell is not a
  * plugin library, so the caller passes the `CompilerRequests.compiler` classpath from that lock generation.
  */
private[histos] object HostCompilerPackage:
  def files(
      version: ScalaVersion,
      jdk: JdkVersion,
      artifacts: List[ResolvedArtifact],
  ): ZIO[Scope, HostCompilerError, List[AssemblyFile]] =
    for
      _      <- embedded(version)
      packed <- ZIO.foreach(artifacts.zipWithIndex) { (artifact, index) => one(artifact, index) }
      entries = packed.map(_.entry)
      _ <- unique(entries)
      _ <- required(entries)
      _ <- compilerRevision(version, entries)
      manifest = HostCompilerInputs.sign(HostCompilerManifest.Current, version, entries, jdk)
      written <- writeManifest(manifest)
    yield written :: packed.map(item => AssemblyFile(s"bootstrap/compiler/${item.entry.relative.value}", item.source))

  private final case class Packed(entry: HostToolEntry, source: java.nio.file.Path)

  private def one(artifact: ResolvedArtifact, index: Int): ZIO[Any, HostCompilerError, Packed] =
    val role = classify(artifact)
    for
      relative <- relative(index, artifact)
      actual   <- digestOf(artifact.path)
      expected = ContentHash.trusted(artifact.sha256.value)
      _ <-
        if actual == expected then ZIO.unit
        else ZIO.fail(HostCompilerError.BadDigest(role, expected, actual))
    yield Packed(
      HostToolEntry(
        role,
        Dep.trusted(artifact.organization, artifact.name, artifact.revision, DepKind.Java),
        actual,
        relative,
      ),
      artifact.path,
    )

  /** Name and organization. A content marker would treat `scala3-library_3` as a second runtime. */
  private def classify(artifact: ResolvedArtifact): HostToolRole =
    artifact.name match
      case "scala3-compiler_3" | "scala-compiler"                       => HostToolRole.Compiler
      case "scala3-sbt-bridge" | "scala2-sbt-bridge"                    => HostToolRole.Bridge
      case "compiler-interface"                                         => HostToolRole.Interfaces
      case "scala-library" if artifact.organization == "org.scala-lang" => HostToolRole.Runtime
      case _                                                            => HostToolRole.Support

  private def relative(index: Int, artifact: ResolvedArtifact): ZIO[Any, HostCompilerError, ArtifactPath] =
    val classifier = artifact.classifier.fold("")(value => s"-${value.value}")
    val raw        = s"$index-${artifact.name}$classifier-${artifact.revision}.jar"
    ZIO
      .fromEither(ArtifactPath.from(raw))
      .mapError(HostCompilerError.BadEntry(_))

  private def digestOf(path: java.nio.file.Path): ZIO[Any, HostCompilerError, ContentHash] =
    ZIO
      .attemptBlocking(Files.readAllBytes(path))
      .mapBoth(
        _ => HostCompilerError.Io(IoFailure(path, IoFault.Read)),
        bytes => ContentHash.trusted(Sha256Digest.of(Chunk.fromArray(bytes)).value),
      )

  private def embedded(version: ScalaVersion): ZIO[Any, HostCompilerError, Unit] =
    ScalaVersion.from(scala.util.Properties.versionNumberString) match
      case Left(error)                      => ZIO.fail(HostCompilerError.BadRevision(error))
      case Right(found) if found == version => ZIO.unit
      case Right(found)                     => ZIO.fail(HostCompilerError.IncompatibleCompiler(found, version))

  private val Unique: List[HostToolRole] = List(
    HostToolRole.Compiler,
    HostToolRole.Bridge,
    HostToolRole.Interfaces,
    HostToolRole.Runtime,
  )

  private def unique(tools: List[HostToolEntry]): ZIO[Any, HostCompilerError, Unit] =
    Unique.find(role => tools.count(_.role == role) > 1) match
      case Some(role) => ZIO.fail(HostCompilerError.ConflictingRole(role))
      case None       => ZIO.unit

  private def required(tools: List[HostToolEntry]): ZIO[Any, HostCompilerError, Unit] =
    Unique.find(role => !tools.exists(_.role == role)) match
      case Some(role) => ZIO.fail(HostCompilerError.EntryAbsent(role))
      case None       => ZIO.unit

  private def compilerRevision(version: ScalaVersion, tools: List[HostToolEntry]): ZIO[Any, HostCompilerError, Unit] =
    tools.find(_.role == HostToolRole.Compiler) match
      case None => ZIO.fail(HostCompilerError.EntryAbsent(HostToolRole.Compiler))
      case Some(entry) if entry.coordinate.revision.value == version.value => ZIO.unit
      case Some(entry)                                                     =>
        ScalaVersion.from(entry.coordinate.revision.value) match
          case Right(found) => ZIO.fail(HostCompilerError.IncompatibleCompiler(found, version))
          case Left(error)  => ZIO.fail(HostCompilerError.BadRevision(error))

  private def writeManifest(manifest: HostCompilerManifest): ZIO[Scope, HostCompilerError, AssemblyFile] =
    for
      dir <- Workspace.work("histos-host-compiler").mapError(HostCompilerError.Io(_))
      path = dir.resolve("manifest.json")
      _ <- ZIO
        .attemptBlocking(Files.writeString(path, manifest.toJson, StandardCharsets.UTF_8))
        .mapError(_ => HostCompilerError.Io(IoFailure(path, IoFault.Write)))
    yield AssemblyFile(HostCompilerInputs.ManifestPath, path)
end HostCompilerPackage
