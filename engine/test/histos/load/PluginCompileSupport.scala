package histos.load

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkVersion, Sha256Digest}
import histos.plugin.{
  ArtifactPath,
  BindingClass,
  BindingKind,
  ContentHash,
  Digests,
  EnvName,
  FrozenEnv,
  FrozenFile,
  InputKey,
  RepoPath,
  StageAssurance,
  StageBinding,
  StageContract,
  StageInputs,
  CompilerConstraint,
  StageRestrictions,
}
import histos.report.WireJson.given
import histos.schedule.RemoteCache
import histos.toolchain.{HostCompilerError, HostCompilerInputs, HostCompilerSession, HostToolEntry, HostToolRole}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompilerStarts, Compilers}
import histos.{Dep, DepKind, ProjectName, ScalaVersion}

import java.net.URI
import java.net.URLClassLoader
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.annotation.tailrec

import zio.json.*
import zio.{Chunk, Ref, Scope, ZIO, ZLayer}

object PluginCompileSupport:
  type Env = ContentStore & CompilerStarts & Compilers & RemoteCache & PluginFlights

  val Floor: JdkVersion = JdkVersion.trusted("25", JdkMajor.trusted(25))

  def run[A](
      body: (Path, Ref[Int]) => ZIO[Env & Scope, PluginCompileError, A]
  ): ZIO[Any, PluginCompileError, Ran[A]] =
    ZIO.scoped:
      for
        root   <- ZIO.attemptBlocking(Files.createTempDirectory("histos-p5")).orDie
        _      <- ZIO.addFinalizer(ZIO.attemptBlocking(Workspace.deleteSync(root)).orDie)
        cas    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
        starts <- Ref.make(0)
        value  <- body(root, starts).provideSomeLayer[Scope](
          ZLayer.succeed(ContentStore.in(cas)) ++
            ZLayer.succeed(CompilerStarts(starts)) ++
            Compilers.live ++
            RemoteCache.off ++
            PluginFlights.live
        )
        count <- starts.get
      yield Ran(count, value)

  def session(payload: Path, store: ContentStore): ZIO[Scope, HostCompilerError, HostCompilerSession] =
    jdkHome.flatMap(home => HostCompilerInputs.materialize(payload, home, store))

  def jdkHome: ZIO[Any, HostCompilerError, Path] =
    ZIO
      .attemptBlocking(Option(java.lang.System.getProperty("java.home")))
      .orDie
      .flatMap:
        case Some(home) if home.nonEmpty => ZIO.succeed(Path.of(home))
        case _ => ZIO.fail(HostCompilerError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))

  def payload(root: Path, version: String, tools: List[Pinned]): ZIO[Any, HostCompilerError, Path] =
    val directory = root.resolve("payload")
    for
      scala   <- ZIO.fromEither(ScalaVersion.from(version)).mapError(_ => HostCompilerError.IncompatibleManifest(0))
      entries <- ZIO.foreach(tools.zipWithIndex) { (pinned, index) =>
        place(directory, pinned, index)
      }
      _ <- ZIO
        .attemptBlocking {
          val manifest = HostCompilerInputs.sign(1, scala, entries, Floor)
          val path     = directory.resolve(HostCompilerInputs.ManifestPath)
          Files.createDirectories(path.getParent)
          Files.writeString(path, manifest.toJson, StandardCharsets.UTF_8)
        }
        .mapError(_ => HostCompilerError.Io(IoFailure(directory, IoFault.Write)))
    yield directory

  def write(path: Path, text: String): ZIO[Any, PluginCompileError, Path] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(path.getParent)
        Files.writeString(path, text, StandardCharsets.UTF_8)
        path
      }
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Write)))

  def apiJar(path: Path, body: String): ZIO[Any, PluginCompileError, Path] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(path.getParent)
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          out.putNextEntry(new ZipEntry("marker.txt"))
          out.write(body.getBytes(StandardCharsets.UTF_8))
          out.closeEntry()
        finally out.close()
        path
      }
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Write)))

  def invoke(
      classes: Path,
      runtime: List[Path],
      binary: String,
      method: String,
  ): ZIO[Scope, PluginCompileError, String] =
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking(
            new URLClassLoader((classes :: runtime).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
          )
          .mapError(_ => PluginCompileError.Io(IoFailure(classes, IoFault.Read)))
      )(loader => ZIO.attemptBlocking(loader.close()).ignore)
      .flatMap: loader =>
        ZIO
          .attemptBlocking(called(loader, binary, method))
          .mapError(_ => PluginCompileError.Io(IoFailure(classes, IoFault.Read)))
          .flatMap(ZIO.fromEither)

  def runtimeJars(session: HostCompilerSession): List[Path] =
    session.tools.filter(_.entry.role == HostToolRole.Runtime).map(_.path)

  def lockFile: Option[Path] =
    val start = Path.of(sys.props.getOrElse("user.dir", ".")).toAbsolutePath.normalize
    findLock(start, 8)

  def compilerCell(text: String, version: String): Boolean =
    text.linesIterator.exists(_.trim == s"tool compiler $version jvm")

  def frozen(path: String, bytes: Array[Byte]): Either[PluginCompileError, FrozenFile] =
    RepoPath.from(path) match
      case Left(_)     => Left(PluginCompileError.Io(IoFailure(Path.of(path), IoFault.Read)))
      case Right(repo) => Right(FrozenFile(repo, Digests.content(bytes), bytes.length.toLong))

  def declared(entry: String, file: FrozenFile, envKey: String, envValue: String): StageContract =
    StageContract(
      StageAssurance.ProducerDeclared,
      List(histos.plugin.EntryName.trusted(entry)),
      Nil,
      CompilerConstraint.unconstrained,
      List(
        StageBinding(InputKey.trusted(file.path.value), BindingClass.Public, BindingKind.File),
        StageBinding(InputKey.trusted(envKey), BindingClass.Public, BindingKind.Env),
      ),
      StageInputs(List(file), Nil, List(FrozenEnv.Present(EnvName.trusted(envKey), envValue))),
      StageRestrictions.v1,
    )

  val Scala3101: List[Pinned] = List(
    pin(HostToolRole.Compiler, "org.scala-lang", "scala3-compiler_3", "3.10.1"),
    pin(HostToolRole.Bridge, "org.scala-lang", "scala3-sbt-bridge", "3.10.1"),
    pin(HostToolRole.Interfaces, "org.scala-sbt", "compiler-interface", "1.12.1"),
    pin(HostToolRole.Runtime, "org.scala-lang", "scala-library", "3.10.1"),
    pin(HostToolRole.Support, "org.scala-lang", "scala3-interfaces", "3.10.1"),
    pin(HostToolRole.Support, "org.scala-lang", "tasty-core_3", "3.10.1"),
    pin(HostToolRole.Support, "org.ow2.asm", "asm", "9.11.1"),
    pin(HostToolRole.Support, "org.ow2.asm", "asm-commons", "9.11.1"),
    pin(HostToolRole.Support, "org.ow2.asm", "asm-tree", "9.11.1"),
    pin(HostToolRole.Support, "org.ow2.asm", "asm-util", "9.11.1"),
    pin(HostToolRole.Support, "org.ow2.asm", "asm-analysis", "9.11.1"),
    pin(HostToolRole.Support, "org.scala-lang", "scala3-repl_3", "3.10.1"),
    pin(HostToolRole.Support, "org.jline", "jline-reader", "4.4.8"),
    pin(HostToolRole.Support, "org.jline", "jline-terminal", "4.4.8"),
    pin(HostToolRole.Support, "org.jline", "jline-terminal-jni", "4.4.8"),
    pin(HostToolRole.Support, "org.jline", "jline-native", "4.4.8"),
    pin(HostToolRole.Support, "org.scala-sbt", "util-interface", "1.12.4"),
    pin(HostToolRole.Support, "org.scala-lang", "scala3-directives-parser_3", "3.10.1"),
    pin(HostToolRole.Support, "io.get-coursier", "interface", "1.0.29-M4"),
    pin(HostToolRole.Support, "org.slf4j", "slf4j-api", "1.7.36"),
  )

  /** Not a compiler cell of this project lock. A missing jar is fetched from Maven Central. */
  val Scala384: List[Pinned] = List(
    pin(HostToolRole.Compiler, "org.scala-lang", "scala3-compiler_3", "3.8.4"),
    pin(HostToolRole.Bridge, "org.scala-lang", "scala3-sbt-bridge", "3.8.4"),
    pin(HostToolRole.Interfaces, "org.scala-sbt", "compiler-interface", "1.10.7"),
    pin(HostToolRole.Runtime, "org.scala-lang", "scala-library", "3.8.4"),
    pin(HostToolRole.Support, "org.scala-lang", "scala3-interfaces", "3.8.4"),
    pin(HostToolRole.Support, "org.scala-lang", "tasty-core_3", "3.8.4"),
    pin(HostToolRole.Support, "org.scala-lang.modules", "scala-asm", "9.9.0-scala-1"),
    pin(HostToolRole.Support, "org.scala-lang", "scala3-repl_3", "3.8.4"),
  )

  final case class Ran[A](starts: Int, value: A)

  final case class Pinned(role: HostToolRole, organization: String, name: String, revision: String)

  private def pin(role: HostToolRole, organization: String, name: String, revision: String): Pinned =
    Pinned(role, organization, name, revision)

  private def place(root: Path, pinned: Pinned, index: Int): ZIO[Any, HostCompilerError, HostToolEntry] =
    val source = maven(pinned.organization, pinned.name, pinned.revision)
    val name   = s"$index-${pinned.name}-${pinned.revision}.jar"
    ZIO
      .fromEither(ArtifactPath.from(name))
      .mapError(_ => HostCompilerError.Io(IoFailure(source, IoFault.Read)))
      .flatMap { relative =>
        val dest = root.resolve("bootstrap/compiler").resolve(name)
        ZIO
          .attemptBlocking {
            copyOrFetch(pinned, source, dest).map { _ =>
              val bytes = Files.readAllBytes(dest)
              HostToolEntry(
                pinned.role,
                Dep.trusted(pinned.organization, pinned.name, pinned.revision, DepKind.Java),
                ContentHash.trusted(Sha256Digest.of(Chunk.fromArray(bytes)).value),
                relative,
              )
            }
          }
          .mapError(_ => HostCompilerError.Io(IoFailure(dest, IoFault.Read)))
          .flatMap(ZIO.fromEither)
      }

  /** The coursier cache when it already has the jar. Maven Central otherwise. */
  private def copyOrFetch(pinned: Pinned, source: Path, dest: Path): Either[HostCompilerError, Unit] =
    Files.createDirectories(dest.getParent)
    if Files.isRegularFile(source) then
      Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING)
      Right(())
    else fetch(pinned, dest)

  private def fetch(pinned: Pinned, dest: Path): Either[HostCompilerError, Unit] =
    val group = pinned.organization.replace('.', '/')
    val uri   = URI.create(
      s"https://repo1.maven.org/maven2/$group/${pinned.name}/${pinned.revision}/${pinned.name}-${pinned.revision}.jar"
    )
    val client   = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
    val response = client.send(
      HttpRequest.newBuilder(uri).header("User-Agent", "histos-test").GET().build(),
      HttpResponse.BodyHandlers.ofInputStream(),
    )
    val body = response.body()
    try
      if response.statusCode() == 200 then
        val partial = dest.resolveSibling(dest.getFileName.toString + ".part")
        Files.copy(body, partial, StandardCopyOption.REPLACE_EXISTING)
        Files.move(partial, dest, StandardCopyOption.REPLACE_EXISTING)
        Right(())
      else Left(HostCompilerError.Io(IoFailure(dest, IoFault.Read)))
    finally body.close()

  private def maven(organization: String, name: String, revision: String): Path =
    val group = organization.replace('.', '/')
    cacheRoot.resolve(s"$group/$name/$revision/$name-$revision.jar")

  private def cacheRoot: Path =
    val home = Path.of(sys.props.getOrElse("user.home", "."))
    val mac  = home.resolve("Library/Caches/Coursier/v1/https/repo1.maven.org/maven2")
    val nix  = home.resolve(".cache/coursier/v1/https/repo1.maven.org/maven2")
    if Files.isDirectory(mac) then mac else nix

  private def called(loader: URLClassLoader, binary: String, method: String): Either[PluginCompileError, String] =
    val cls    = loader.loadClass(binary)
    val module = if binary.endsWith("$") then Some(cls.getField("MODULE$").get(null)) else None
    cls.getMethod(method).invoke(module.orNull) match
      case text: String => Right(text)
      case _            => Left(PluginCompileError.Io(IoFailure(Path.of(binary), IoFault.Read)))

  @tailrec
  private def findLock(dir: Path, left: Int): Option[Path] =
    val lock = dir.resolve("dependencies.lock")
    if Files.isRegularFile(lock) then Some(lock)
    else if left == 0 then None
    else
      Option(dir.getParent) match
        case Some(parent) => findLock(parent, left - 1)
        case None         => None

  val App: ProjectName = ProjectName("app")
end PluginCompileSupport
