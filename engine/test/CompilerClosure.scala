package histos

import histos.resolve.{ClasspathScope, DependencyLock, LockedArtifact, LockedCell, LockedTool, Resolve, ResolveError}
import histos.resolve.{ToolId, ToolRole}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO, ZLayer}

/** What a law that runs Zinc compiles with. [[lock]] is this checkout's `dependencies.lock`. [[compilers]] are the
  * Scala 3 compilers it pins, newest first. Every jar of every compiler tool row was read offline from the Coursier
  * cache before the first law, so a cold cache is one named precondition and not a run-order accident. [[held]] does
  * the same for another locked tool, and [[nativeScala]] names the Scala version a Native cell compiles at.
  */
final case class CompilerClosure(lock: DependencyLock, compilers: ::[ScalaVersion]):
  /** The newest locked Scala 3 compiler. */
  def scalaVersion: ScalaVersion = compilers.head

  /** The newest locked compiler of an older minor series. It cannot read [[scalaVersion]]'s TASTy. */
  def older: IO[CompilerClosure.Missing, ScalaVersion] =
    ZIO
      .fromOption(
        compilers
          .find(found => Ordering[(Int, Int)].lt(CompilerClosure.series(found), CompilerClosure.series(scalaVersion)))
      )
      .orElseFail(CompilerClosure.Missing.OneSeries(scalaVersion))

  /** The newest locked compiler older than [[scalaVersion]], of any series. [[scalaVersion]] reads its TASTy. */
  def previous: IO[CompilerClosure.Missing, ScalaVersion] =
    compilers match
      case _ :: next :: _ => ZIO.succeed(next)
      case _              => ZIO.fail(CompilerClosure.Missing.OneCompiler(scalaVersion))

  /** The Scala version of the lock's newest Scala 3 `nativelibraries` row, with every jar of that role read offline. A
    * law that compiles Native compiles at this version, because Scala Native can trail the newest compiler.
    */
  def nativeScala: ZIO[Resolve, CompilerClosure.Missing, ScalaVersion] =
    held(ToolRole.NativeLibraries).flatMap: rows =>
      CompilerClosure.scala3(rows.map(_.id.scalaVersion)) match
        case newest :: _ => ZIO.succeed(newest)
        case Nil         => ZIO.fail(CompilerClosure.Missing.Scala3(ToolRole.NativeLibraries))

  /** Every jar of the lock's rows for `role`, read offline. A law that runs another locked tool asks for it here. */
  def held(role: ToolRole): ZIO[Resolve, CompilerClosure.Missing, List[LockedTool]] =
    lock.tools.filter(_.id.role == role) match
      case Nil  => ZIO.fail(CompilerClosure.Missing.Tool(role))
      case rows =>
        ZIO.serviceWithZIO[Resolve]: resolve =>
          ZIO
            .foreachDiscard(rows)(row =>
              ZIO.foreachDiscard(row.artifacts)(artifact =>
                resolve.fetch(artifact).mapError(CompilerClosure.Missing.Jar(row.id, artifact, _))
              )
            )
            .as(rows)

  /** The lock with an empty JVM compile cell for each module at [[scalaVersion]], in place of any cell the checkout
    * pins under that name. A law's modules name no dependencies, so their cells pin no jars.
    */
  def pinning(modules: ProjectName*): DependencyLock =
    pinningAt(modules.map(_ -> scalaVersion)*)

  /** [[pinning]] with each module's Scala version named. */
  def pinningAt(cells: (ProjectName, ScalaVersion)*): DependencyLock =
    val added = cells.toList.map: (module, version) =>
      LockedCell(module, version, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    val kept = lock.cells.filterNot: cell =>
      added.exists(next => next.module == cell.module && next.scalaVersion == cell.scalaVersion) &&
        cell.platform == Platform.Jvm && cell.scope == ClasspathScope.Compile
    lock.copy(cells = added ++ kept)
end CompilerClosure

object CompilerClosure:
  /** Coursier's cache, read offline. `Resolve` never downloads. */
  val layer: ZLayer[Any, Missing, Resolve & CompilerClosure] =
    in(_root_.coursier.cache.CacheDefaults.location.toPath)

  def in(cache: Path): ZLayer[Any, Missing, Resolve & CompilerClosure] =
    Resolve.offline(cache) >+> ZLayer.fromZIO(checkout.flatMap(read))

  private def read(repo: Path): ZIO[Resolve, Missing, CompilerClosure] =
    for
      lock <- Resolve.readLock(repo).mapError(Missing.Lock(_))
      newest = scala3(lock.tools.filter(_.id.role == ToolRole.Compiler).map(_.id.scalaVersion))
      found <- newest match
        case first :: rest => ZIO.succeed(CompilerClosure(lock, ::(first, rest)))
        case Nil           => ZIO.fail(Missing.Compiler(Resolve.lockPath(repo)))
      _ <- found.held(ToolRole.Compiler)
    yield found

  /** The nearest directory with a build file and a lock. The test JVM may run below the checkout root. */
  private def checkout: IO[Missing, Path] =
    val here = Path.of("").toAbsolutePath.normalize
    ZIO
      .attemptBlocking(
        Iterator
          .unfold(Option(here))(dir => dir.map(found => (found, Option(found.getParent))))
          .find(dir => Files.isRegularFile(Resolve.lockPath(dir)) && Files.isRegularFile(dir.resolve("histos.scala")))
      )
      .orElseFail(Missing.Checkout(here))
      .someOrFail(Missing.Checkout(here))

  /** The Scala 3 versions among `versions`, newest first. */
  private def scala3(versions: List[ScalaVersion]): List[ScalaVersion] =
    versions.filter(_.value.startsWith("3.")).distinct.sortBy(numbers).reverse

  private def numbers(version: ScalaVersion): (Int, Int, Int) =
    version.value.split("[.+-]").toList.flatMap(_.toIntOption) match
      case major :: minor :: patch :: _ => (major, minor, patch)
      case _                            => (0, 0, 0)

  private def series(version: ScalaVersion): (Int, Int) =
    val (major, minor, _) = numbers(version)
    (major, minor)

  /** A precondition of a compiling law. The message names the step that fixes it. */
  enum Missing:
    case Checkout(from: Path)
    case Lock(error: ResolveError)
    case Compiler(lock: Path)
    case Tool(role: ToolRole)
    case Scala3(role: ToolRole)
    case Jar(tool: ToolId, artifact: LockedArtifact, error: ResolveError)
    case OneSeries(newest: ScalaVersion)
    case OneCompiler(newest: ScalaVersion)

    def message: String = this match
      case Checkout(from)         => s"no directory at or above $from has histos.scala and dependencies.lock"
      case Lock(error)            => s"dependencies.lock is not readable (${error.message}). $resolve"
      case Compiler(lock)         => s"$lock pins no Scala 3 compiler. $resolve"
      case Tool(role)             => s"dependencies.lock has no ${role.token} tool row. $resolve"
      case Scala3(role)           => s"dependencies.lock has no Scala 3 ${role.token} tool row. $resolve"
      case Jar(tool, artifact, _) =>
        s"the locked ${tool.token} jar ${artifact.organization}:${artifact.name}:${artifact.revision} is not in the " +
          s"Coursier cache. $resolve"
      case OneSeries(newest)   => s"dependencies.lock pins no Scala 3 compiler older than the ${newest.value} series"
      case OneCompiler(newest) => s"dependencies.lock pins no Scala 3 compiler older than ${newest.value}"

    override def toString: String = message

    private def resolve: String = "Run `./histos resolve` in this checkout, then run the law again."
  end Missing
end CompilerClosure
