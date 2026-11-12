package histos

import histos.resolve.{ClasspathScope, DependencyLock, LockedArtifact, LockedCell, Resolve, ResolveError}
import histos.resolve.{ToolId, ToolRole}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO, ZLayer}

/** What an app law that runs Zinc compiles with, the same shape as engine's fixture, which app's tests cannot read.
  * [[lock]] is this checkout's `dependencies.lock`. [[compilers]] are the Scala 3 compilers it pins, newest first.
  * Every jar of every compiler tool row was read offline from the Coursier cache before the first law, so a cold cache
  * is one named precondition and not a run-order accident.
  */
final case class CompilerClosure(lock: DependencyLock, compilers: ::[ScalaVersion]):
  /** The newest locked Scala 3 compiler. */
  def scalaVersion: ScalaVersion = compilers.head

  /** The lock with an empty JVM compile cell for each module at [[scalaVersion]], in place of any cell the checkout
    * pins under that name. A law's modules name no dependencies, so their cells pin no jars.
    */
  def pinning(modules: ProjectName*): DependencyLock =
    val added = modules.toList.map: module =>
      LockedCell(module, scalaVersion, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile)
    val kept = lock.cells.filterNot: cell =>
      modules.contains(cell.module) && cell.scalaVersion == scalaVersion && cell.platform == Platform.Jvm &&
        cell.scope == ClasspathScope.Compile
    lock.copy(cells = added ++ kept)
end CompilerClosure

object CompilerClosure:
  /** Coursier's cache, read offline. `Resolve` never downloads. */
  val layer: ZLayer[Any, Missing, Resolve & CompilerClosure] =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) >+> ZLayer.fromZIO(checkout.flatMap(read))

  private def read(repo: Path): ZIO[Resolve, Missing, CompilerClosure] =
    for
      lock <- Resolve.readLock(repo).mapError(Missing.Lock(_))
      rows   = lock.tools.filter(_.id.role == ToolRole.Compiler)
      newest = rows
        .map(_.id.scalaVersion)
        .filter(_.value.startsWith("3."))
        .distinct
        .sortBy(numbers)
        .reverse
      found <- newest match
        case first :: rest => ZIO.succeed(CompilerClosure(lock, ::(first, rest)))
        case Nil           => ZIO.fail(Missing.Compiler(Resolve.lockPath(repo)))
      _ <- ZIO.serviceWithZIO[Resolve]: resolve =>
        ZIO.foreachDiscard(rows)(row =>
          ZIO.foreachDiscard(row.artifacts)(artifact =>
            resolve.fetch(artifact).mapError(Missing.Jar(row.id, artifact, _))
          )
        )
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

  private def numbers(version: ScalaVersion): (Int, Int, Int) =
    version.value.split("[.+-]").toList.flatMap(_.toIntOption) match
      case major :: minor :: patch :: _ => (major, minor, patch)
      case _                            => (0, 0, 0)

  /** A precondition of a compiling law. The message names the step that fixes it. */
  enum Missing:
    case Checkout(from: Path)
    case Lock(error: ResolveError)
    case Compiler(lock: Path)
    case Jar(tool: ToolId, artifact: LockedArtifact, error: ResolveError)

    def message: String = this match
      case Checkout(from)         => s"no directory at or above $from has histos.scala and dependencies.lock"
      case Lock(error)            => s"dependencies.lock is not readable (${error.message}). $resolve"
      case Compiler(lock)         => s"$lock pins no Scala 3 compiler. $resolve"
      case Jar(tool, artifact, _) =>
        s"the locked ${tool.token} jar ${artifact.organization}:${artifact.name}:${artifact.revision} is not in the " +
          s"Coursier cache. $resolve"

    override def toString: String = message

    private def resolve: String = "Run `./histos resolve` in this checkout, then run the law again."
  end Missing
end CompilerClosure
