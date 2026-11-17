package histos.plugin

import histos.lock.LockDefaults
import histos.Resolution

/** Catalog data the sequencer plans from. It does not load classes. */
final case class BootstrapCatalog(
    sources: PluginCatalog,
    intent: ResolutionIntent,
    resolvers: PluginResolvers,
    locks: LockDefaults,
    bootstrap: BootstrapSource,
)

object BootstrapCatalog:
  def from(plugins: HistosPlugins): BootstrapCatalog =
    BootstrapCatalog(
      plugins.sources,
      ResolutionIntent.Select(plugins.resolution),
      plugins.resolvers,
      plugins.bootstrapLocks,
      plugins.bootstrap,
    )

/** Today's only plan: core seed, optional strategy source, then the remaining roots. */
enum BootstrapSteps:
  case CoreFirst(seed: PluginCatalog, early: Option[RepoPath], remaining: PluginCatalog)

enum BootstrapPlanError:
  case SeedOutsideCatalog

  def message: String = this match
    case SeedOutsideCatalog => "the strategy prerequisites are not in the plugin catalog"
end BootstrapPlanError

/** An explicit main resolution that disagrees with the catalog. */
enum BootstrapError:
  case ResolutionConflict(catalog: Resolution, main: Resolution)
  case SeedOutsideCatalog
  case MissingStrategy(path: RepoPath)
  case LateLoading
  case AmbiguousScala(module: String)

  def message: String = this match
    case ResolutionConflict(catalog, main) =>
      s"main selects $main and the catalog selects $catalog"
    case SeedOutsideCatalog     => "the strategy prerequisites are not in the plugin catalog"
    case MissingStrategy(path)  => s"strategy source ${path.value} is not in the catalog closure"
    case LateLoading            => "plugin loading is configured after plugins have loaded"
    case AmbiguousScala(module) => s"module $module declares more than one Scala version"
end BootstrapError

/** Host-injected construction seam. Project code does not run this before the host does. */
trait BootstrapSequencer:
  def plan(catalog: BootstrapCatalog): Either[BootstrapPlanError, BootstrapSteps]

object BootstrapSequencer:
  /** Core defaults, then the optional strategy file, then everything else in catalog order. */
  val coreFirst: BootstrapSequencer = catalog =>
    catalog.bootstrap match
      case BootstrapSource.Core =>
        Right(BootstrapSteps.CoreFirst(PluginCatalog.empty, None, catalog.sources))
      case BootstrapSource.File(path, required) =>
        if required.entries.forall(catalog.sources.entries.contains) then
          val remaining = catalog.sources.entries.filterNot(required.entries.contains)
          Right(BootstrapSteps.CoreFirst(required, Some(path), PluginCatalog(remaining)))
        else Left(BootstrapPlanError.SeedOutsideCatalog)
