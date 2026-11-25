package histos.plugin

import histos.lock.ExecutionKind
import histos.{ProjectName, Resolution}

/** Local or CI, as the caller declared it. The server environment is not a source. */
enum Interaction:
  case Offers
  case Unattended

/** Artifact fetch and solve are refused when offline. This is not a sandbox. */
enum OfflineSelection:
  case Online
  case Offline

/** The command this invocation will run. Not the wire `Call`. */
enum CommandIdentity:
  case Compile(
      projects: List[ProjectName],
      platforms: histos.PlatformSelection = histos.PlatformSelection.All,
      scala: histos.ScalaSelection = histos.ScalaSelection.All,
  )
  case Test(selection: histos.TestSelection)
  case Run(
      module: CommandToken,
      platforms: Option[histos.PlatformSelection],
      main: Option[histos.MainClass],
      scala: Option[histos.ScalaSelection] = None,
  )
  case Resolve(action: ResolveVerb)
  case Refresh(selectors: List[ProjectName])
  case Task(id: PluginTaskId)
  case Format
  case Publish
  case Other(token: CommandToken)

enum ResolveVerb:
  case Verify
  case Update
  case Check

/** Caller data a service method receives. Construction does not capture it. */
final case class InvocationContext(
    id: InvocationId,
    client: ClientId,
    command: CommandIdentity,
    execution: ExecutionKind,
    interaction: Interaction,
    offline: OfflineSelection,
)

/** Main omission is [[ResolutionIntent.Inherit]], not a request for [[Resolution.Solve]]. */
enum ResolutionIntent:
  case Inherit
  case Select(resolution: Resolution)
