package histos.publish

import histos.ProjectName

import zio.json.*
import histos.report.WireJson.given JsonCodec[ProjectName]

/** Which published JVM members `publish check` and `publish local` compare. `All` is every member. `Selected` is only
  * those names. A name that is not a published JVM member adds nothing. Snapshots and release do not narrow.
  */
enum ProofScope derives JsonCodec:
  case All
  case Selected(names: Set[ProjectName])

  def allows(name: ProjectName): Boolean =
    this match
      case ProofScope.All             => true
      case ProofScope.Selected(names) => names.contains(name)
end ProofScope
