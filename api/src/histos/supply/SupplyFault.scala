package histos.supply

import histos.plugin.{ActionName, ActionPin, RepoPath}
import histos.version.CommitSha

/** What `deps` and the pin scan refuse. Doctor findings are [[SupplyDiagnostic]], not this enum. */
enum SupplyFault:
  case UnpinnedUses(file: RepoPath, line: Int, ref: String)
  case PinWithoutTag(file: RepoPath, line: Int, action: ActionName)
  case PinMismatch(file: RepoPath, line: Int, action: ActionName, found: ActionPin, expected: ActionPin)
  case TagNotReachable(action: ActionName, tag: String, sha: CommitSha)
  case ActionRepoMoved(action: ActionName, was: RepoId, now: RepoId)
  case CrossVersionSplit(dep: Coordinate, cells: List[CellName])
  case Advisory(finding: Finding)
  case IgnoreExpired(id: AdvisoryId, until: Date)
  case IgnoreNeedsExpiry(id: AdvisoryId)
  case IgnoreTooLong(id: AdvisoryId, max: Days)
  case IgnoresDisallowed(ids: List[AdvisoryId])
  case ServiceDown(source: SourceName, strategy: OutageStrategy)
  case IdentityMissing
  case HumanCommitsOnBranch(branch: BranchName, shas: List[CommitSha])
  case HeadMoved(branch: BranchName, expected: CommitSha)

  def message: String = this match
    case UnpinnedUses(file, line, ref) =>
      s"${file.value}:$line\n  $ref is not pinned: need owner/repo@<40-hex> # <tag>"
    case PinWithoutTag(file, line, action) =>
      s"${file.value}:$line\n  ${action.value} is pinned without a tag comment"
    case PinMismatch(file, line, action, found, expected) =>
      s"${file.value}:$line\n  ${action.value}@${abbrev(found.sha)} # ${found.tag} does not match the catalog pin ${abbrev(expected.sha)} # ${expected.tag}"
    case TagNotReachable(action, tag, sha) =>
      s"${action.value}@$tag does not reach ${sha.value}"
    case ActionRepoMoved(action, was, now) =>
      s"${action.value} moved from ${was.value} to ${now.value}"
    case CrossVersionSplit(dep, cells) =>
      s"${dep.value} would split ${cells.map(_.value).mkString(", ")}"
    case Advisory(finding) =>
      s"${finding.id.value} ${finding.severity} ${finding.summary}"
    case IgnoreExpired(id, until) =>
      s"ignore ${id.value} expired on ${until.value}"
    case IgnoreNeedsExpiry(id) =>
      s"ignore ${id.value} needs an expiry"
    case IgnoreTooLong(id, max) =>
      s"ignore ${id.value} is longer than ${max.value} days"
    case IgnoresDisallowed(ids) =>
      s"ignores are not allowed: ${ids.map(_.value).mkString(", ")}"
    case ServiceDown(source, _) =>
      s"${source.value} is down"
    case IdentityMissing =>
      "Identity.FirstDeveloper needs one developer on the organization"
    case HumanCommitsOnBranch(branch, shas) =>
      s"${branch.value} has human commits ${shas.map(_.abbreviate.value).mkString(", ")}"
    case HeadMoved(branch, expected) =>
      s"${branch.value} moved from ${expected.value}"

  private def abbrev(sha: CommitSha): String =
    sha.value.take(8) + "..."
end SupplyFault
