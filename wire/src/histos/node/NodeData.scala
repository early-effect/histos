package histos.node

enum NodeReject:
  case Absent
  case NoBinary
  case UnreadableVersion(error: NodeVersionError)

  def message: String = this match
    case Absent                   => "not a Node home"
    case NoBinary                 => "no node"
    case UnreadableVersion(error) => error.message
end NodeReject
