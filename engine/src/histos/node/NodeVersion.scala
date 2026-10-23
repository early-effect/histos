package histos.node

opaque type NodeMajor = Int

object NodeMajor:
  given CanEqual[NodeMajor, NodeMajor] = CanEqual.derived

  def from(raw: Int): Either[NodeError, NodeMajor] =
    if raw > 0 then Right(raw) else Left(NodeError.BadMajor(raw))

  private[histos] def trusted(raw: Int): NodeMajor = raw

  extension (major: NodeMajor) def value: Int = major

/** A full Node version, such as `22.14.0`. The leading `v` from `node --version` is not part of it. */
final case class NodeVersion private (value: String, major: NodeMajor)

object NodeVersion:
  given CanEqual[NodeVersion, NodeVersion] = CanEqual.derived

  private val Full = """(\d+)\.(\d+)\.(\d+)""".r

  def from(raw: String): Either[NodeVersionError, NodeVersion] =
    val body = if raw.startsWith("v") || raw.startsWith("V") then raw.drop(1) else raw
    body match
      case Full(majorText, _, _) =>
        majorText.toIntOption match
          case Some(major) =>
            NodeMajor.from(major) match
              case Right(pinned) => Right(NodeVersion(body, pinned))
              case Left(_)       => Left(NodeVersionError.Bad(raw))
          case None => Left(NodeVersionError.Bad(raw))
      case _ => Left(NodeVersionError.Bad(raw))

  private[histos] def trusted(text: String, major: NodeMajor): NodeVersion = NodeVersion(text, major)

enum NodeVersionError:
  case Bad(raw: String)
  case Status(code: Int)
  case Missing
  case NotExecutable(binary: java.nio.file.Path)

  def message: String = this match
    case Bad(raw)              => s"not a Node version: $raw"
    case Status(code)          => s"node --version exited $code"
    case Missing               => "node binary is missing"
    case NotExecutable(binary) => s"$binary is not executable"
