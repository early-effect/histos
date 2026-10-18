package histos

/** Where linked Scala.js runs. Node, a DOM, or Firefox. Not a flag. */
enum JsEnv:
  case Node
  case Jsdom
  case Firefox

  def token: String = this match
    case Node    => "node"
    case Jsdom   => "jsdom"
    case Firefox => "firefox"
