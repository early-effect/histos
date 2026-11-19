package histos.server

import zio.json.JsonCodec

/** What `histos complete` asks a running server. Nothing here compiles or resolves. */
enum CatalogQuery derives JsonCodec:
  case Projects
  case Modules
  case Plugins
  case Flags(command: String)

object CatalogQuery:
  def parse(args: List[String]): Option[CatalogQuery] =
    args match
      case "projects" :: Nil                       => Some(Projects)
      case "modules" :: Nil                        => Some(Modules)
      case "plugins" :: Nil                        => Some(Plugins)
      case "flags" :: name :: Nil if name.nonEmpty => Some(Flags(name))
      case _                                       => None
