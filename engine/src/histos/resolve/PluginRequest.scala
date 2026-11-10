package histos.resolve

import histos.plugin.LinkStrategy

/** A published plugin the lock can name. The build file does not parse this from text. */
enum PluginRequest:
  case Dep(dependency: histos.Dep, link: LinkStrategy)
