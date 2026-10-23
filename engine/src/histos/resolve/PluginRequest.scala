package histos.resolve

import histos.plugin.LinkStrategy
import histos.{Dep, ProjectPath}

/** A header the loader can put on the build classpath. */
enum PluginRequest:
  case Dep(dependency: histos.Dep, link: LinkStrategy)
  case Path(path: ProjectPath)
