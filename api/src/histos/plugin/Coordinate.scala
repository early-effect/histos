package histos.plugin

import histos.{Dep, DepKind, ProjectPath}

/** The header line that named a plugin, before any plugin object exists. */
enum PluginCoordinate:
  case Published(dependency: Dep)
  case Path(path: ProjectPath)

  def render: String = this match
    case Published(dependency) =>
      val separator = dependency.kind match
        case DepKind.Java   => ":"
        case DepKind.Binary => "::"
        case DepKind.Full   => ":::"
      s"${dependency.organization}$separator${dependency.name}:${dependency.revision}"
    case Path(path) => path.value
end PluginCoordinate
