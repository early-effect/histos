package histos.plugin

import histos.{Dep, DepKind, ProjectName}

/** A plugin the graph names before any plugin value exists. A published pin links `BinaryOrSource` unless the build
  * says otherwise.
  */
enum PluginCoordinate:
  case Published(dependency: Dep, link: LinkStrategy = LinkStrategy.BinaryOrSource)
  case Module(name: ProjectName)

  def render: String = this match
    case Published(dependency, _) =>
      val separator = dependency.kind match
        case DepKind.Java   => ":"
        case DepKind.Binary => "::"
        case DepKind.Full   => ":::"
      s"${dependency.organization}$separator${dependency.name}:${dependency.revision}"
    case Module(name) => name.value
end PluginCoordinate
