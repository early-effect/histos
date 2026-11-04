package histos.workflow

import histos.{Module, ProjectGraph, ProjectName}

/** Which modules a diff selects. A build-definition change is the whole suite. A path that no module owns selects
  * nothing. Dependents of a changed module are included.
  */
object Affected:
  enum Selection:
    case Everything
    case Modules(names: List[ProjectName])

  def decide(graph: ProjectGraph, paths: List[String]): Selection =
    if paths.exists(buildFile) then Selection.Everything
    else
      val modules = graph.projects.collect { case module: Module => module }
      val roots   = paths.flatMap(path => owner(modules, path)).distinct
      Selection.Modules(close(modules, roots.toSet))

  /** `base == "all"` and a failed diff both select the whole suite. An empty diff selects nothing. */
  def report(graph: ProjectGraph, base: String, paths: Option[List[String]]): String =
    if base == "all" || paths.isEmpty then """["all"]"""
    else encode(decide(graph, paths.getOrElse(Nil)))

  private def encode(selection: Selection): String =
    selection match
      case Selection.Everything     => """["all"]"""
      case Selection.Modules(names) =>
        names.map(name => s"\"${name.value}\"").mkString("[", ",", "]")

  private def buildFile(path: String): Boolean =
    path == "histos.scala" || path.endsWith("/histos.scala")

  private def owner(modules: List[Module], path: String): Option[ProjectName] =
    val ranked = modules.flatMap { module =>
      val root = module.baseDirectory
      if path == root || path.startsWith(s"$root/") then Some(root.length -> module.name) else None
    }
    ranked.sortBy { case (length, name) => (-length, name.value) }.headOption.map(_._2)

  private def close(modules: List[Module], roots: Set[ProjectName]): List[ProjectName] =
    def step(have: Set[ProjectName]): Set[ProjectName] =
      val extra = modules.collect {
        case module if module.upstreamNames.exists(have.contains) => module.name
      }.toSet
      if extra.subsetOf(have) then have else step(have ++ extra)
    val grown = step(roots)
    modules.collect { case module if grown.contains(module.name) => module.name }
end Affected
