package histos.schedule

enum ScheduleError[+E]:
  case UnknownAction(id: ActionId)
  case DuplicateAction(id: ActionId)
  case MissingDependency(action: ActionId, missing: ActionId)
  case Cycle(path: List[ActionId])
  case DuplicateInput(action: ActionId, name: InputName)
  case Failed(action: ActionId, error: E)
  case Cache(error: CacheError)
  case Remote(error: RemoteError)

  def message: String = this match
    case UnknownAction(id)                  => s"no action '${id.value}'"
    case DuplicateAction(id)                => s"action '${id.value}' is declared twice"
    case MissingDependency(action, missing) =>
      s"'${action.value}' depends on '${missing.value}', which is not in the plan"
    case Cycle(path)                  => s"action cycle ${path.map(_.value).mkString(" -> ")}"
    case DuplicateInput(action, name) =>
      s"'${action.value}' declares input '${name.value}' twice"
    case Failed(action, error) => s"'${action.value}' failed: $error"
    case Cache(error)          => error.message
    case Remote(error)         => error.message
end ScheduleError
