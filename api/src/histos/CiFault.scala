package histos

import histos.plugin.{ActionName, JobName, RunnerName, SecretName, TargetName}

/** Faults of an open [[histos.plugin.CiJob]]. Nested so the workflow schema stays inside the JVM method limit. */
enum CiFault:
  case IllegalSite(name: JobName)
  case UnknownNeed(job: JobName, need: JobName)
  case DuplicateNeed(job: JobName, need: JobName)
  case UnknownModule(job: JobName, module: ProjectName)
  case DuplicateTarget(job: JobName, target: TargetName)
  case UnpinnedAction(job: JobName, action: ActionName)
  case GrantMissing(job: JobName, action: ActionName)
  case UnknownRunner(job: JobName, runner: RunnerName)
  case UnknownSecret(secret: SecretName)
  case UnreachableCond(job: JobName)
  case DispatchInputConflict(name: String)
  case ConcurrencyConflict(file: String)
  case UnpinnedWorkflowRun(name: String)

  def message: String = this match
    case IllegalSite(name)      => s"job ${name.value} does not render one site"
    case UnknownNeed(job, need) =>
      s"job ${job.value} needs ${need.value}, which is not a job on this graph"
    case DuplicateNeed(job, need)   => s"job ${job.value} needs ${need.value} twice"
    case UnknownModule(job, module) =>
      s"job ${job.value} names module ${module.value}, which is not a module"
    case DuplicateTarget(job, target) => s"job ${job.value} names target ${target.value} twice"
    case UnpinnedAction(job, action)  => s"job ${job.value} uses ${action.value}, which has no pin"
    case GrantMissing(job, action)    =>
      s"job ${job.value} uses ${action.value} without its grant"
    case UnknownRunner(job, runner) =>
      s"job ${job.value} names runner ${runner.value}, which is not in the catalog"
    case UnknownSecret(secret)       => s"secret ${secret.value} is not declared"
    case UnreachableCond(job)        => s"job ${job.value} has a condition its events do not reach"
    case DispatchInputConflict(name) =>
      s"workflow_dispatch input $name is declared twice with different types"
    case ConcurrencyConflict(file) => s"jobs in $file do not share a concurrency group"
    case UnpinnedWorkflowRun(name) => s"workflow $name is not rendered by this graph"
end CiFault
