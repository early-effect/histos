# CiJob as an open trait

A plugin contributes jobs by implementing `CiJob`. The trait is open. Histos ships `TaskJob` and `SiteJob`. The engine never matches on the trait. It matches the sealed `JobRender` vocabulary that `render` returns: one task, one cron, one site shape, or one fan.

A fan is one module. Each `Target` is a matrix row (a region, an account), not a second job. `needs` names another contribution job or a generated job that is actually on the graph: `test`, `doc`, `workflow`, `launcher`, `affected`, and `cache-seed` when that job is rendered. `pages` is not a `CiJob` and is not a legal need. A job that names itself is `UnknownNeed`. A repeated name is `DuplicateNeed`. The message is `job X needs Y, which is not a job on this graph`.

## Decisions

**Modules and targets.** Modules are separate jobs. Deploy targets are `JobRender.Fan(job, targets)`.

**Affected detection does both.** The engine owns a reserved `affected` job. It writes a JSON list of module names to the output `modules`. Each generated test command is `./histos test --affected '<that json>'`. A job may also carry `Guard.Affected`, which skips the whole job unless `affected` reported that module or `all`. The `test` task still narrows inside the suite, so one cell does not run another module's tests.

The push base is the head SHA of the last successful push of the same workflow on the same branch, when that commit is an ancestor of `HEAD`. Otherwise the run selects every module. A pull request uses the pull-request base. A tag, a dispatch, and any other event select every module. A failed diff fails open to `["all"]`. An empty diff stays `[]` and runs nothing. `histos affected --base <sha>` prints the list. `--base all` prints `["all"]`.

A path belongs to the module whose directory is the longest prefix. Dependents of that module are included. A change to `histos.scala` selects the whole suite. A path that no module owns selects nothing.

**Actions.** `PinnedAction` carries typed inputs and the grants it needs. The catalog maps the action to `repo@sha` plus the tag, printed as `uses: repo@sha # tag`. A missing pin is `UnpinnedAction`. `AwsOidc` is the shipped helper for `aws-actions/configure-aws-credentials` and it requires `IdTokenWrite`. LaunchPad stays in the adopter's build.

**Permissions.** A job carries a set of `Grant`s. The printer always emits `contents: read` unless the set has `contents: write`. A pre-step whose grant is absent is `GrantMissing`. Nothing is implied from the action.

**Environment.** `EnvValue` is `Plain`, `Secret`, `Github`, or `Env`. Graph-wide plain values are workflow `env`. Graph-wide secrets are copied onto each job, because workflow `env` would hand them to every job. A job that runs a `PinnedAction` does not receive graph-wide secrets unless it sets `GraphSecrets.Include`. A job's own env overrides the graph entry of the same name. A secret that is not in `CiSettings.secrets` is `UnknownSecret`.

**Runners.** `CiSettings.runners` maps a `RunnerName` to the `runs-on` label. `Runner.Default` is `ubuntu-26.04`. An unknown name is `UnknownRunner`. The catalog is `ProjectGraph.ci`, not the ship catalog.

**Verify.** There is no gate job. `test`, `doc`, `versions`, and `workflow` run when the event is `pull_request` or `workflow_dispatch`. A push still starts the workflow. Those jobs skip. `cache-seed` runs on a push to the default branch, compiles, and saves the cache. It does not set the cache namespace. The test job is the only writer of that namespace.

**Triggers.** `Trigger` is an open trait. It renders only into the closed `On` and `Cond` vocabularies. `Cond` is an expression tree. The printer is the only place a condition becomes a string. Shipped presets are `Ci`, `DefaultPush`, `Tag`, `Dispatch`, `Label`, `Paths`, and `Nightly` (`TaskJob.nightly`, which is `Dispatch` plus a cron). `SitePublish` (`Automatic`, `Manual`, `Both`, default `Both`) is those presets applied to a site. A site has no cron, so `ScheduledSite` cannot be constructed.

The default branch is one expression: `github.ref == format('refs/heads/{0}', github.event.repository.default_branch)`. The branch name `main` is not hard-coded. The cache namespace token `main` is `CacheNamespace.Main` and is unrelated.

Events that would wake or cancel `ci.yml` are written to `histos-<job>.yml` automatically: a label, a path filter, a cron, and `workflow_run`. `push`, the default pull-request types, `workflow_dispatch`, tags, `merge_group`, and `release` stay in `ci.yml`. A job in `ci.yml` is skipped on an event it did not ask for.

**Defaults.** A `SiteJob` runs on a push to the default branch. It does not run on a pull request or a tag. Upload and the internal `pages` deploy stay on that same push, so a custom trigger that also matches a pull request still does not deploy from the pull request. A job that has `needs` uses `NeedsPolicy.NoneFailed`: it runs after a skipped upstream when nothing failed. The internal `pages` job is not a `CiJob`. It keeps GitHub's default, every need succeeded, so a skipped site build does not deploy.

## Audit

These four rows are closed.

- `site: Option[Site]` is gone. A site is a `SiteJob`. A task is a `TaskJob`.
- A schedule combined with a site does not compile. `ScheduledSite` is not an error case, because the state cannot be built.
- The engine matches `JobRender` only.
- The specular fixture asserts each extracted field on its own, not as one conjunction.

## Migration

`CiJob(name, task)` becomes `TaskJob(name, task)`. The default trigger is still `Trigger.Ci` (a pull request, or a push to the default branch). `JobWhen.Schedule(cron)` becomes `TaskJob(..., cron = Some(cron))`, which renders a cron-only file. `TaskJob.nightly(name, task, cron)` is that cron plus `workflow_dispatch`. `JobPermissions.ContentsWrite` becomes `JobPermissions.write`. `site = Some(Site(...))` becomes `SiteJob(name, task, site)`.

`BuildError.ScheduledSite` and `CiError.ScheduledSite` are removed. New faults live on `CiFault` and, on the wire, as flat `CiError` cases: `IllegalSite`, `UnknownNeed`, `DuplicateNeed`, `UnknownModule`, `DuplicateTarget`, `UnpinnedAction`, `GrantMissing`, `UnknownRunner`, `UnknownSecret`, `UnreachableCond`, `DispatchInputConflict`, `ConcurrencyConflict`, `UnpinnedWorkflowRun`.

Reserved job ids add `affected` and `cache-seed`. The command name `affected` is reserved.
