# Site publish modes

This file is the decision for [#90](https://github.com/early-effect/histos/issues/90). Implementation has not started. The catalog is `manual("histos", manual"0.1.3")`, so a breaking change that makes this API honest is in scope.

A job that names a site builds that site on every run of the job. Publishing it is a mode on the site. `Automatic` deploys on a push to the branch `ci.yml` already triggers, after the site task. `Manual` deploys from a generated `workflow_dispatch` workflow. `Both` is the two together. A pull request builds the site and does not request the `github-pages` environment.

The deploy is a separate job in `ci.yml`. It `needs` the build job. The build uploads the site with `actions/upload-pages-artifact` only on that push. The deploy job carries `environment: github-pages` and runs `actions/deploy-pages`. A job-level `if` skips the deploy job on every other event, so the environment is never requested. Gating the upload and deploy steps inside the build job leaves the environment on that job, and GitHub evaluates it before any step runs.

No new layer. The model is data on `CiJob`. The graph check is `Either[BuildError, _]`. Rendering is `Either[CiError, _]`.

## Current state

`CiJob` is the whole CI contribution ([Contribution.scala](../api/src/histos/plugin/Contribution.scala) lines 51-61). The site is an optional path. `Some` means "upload this directory and deploy it on every run of this job." `None` means the task runs and the pages actions are omitted. That `Option` is the publish decision.

```scala
final case class CiJob(
    name: JobName,
    task: TaskName,
    when: JobWhen,
    permissions: JobPermissions = JobPermissions.ContentsRead,
    site: Option[RepoPath] = None,
)

enum JobWhen:
  case Always
  case Schedule(cron: Cron)
```

`JobWhen.Always` is rendered into `ci.yml`. `JobWhen.Schedule` is rendered into its own `histos-<job>.yml`. Both paths call `jobLines`. `pluginJobs` is [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 242-248. `scheduled` is lines 503-518 and calls the same `jobLines` (lines 520-533):

```scala
private def pluginJobs(plugins: List[RegisteredPlugin]): List[String] =
  plugins.flatMap: plugin =>
    plugin.contribution.jobs.flatMap: job =>
      job.when match
        case JobWhen.Always      => List(jobLines(plugin, job).mkString("\n"))
        case JobWhen.Schedule(_) => Nil

private def jobLines(plugin: RegisteredPlugin, job: CiJob): List[String] =
  val run = quote(s"./histos run ${PluginTaskId(plugin.id, job.task).render}")
  List(
    s"  ${job.name.value}:",
    s"    runs-on: $Runner",
  ) ++ siteEnvironment(job) ++ List(
    "    permissions:",
    s"      contents: ${access(job.permissions)}",
  ) ++ sitePermissions(job) ++ List(
    "    steps:",
    s"      - uses: $Checkout",
  ) ++ coursierCli ++ List(
    s"      - run: $run"
  ) ++ siteSteps(job)
```

The pages keys are unconditional once `site` is set. They sit on the same job as `./histos run` ([Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 535-564).

```scala
private def siteEnvironment(job: CiJob): List[String] =
    job.site match
      case None    => Nil
      case Some(_) =>
        List(
          "    environment:",
          "      name: github-pages",
          "      url: ${{ steps.deployment.outputs.page_url }}",
        )

  private def sitePermissions(job: CiJob): List[String] =
    job.site match
      case None    => Nil
      case Some(_) =>
        List(
          "      pages: write",
          "      id-token: write",
        )

  private def siteSteps(job: CiJob): List[String] =
    job.site match
      case None       => Nil
      case Some(path) =>
        List(
          s"      - uses: $UploadPages",
          "        with:",
          s"          path: ${quote(path.value)}",
          "      - id: deployment",
          s"        uses: $DeployPages",
        )
```

`UploadPages` is `actions/upload-pages-artifact@v5`. `DeployPages` is `actions/deploy-pages@v5`. `Checkout` is `actions/checkout@v7`. The runner is `ubuntu-26.04`. Those pins stay.

`ci.yml` runs on every pull request and on a push to `main`. There is no `workflow_dispatch` on that file. The branch is a string literal, repeated in the cache keys ([Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 571-584).

```scala
private val header: List[String] =
    List(
      "name: ci",
      "on:",
      "  pull_request:",
      "  push:",
      "    branches:",
      "      - main",
      "permissions:",
      "  contents: read",
      "jobs:",
    )
```

The same literal is the non-PR cache namespace at `Workflow.scala` lines 849-852 and 884-885. A push that is not `main` does not start this workflow. "The default branch" in this generator is that literal.

`renderAll` emits `ci.yml`, then `histos-release.yml` and `histos-promote.yml` when the catalog asks, then one file per schedule (`Workflow.scala` lines 114-125). A stale `histos-*.yml` is deleted on update (`Workflow.scala` lines 961-968). A pages file has to use that prefix or a removed site leaves the file behind.

Graph load checks job names and that the task exists. It does not look at `site` ([Graph.scala](../api/src/histos/Graph.scala) lines 766-773).

```scala
private def jobProblem(
    plugin: RegisteredPlugin,
    tasks: List[TaskName],
): Option[BuildError] =
  plugin.contribution.jobs.collectFirst:
    case job if job.name.isReserved       => BuildError.ReservedJob(job.name)
    case job if !tasks.contains(job.task) =>
      BuildError.UnknownPluginTask(plugin.id, job.task)
```

Reserved ids are `test`, `versions`, `workflow`, and `launcher` (`api/src/histos/plugin/Names.scala` lines 109-114). The generator also emits a job named `doc` when `DocCheck` is on (`Workflow.scala` lines 903-916). `doc` is not reserved. That hole is in the audit ledger. It is not this feature.

A site job today is one job. For `CiJob(JobName("docs"), TaskName("site"), JobWhen.Always, site = Some(RepoPath("target/site")))` on plugin `dev.example`, the job is:

```yaml
  docs:
    runs-on: ubuntu-26.04
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    permissions:
      contents: read
      pages: write
      id-token: write
    steps:
      - uses: actions/checkout@v7
      - uses: ./.github/actions/setup
      - run: "./histos run dev.example/site"
      - uses: actions/upload-pages-artifact@v5
        with:
          path: "target/site"
      - id: deployment
        uses: actions/deploy-pages@v5
```

That job is in a workflow whose `on` includes `pull_request`. The environment is requested for `refs/pull/<n>/merge` before the site task starts.

## Types

`dir` is `RepoPath`, the path type `CiJob.site` already stored. The job field stays `site`. A publish mode is a member of `Site`, so it cannot be set without one.

```scala
enum SitePublish:
  case Automatic
  case Manual
  case Both

final case class Site(dir: RepoPath, publish: SitePublish = SitePublish.Both)

final case class CiJob(
    name: JobName,
    task: TaskName,
    when: JobWhen,
    permissions: JobPermissions = JobPermissions.ContentsRead,
    site: Option[Site] = None,
)
```

`Site` and `SitePublish` live in `histos.plugin`, next to `CiJob`. There is no `Site` type elsewhere. A job with a site and no mode is `Both`, because the default argument is `SitePublish.Both`. `site = None` still means the task runs and nothing is published.

`JobName.reserved` gains `pages`. That is the deploy job id in `ci.yml`. One repository has one Pages site, so one deploy job is enough, and a suffix of the plugin job name can collide with another job.

## Errors

Two illegal configurations are representable if we only add the types. Both are refused.

| Fact | When | Error |
| --- | --- | --- |
| Two jobs carry a `Site` | `Projects.checkPlugins`, across every plugin | `BuildError.DuplicateSite(first: JobName, second: JobName)` |
| A `JobWhen.Schedule` job carries a `Site` | the same check | `BuildError.ScheduledSite(name: JobName)` |
| A job is named `pages` | the existing reserved-name check | `BuildError.ReservedJob` |

`DuplicateSite` is required by the platform. `actions/upload-pages-artifact@v5` defaults the artifact name to `github-pages`, and a workflow run can upload that name once. GitHub Pages is one site per repository. A pull-request preview environment is a different feature.

`ScheduledSite` follows the semantics. `Automatic` deploys on a push to `main`. `Manual` deploys from `workflow_dispatch`. A cron is neither. A schedule that should build the site and not publish it uses `site = None`. The task still runs.

`ProjectGraph.trusted` skips `checkPlugins`. `Workflow.render` and `Workflow.renderAll` refuse the same two facts as `CiError`, so a trusted graph cannot emit two uploads or a cron that requests `github-pages`.

```scala
enum BuildError:
  case DuplicateSite(first: JobName, second: JobName)
  case ScheduledSite(name: JobName)

enum CiError:
  case DuplicateSite(first: JobName, second: JobName)
  case ScheduledSite(name: JobName)
```

`message` on each enum gains an arm. `Schema[BuildError]`, `JsonCodec[BuildError]`, `Schema[CiError]`, and `JsonCodec[CiError]` are derived. They pick up the cases. Do not add a hand-written codec and do not flatten either error to `String`.

Load is the user-facing gate. Render repeats the check because the test graphs are trusted. The two enums stay apart: `BuildError` is graph load, `CiError` is the workflow effect.

## Generator

`jobLines` stops deciding the deploy. A private enum in `Workflow` names the file being written. A boolean parameter would reintroduce the thing this change removes.

```scala
private enum PagesFile:
  case Ci
  case Schedule
  case Dispatch
```

The match:

| `SitePublish` | `PagesFile.Ci` | `PagesFile.Dispatch` | `PagesFile.Schedule` |
| --- | --- | --- | --- |
| `Automatic` | build, upload guarded, deploy job guarded | file not emitted | not reached (`ScheduledSite`) |
| `Manual` | build only | one job, build then upload then deploy | not reached |
| `Both` | same `ci.yml` as `Automatic` | same file as `Manual` | not reached |
| no `Site` | build only, as today | file not emitted | build only, as today |

The push condition is one value, shared with the `on.push.branches` entry:

```scala
private val PushBranch: String = "main"

private val OnPushBranch: String =
  s"github.event_name == 'push' && github.ref == 'refs/heads/$PushBranch'"
```

`header` prints `PushBranch`. The cache namespace expressions that spell `main` print it too. One literal cannot drift from the other.

`ci.yml` for `Automatic` and `Both`, after the plugin build job and before `workflow`:

- The build job keeps the plugin's job name, runner, contents permission, checkout, setup, and `./histos run`.
- It gains `actions: write`. The permission sits on the job for every event: a permissions block cannot be guarded by the step `if`. The workflow header sets `contents: read`, and any permissions block sets every omitted scope to `none` ([workflow syntax](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax#permissions)). `upload-pages-artifact@v5` uploads through `actions/upload-artifact@v7`. That action's own test workflow grants `actions: write`, and a restricted token 403s on `FinalizeArtifact` without it.
- The upload step is guarded with `if: "<OnPushBranch>"`. The path is `site.dir`. The artifact name stays the action default, `github-pages`.
- A job named `pages` follows. `needs` is the build job. `if` is the same guard. `environment` is `github-pages`. The only step is `deploy-pages`, id `deployment`.
- The deploy job's permissions are `actions: read`, `contents: read`, `id-token: write`, `pages: write`. The Pages doc requires the last two. `actions: read` is there so the restricted token can read the artifact. `pages: write` and `id-token: write` are not on the build job.
- `concurrency` on the deploy job is `group: pages`, `cancel-in-progress: false`. The build job is not in the group, so a pull request is not queued behind a deploy.

`histos-pages.yml` (`Workflow.pagesName`) is emitted for `Manual` and `Both`, next to the release file in `renderAll`. `on` is `workflow_dispatch` with no inputs. The single job is named `pages`, checks out the ref the dispatcher chose, runs the site task, uploads `site.dir`, and deploys. The job carries `environment: github-pages`. A dispatch is not a pull request ref, so one job is legal here. Workflow-level concurrency uses the same `pages` group. Concurrency groups are repository-wide, so a push deploy and a dispatch deploy queue, and the one already running finishes.

The dispatch workflow does not add its own branch `if`. The `github-pages` environment is the branch policy. A histos-side `refs/heads/main` check would refuse a repository whose environment allows another branch.

`configure-pages` is not emitted. The site task owns the base path. Specular already generates pages with the project-site base path.

A schedule file never calls the upload or deploy helpers. Render of a schedule that carries a `Site` is `Left(CiError.ScheduledSite)` before any YAML is built.

`actions: write` is the scope to grant. The syntax table also lists `artifact-metadata: write` for creating artifact storage records. Add that scope on the uploading job if a run 403s naming it. Do not put it in the golden until a run says so. The release workflow's `actions/upload-artifact@v7` steps currently grant only `contents: read` (`Workflow.scala` lines 277-300 and 333-358). That is the same restricted-token hole. It is a ledger hit, fixed on its own, not by this feature.

## Samples

Plugin `dev.example`, job `docs`, task `site`, directory `target/site`. `test` and `workflow` are the jobs in [plugin-always.yml](../engine/test/fixtures/ci/plugin-always.yml) and are omitted below. They do not change.

`Manual` emits the same `ci.yml` as a job with `site = None`. `Both` emits the same `ci.yml` as `Automatic`, and the same `histos-pages.yml` as `Manual`.

### ci.yml, no site, and Manual

```yaml
  docs:
    runs-on: ubuntu-26.04
    permissions:
      contents: read
    steps:
      - uses: actions/checkout@v7
      - uses: ./.github/actions/setup
      - run: "./histos run dev.example/site"
```

| Event | What runs |
| --- | --- |
| `pull_request` | `docs` runs the site task. No upload. No `pages` job exists. The `github-pages` environment is not requested. |
| push to `main` | The same job. Nothing is deployed. |
| `workflow_dispatch` | `ci.yml` has no such trigger. |

### ci.yml, Automatic and Both

```yaml
  docs:
    runs-on: ubuntu-26.04
    permissions:
      actions: write
      contents: read
    steps:
      - uses: actions/checkout@v7
      - uses: ./.github/actions/setup
      - run: "./histos run dev.example/site"
      - if: "github.event_name == 'push' && github.ref == 'refs/heads/main'"
        uses: actions/upload-pages-artifact@v5
        with:
          path: "target/site"
  pages:
    needs: docs
    if: "github.event_name == 'push' && github.ref == 'refs/heads/main'"
    runs-on: ubuntu-26.04
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    concurrency:
      group: pages
      cancel-in-progress: false
    permissions:
      actions: read
      contents: read
      id-token: write
      pages: write
    steps:
      - id: deployment
        uses: actions/deploy-pages@v5
```

| Event | What runs |
| --- | --- |
| `pull_request` | `docs` runs the site task. The upload step is skipped. `pages` is skipped by its job `if` before it is sent to a runner. The environment is not requested. |
| push to `main` | `docs` runs the task, then uploads `target/site`. `pages` runs, requests `github-pages`, and deploys that artifact. |
| `workflow_dispatch` | `ci.yml` has no such trigger. `Automatic` has no other file, so nothing deploys. `Both` deploys from the file below. |

A failed site task fails `docs`. `pages` is skipped because `needs` requires success. The guard is not `always()`.

### histos-pages.yml, Manual and Both

```yaml
name: pages
on:
  workflow_dispatch:
permissions:
  contents: read
concurrency:
  group: pages
  cancel-in-progress: false
jobs:
  pages:
    runs-on: ubuntu-26.04
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    permissions:
      actions: write
      contents: read
      id-token: write
      pages: write
    steps:
      - uses: actions/checkout@v7
      - uses: ./.github/actions/setup
      - run: "./histos run dev.example/site"
      - uses: actions/upload-pages-artifact@v5
        with:
          path: "target/site"
      - id: deployment
        uses: actions/deploy-pages@v5
```

| Event | What runs |
| --- | --- |
| `pull_request` | This file does not run. |
| push to `main` | This file does not run. |
| `workflow_dispatch` | `pages` builds the site on the ref the dispatcher chose, uploads it, and deploys it. The environment applies its own branch rules to that ref. |

## Why the deploy is its own job

The owner wanted the upload and deploy as guarded steps on the build job. That loses. `environment` is `jobs.<job_id>.environment`. The keys are `name`, `url`, and `deployment`. There is no `if`. The syntax note says all deployment protection rules must pass before a job that references the environment is sent to a runner ([environment](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax#jobsjob_idenvironment), [if](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax#jobsjob_idif)).

A step `if` runs on the runner, after that check. Histos puts the environment on the build job (`siteEnvironment`, lines 535-543) inside a workflow that triggers on `pull_request` (lines 573-584). The pull request ref is `refs/pull/<n>/merge`. Deployment branch rules match `GITHUB_REF` ([deployment branches](https://docs.github.com/en/actions/reference/workflows-and-actions/deployments-and-environments)). Since December 2025 those rules evaluate a `pull_request` run against `refs/pull/number/merge`, not the head branch ([changelog](https://github.blog/changelog/2025-11-07-actions-pull_request_target-and-environment-branch-protections-changes/)). The `github-pages` environment allows the default branch. The job fails before the site task.

Putting `if` on the build job would skip that failure and would also skip the site task. Pull requests have to build the site.

`environment.name` may be an expression. The allowed contexts include `github`. The docs do not define an empty string as "no environment." `deployment: false` still references the environment, skips creating a deployment object, and is incompatible with custom protection rules. `github-pages` is a deployment environment. That flag is the wrong tool.

A separate workflow triggered by `workflow_run` can see the artifact only through a second download, and it runs with the default branch's token. The Pages artifact does not need a second workflow. `upload-pages-artifact` and `deploy-pages` are built to sit in one run, joined by `needs`.

GitHub's Pages doc is the shape we generate. The build job uploads. The deploy job `needs` the build, sets `environment: github-pages`, and runs `deploy-pages` ([separate build and deploy](https://docs.github.com/en/pages/getting-started-with-github-pages/using-custom-workflows-with-github-pages)). The directory is an input of the upload action. `deploy-pages` takes no path. It deploys the `github-pages` artifact from that run. The path crosses the job boundary as that artifact.

A build job that kept the environment and guarded the steps still failed pull requests in practice. The failure was `Branch "refs/pull/*/merge" is not allowed to deploy to github-pages due to environment protection rules`, with the environment on the job that also built the site ([proof-systems fix](https://github.com/o1-labs/proof-systems/commit/24003ff0c3e64eab5405f475dcf4f6e979aa5754)). The fix was a deploy job whose `if` is the branch, with the environment only on that job.

| Shape | Pull request builds the site | Environment requested on a pull request |
| --- | --- | --- |
| Guarded steps, environment on the build job | yes | yes, before the steps |
| `if` on the build job | no | no |
| Expression as `environment.name`, or `deployment: false` | yes | the job still references an environment |
| `workflow_run` workflow | yes | no, and the artifact hop is a second protocol |
| Deploy job `needs` the build, job `if` on the deploy, upload step `if` on the build | yes | no |

The last row is the one to implement.

## Migration

`0.1.3` is a `0.x` release. The `site` parameter changes type. Source that passes `Some(RepoPath(...))` stops compiling. Source that omits `site` still compiles and still does not publish.

This repository does not set `site`. The known consumer is Specular. The stand-in in this repo contributes the job with no site ([SpecularFixtureSpec.scala](../engine/test/SpecularFixtureSpec.scala) line 73):

```scala
jobs = List(CiJob(JobName("docs"), TaskName("site"), JobWhen.Always))
```

That is the configuration the issue describes: CI runs the site task and publishes nothing. It keeps compiling. The fixture writes `docs/index.html` and reads the `docs` directory, so the pages directory in the example is `docs`. The real plugin chooses its own directory.

The setting that fails pull requests today:

```scala
CiJob(
  JobName("docs"),
  TaskName("site"),
  JobWhen.Always,
  site = Some(RepoPath("docs")),
)
```

After, the same intent (build on pull requests, deploy on `main`, and a dispatch) is the default mode:

```scala
CiJob(
  JobName("docs"),
  TaskName("site"),
  JobWhen.Always,
  site = Some(Site(RepoPath("docs"))),
)
```

`SitePublish.Manual` is the explicit form when a person should publish and a push to `main` should not:

```scala
site = Some(Site(RepoPath("docs"), SitePublish.Manual))
```

Recommend `Both` for Specular. The pages already have a base path, and a docs job that can only publish by a separate edit will go stale. Pull requests still only build.

What else changes for a caller who already set a path:

- The generated build job loses `environment`, `pages: write`, and `id-token: write`. Pull requests stop failing the environment check.
- `Automatic` and `Both` add the `pages` job and the guarded upload. `histos workflow --check` reports drift until the file is updated.
- `Manual` and `Both` add `histos-pages.yml`. Update deletes that file when the mode no longer emits it.
- A second `Site` in the graph fails at load with `DuplicateSite`.
- A `Site` on a cron job fails at load with `ScheduledSite`. Today that job would deploy on the cron, environment included, because `scheduled` calls `jobLines`.
- A plugin job named `pages` fails with `ReservedJob`.
- A pattern match that binds the site field as `Option[RepoPath]` stops compiling. The fixture's `CiJob(name, task, JobWhen.Always, _, _)` still matches.

## Test plan

ZIO Test. New suites are `val`s, as `WorkflowSpec`'s `plugins` suite already is. Assertions are equality against a golden or equality with a typed error. A conjunction of `contains` calls hides which clause failed. The current site test is that shape (`engine/test/WorkflowSpec.scala` lines 435-466) and is replaced, not extended.

Goldens under `engine/test/fixtures/ci/`:

| File | Body |
| --- | --- |
| `plugin-always.yml` | unchanged, and equal to `Manual`'s `ci.yml` |
| `plugin-pages-ci.yml` | `Automatic` `ci.yml`, and equal to `Both`'s `ci.yml` |
| `plugin-pages-dispatch.yml` | `Manual` `histos-pages.yml`, and equal to `Both`'s dispatch file |

Also assert:

- `renderAll` of `Automatic` is `ci.yml` only (plus release and promote when the catalog already asks for them). No `histos-pages.yml`.
- `renderAll` of `Manual` and of `Both` includes `RenderedWorkflow("histos-pages.yml", _)`.
- `Workflow.update` writes the dispatch file, and a later update with `site = None` deletes it and leaves an unrelated workflow in place. The existing orphan test is the pattern.
- A missing dispatch file is `CiError.Drift` from `Workflow.check`.
- `BuildError.DuplicateSite` and `BuildError.ScheduledSite` from graph load, in the plugin graph tests next to `DuplicateJob` (`api/test/PluginSpec.scala`).
- `CiError.DuplicateSite` and `CiError.ScheduledSite` from `Workflow.render` on a trusted graph.
- `BuildError.ReservedJob(JobName("pages"))`.
- The specular fixture still contributes `JobName("docs")` / `TaskName("site")` and `site` is `None`.

No new dependency. `./histos test` runs the suites. The default `testStore` is `Off`, so that command does not skip.

## Implementation plan

Two stacked PRs close the issue. The second branch starts from the first. Do not tag a release on the first alone: until the second merges, `Some(path)` means the automatic deploy, and the second PR's default `Both` adds `histos-pages.yml` for every site. Specular does not set `site`, so neither PR changes that plugin until it opts in. The README workflow section is updated in the second PR, in the same change as the generator it describes.

1. **Split the Pages deploy off the build job.** Keep `site: Option[RepoPath]`. `Some` means build on every trigger, upload and deploy only on a push to `PushBranch`, deploy job `pages` with the environment. `None` is unchanged. Replace the site `contains` test with `plugin-pages-ci.yml`. Introduce `PushBranch` and use it in the header, the deploy guard, and the cache namespace expressions. No new boolean parameter.

2. **Add `SitePublish`, the dispatch workflow, and the refusals.** Change `site` to `Option[Site]` with default `Both`. Emit `histos-pages.yml` for `Manual` and `Both`. `Automatic` keeps the `ci.yml` from step 1. Add `DuplicateSite`, `ScheduledSite`, and the `pages` reserved name. Goldens as in the test plan. Update the specular fixture assertion to record `site = None`. Update the README.

Follow-ups, each its own PR after the feature. They are not required to close #90. The ledger names them so the hits are not dropped.

3. **Replace generator boolean flags with ADTs.**
4. **Make the workflow helpers total.**
5. **Reserve the generated `doc` job id.**
6. **Grant artifact scope on the release upload.**

## Audit ledger

Audited while reading the CI model and the generator: `CiJob`, `JobWhen`, `JobName`, graph job checks, `Workflow`, `CiError`, and the workflow tests those types feed. Hits against the totality list and the review bar are below, including ones this feature does not fix.

| Done | Category | Location | Owning step |
| --- | --- | --- | --- |
| [ ] | booleanly | `CiJob.site: Option[RepoPath]` is both the directory and the decision to deploy on every event. [Contribution.scala](../api/src/histos/plugin/Contribution.scala) lines 51-57. | 2. SitePublish |
| [ ] | booleanly | `steps` takes `gate: Boolean` to mean "emit a matrix `if`." [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 602-607. | 3. Boolean flags |
| [ ] | booleanly | `nodeStep` takes `gate: Boolean`. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 660. | 3. Boolean flags |
| [ ] | booleanly | `llvmStep` takes `gate: Boolean`. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 670. | 3. Boolean flags |
| [ ] | booleanly | `playwrightStep` takes `gate: Boolean`. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 697. | 3. Boolean flags |
| [ ] | booleanly | `clientStep` takes `present: Boolean` and `gate: Boolean`. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 756. | 3. Boolean flags |
| [ ] | booleanly | `include` takes `flags: Boolean` to mean "emit the tool columns." [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 762. | 3. Boolean flags |
| [ ] | booleanly | `releaseJob` takes `install: Boolean` to mean "append the install uploads." [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 277. | 3. Boolean flags |
| [ ] | stringly | The push branch is the literal `main` in the workflow header and in both cache namespace expressions. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 573-584, 849-852, and 884-885. | 1. Split the deploy job |
| [ ] | `Either[String, _]` | `PluginMacros.textOf` flattens a typed `from` error to `String` for the compiler abort, including `JobName`. [Macros.scala](../api/src/histos/plugin/Macros.scala) lines 142-143. `JobName.from` stays `Either[JobNameError, JobName]`. Scalac takes a string, so this row is an accepted exception unless the owner wants a different report path. | accepted, no code |
| [ ] | illegal state | `docJob` emits a job id `doc` that `JobName.reserved` does not contain, so a plugin can take the generated id. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 903-916 and [Names.scala](../api/src/histos/plugin/Names.scala) lines 109-114. | 5. Reserve `doc` |
| [ ] | partial | `oneModule` maps an empty group to `ModuleRef.All`. An empty group is not every module. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 220-223. | 4. Total helpers |
| [ ] | partial | `WorkflowSpec.project` discards `Left` and recurses on `"core"`. A bad name silently becomes `core`, and a failure of `"core"` does not terminate. [WorkflowSpec.scala](../engine/test/WorkflowSpec.scala) lines 404-407. | 4. Total helpers |
| [ ] | partial | The specular fixture source handles `ProjectName.from("docs")` with `case Left(_) => Contribution(Nil, Nil, Nil)`, dropping the typed error. [SpecularFixtureSpec.scala](../engine/test/SpecularFixtureSpec.scala) lines 59-75. | 4. Total helpers |
| [ ] | weak test | The site test asserts a conjunction of `contains` calls, so a failure does not say which clause failed. [WorkflowSpec.scala](../engine/test/WorkflowSpec.scala) lines 435-466. | 1. Split the deploy job |
| [ ] | weak test | The contents-write and scheduled-permission tests use the same conjunction. [WorkflowSpec.scala](../engine/test/WorkflowSpec.scala) lines 589-643. | 4. Total helpers |
| [ ] | `Task` | `WorkflowSpec.text` is `ZIO[Any, Throwable, String]`. The suite's other effects use typed errors. [WorkflowSpec.scala](../engine/test/WorkflowSpec.scala) line 409. | 4. Total helpers |
| [ ] | permissions | The release and client jobs upload with `actions/upload-artifact@v7` under `contents: read` only. A permissions block sets every omitted scope to `none`, and that upload 403s on `FinalizeArtifact` without `actions: write`. [Workflow.scala](../engine/src/histos/workflow/Workflow.scala) lines 277-300 and 333-358. | 6. Release upload scope |

Eighteen hits. Eight are `booleanly`, three are `partial`, two are `weak test`, and `stringly`, `Either[String, _]`, `illegal state`, `Task`, and `permissions` are one each.

`fileNameOf` uses `getOrElse("")` ([Workflow.scala](../engine/src/histos/workflow/Workflow.scala) line 971). That is the total form, and a listed directory entry has a file name, so it is not a row. The `AtomicMoveNotSupportedException` handler matches the exception type, not its message.

## Open questions

None that block the two PRs. The schedule refusal, the single `Site`, and the lack of a branch `if` on the dispatch workflow follow the owner's semantics and from one `github-pages` artifact. They are decisions in the sections above.

The upload scope starts as `actions: write`. `artifact-metadata: write` is added on that job if a run 403s naming it. That is a golden change in the PR that sees the 403, not a second design.
