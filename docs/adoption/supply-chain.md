# Supply chain

histos pins every generated `uses:` to a 40-character commit SHA and a tag comment, queries OSV for advisories, and can open one grouped dependency pull request a week.

## Five minutes

1. Create a GitHub App named `histos-deps`. Permissions: Contents write, Pull requests write, Workflows write, Metadata read. Webhook off. Install it on the repository.
2. Add repository secrets `HISTOS_APP_ID` and `HISTOS_APP_PRIVATE_KEY`. doctor checks that the names exist. It never reads the values.
3. In `histos.scala`, before `.docCheck`, add `.supplyChain(SupplyChain())`. The defaults are the weekly Monday 06:00 `America/Chicago` schedule, branch `histos/deps`, one pull request, one commit per bump kind, and `CommitSigning.ApiSigned`. This repository leaves the call out until the published toolchain can compile it. On a toolchain that has the method, omitting it selects that same default.
4. Run `./histos setup supply-chain --fix` and `./histos workflow --update`.
5. Run `./histos doctor` and `./histos workflow --check`.

`./histos setup supply-chain --from zipx` reads `project/ZipxVersions.scala`, the zipx workflows, and `.github/dependabot.yml`. `Lib` and `Action` rows are mapped. A `Pin(...)` row is reported as a TODO and is not dropped. The command is idempotent.

## What the workflows do

`ci.yml` has a `supply-chain` job. It runs doctor, `workflow --check`, `resolve --check`, and `deps audit`. On a non-pull-request event it also runs `deps alert`. The `dependency-snapshot` job runs on a push to the default branch and is the supply job that asks for `contents: write`.

`histos-deps.yml` is the weekly update. `workflow_dispatch` takes `dry-run`. A dry run is doctor plus update: no branch and no pull request. `deps pr` under `CommitSigning.ApiSigned` updates the branch with `createCommitOnBranch`, one commit per bump kind, then opens or updates a pull request titled `deps: weekly updates`. The body carries `<!-- histos-deps head=<oid> -->`. Human commits on the branch produce a comment and no force-push. An empty plan closes a stale pull request.

`histos-audit.yml` is rendered only when `DailyAudit` is `On`. `DailyAudit.sixCentral` is 06:00 America/Chicago. The default is `Off`. With `SecurityFixes.OutOfBand` (the default) that file also contains the `histos/security` job.

## Commands

`deps check` lists locked coordinates and changes nothing. `deps update [--yes] [--security]` applies a bump plan; `--security` picks the smallest fixed release and skips the cooldown. `deps audit [--report]` always posts one OSV `/v1/querybatch` and hydrates only new or changed advisories into `~/.cache/histos/advisories`. `deps pr [--security] [--open]` runs the pull-request flow. `deps alert` keeps one `histos-advisory` issue per advisory id. `deps snapshot --submit` builds the dependency snapshot from `dependencies.lock`.

`doctor [--area supply-chain] [--json] [--preflight <job>] [--fix] [--yes]` prints findings. `--fix` may edit local files and prints histos commands. It never changes a GitHub setting without a confirmation, and `--yes` never confirms one. It never reads or writes secrets. Exit 0 is no error. Exit 1 is an error or a timed-out check. Exit 2 is usage. A warning alone stays 0.

`workflow --check` fails when a `uses:` is unpinned, pinned without a tag comment, or different from the catalog. The message names the file and line. Fix it with `./histos workflow --update` or add the action to `SupplyChain.extraActions`.

## Policies

Set them on `SupplyChain`. The defaults are `SeverityGate.Threshold(High, Low)`, `GateScope.All`, `OutageStrategy.ByEvent` (pull requests may use a 24-hour cache; push and schedule fail closed), `IgnoreRule.ReasonAndExpiry(90 days)`, `ZeroX.MinorIsMinor`, `Majors.ListOnly`, `PreRelease.Skip`, `Cooldown.After(3 days)`, and `DoctorPlacement.PreflightPlusWorkflowJob`. `WorkflowJobOnly` drops the per-job preflight step and still runs doctor on `supply-chain`.
