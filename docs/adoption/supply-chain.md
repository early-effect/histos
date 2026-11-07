# Supply chain

histos pins every generated `uses:` to a 40-character commit SHA and a tag comment, queries OSV for advisories, and can open one grouped dependency pull request a week.

## Five minutes

Omitting `.supplyChain` selects `PrAuthor.Actions`. The Actions bot opens the weekly pull request. No app and no secrets. Doctor does not fail the job for a missing app. It warns `supply.workflow-files`: a person runs `./histos workflow --update` and commits `.github/workflows`. The weekly pull request commits the catalog and the lock, and its body names that command.

1. Run `./histos setup supply-chain --fix` and `./histos workflow --update`. Commit the workflow files.
2. Run `./histos doctor` and `./histos workflow --check`.
3. On the repository, turn on "Allow GitHub Actions to create and approve pull requests". When doctor can read that document and the setting is off, `supply.allow-prs` is an error. The Actions token cannot read it (that call needs Administration), so CI keeps the warning.

An organization that wants the bot to commit workflow files names an author with a credential. `PrAuthor.App`, `PrAuthor.OwnerOpens`, and `PrAuthor.Pat` keep the fail-closed gate: a missing secret, an app that is not installed, permissions that were not seen, a failed token step, or a missing required check is an error. A skip is not a pass.

```scala
.supplyChain(SupplyChain(updates = UpdatePolicy(author = PrAuthor.histosDeps)))
```

`PrAuthor.histosDeps` is `App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY"))`. Create a GitHub App. Permissions: Contents write, Pull requests write, Workflows write, Metadata read. Webhook off. Install it on the repository. Add the two secrets where the workflow can read them. A repository secret counts. An organization secret shared with the repository counts. doctor checks that the names exist. It never reads the values. One app belongs to one owner. Do not share a private key across organizations.

`./histos setup supply-chain --from zipx` reads `project/ZipxVersions.scala`, the zipx workflows, and `.github/dependabot.yml`. `Lib` and `Action` rows are mapped. A `Pin(...)` row is reported as a TODO and is not dropped. The command is idempotent.

## What the workflows do

`ci.yml` has a `workflow` job. It runs on a pull request, runs the full doctor, and runs `workflow --check`. `ci.yml` also has a `supply-chain` job. It runs doctor, `workflow --check`, `resolve --check`, and `deps audit`. On a non-pull-request event it also runs `deps alert`. The `dependency-snapshot` job runs on a push to the default branch and is the supply job that asks for `contents: write`.

`histos-deps.yml` is the weekly update. `workflow_dispatch` takes `dry-run`. A dry run is doctor plus update: no branch and no pull request. `deps pr` under `CommitSigning.ApiSigned` updates the branch with `createCommitOnBranch`, one commit per bump kind, then opens or updates a pull request titled `deps: weekly updates`. The body carries `<!-- histos-deps head=<oid> -->`. For `PrAuthor.Actions` the body also says to run `./histos workflow --update` and push, and the commit set drops every file under `.github/workflows/`. The workflow grants `contents: write` and `pull-requests: write` to the job token and does not grant `workflows`. An App, an owner, or a PAT mints a token and can commit the workflow files. Human commits on the branch produce a comment and no force-push. An empty plan closes a stale pull request.

`histos-audit.yml` is rendered only when `DailyAudit` is `On`. `DailyAudit.sixCentral` is 06:00 America/Chicago. The default is `Off`. With `SecurityFixes.OutOfBand` (the default) that file also contains the `histos/security` job.

## Commands

`deps check` lists locked coordinates and changes nothing. `deps update [--yes] [--security]` applies a bump plan; `--security` picks the smallest fixed release and skips the cooldown. `deps audit [--report]` always posts one OSV `/v1/querybatch` and hydrates only new or changed advisories into `~/.cache/histos/advisories`. `deps pr [--security] [--open]` runs the pull-request flow. `deps alert` keeps one `histos-advisory` issue per advisory id. `deps snapshot --submit` builds the dependency snapshot from `dependencies.lock`.

`doctor [--area supply-chain] [--json] [--preflight <job>] [--fix] [--yes]` prints findings. `--fix` may edit local files and prints histos commands. It never changes a GitHub setting without a confirmation, and `--yes` never confirms one. It never reads or writes secrets. Exit 0 is no error. Exit 1 is an error or a timed-out check. Exit 2 is usage. A warning alone stays 0.

The app gate runs only for an author that has a credential. The pull request then fails when a needed secret is missing or unreadable, when the App is not installed or its contents, pull requests, and workflows writes were not seen, or when no rule on the default branch requires the supply check. No credential is an error on those checks, not a skip. `PrAuthor.Actions` does not ask them. Doctor reads `GH_TOKEN` or `GITHUB_TOKEN` (and `GH_ENTERPRISE_TOKEN` or `GITHUB_ENTERPRISE_TOKEN` for a host other than `github.com`, `*.ghe.com`, or `localhost`). When `gh` is installed it also runs `gh auth token`. It does not require `gh`. A 401 from the installation endpoint means this token is not the App JWT. For an author with a credential, the generated `workflow` and `supply-chain` jobs mint that token and pass `HISTOS_SECRET_PRESENT_*`, `HISTOS_STEP_APP`, and `GH_TOKEN`. For `PrAuthor.Actions` those jobs do not mint, and doctor runs with `GH_TOKEN: ${{ github.token }}`. The required-check fix opens the rule GitHub enforces: a repository ruleset, an organization ruleset, or classic branch protection. `--preflight` stays offline and does not call GitHub.

`workflow --check` fails when a `uses:` is unpinned, pinned without a tag comment, or different from the catalog. The message names the file and line. Fix it with `./histos workflow --update` or add the action to `SupplyChain.extraActions`.

## Policies

Set them on `SupplyChain`. The defaults are `SeverityGate.Threshold(High, Low)`, `GateScope.All`, `OutageStrategy.ByEvent` (pull requests may use a 24-hour cache; push and schedule fail closed), `IgnoreRule.ReasonAndExpiry(90 days)`, `ZeroX.MinorIsMinor`, `Majors.ListOnly`, `PreRelease.Skip`, `Cooldown.After(3 days)`, and `DoctorPlacement.PreflightPlusWorkflowJob`. `WorkflowJobOnly` drops the per-job preflight step and still runs doctor on `workflow`.
