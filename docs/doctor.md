# doctor

`histos doctor` runs registered checks and prints how to fix each finding.

A check has an id, an area, the probes it needs, and a run function. Areas are toolchain, build definition, workflows, repository settings, supply chain, site publish, release, and a plugin id. Probes are local files, GitHub read, GitHub admin, an App token, the job environment, and the registry. A missing probe is a skip, with the command that would provide it. A skipped check prints `-  skipped <id>  (needs <Probe>)` and the first fix line. A skip leaves the exit code alone and never counts as a pass.

The venue decides what a supply check does when the token could not see its fact. The venue is `Actions` only when `GITHUB_ACTIONS` is the string `true`, and `Laptop` otherwise. For an author with a credential (`PrAuthor.App`, `PrAuthor.OwnerOpens`, `PrAuthor.Pat`), an answer the token could not give is an error in Actions and a skipped check on a laptop. Those answers are secret presence, the App installation, the App permissions, and the required check. A fact the token did see is an error on every venue: the App is not installed, a permission is missing, a secret is missing, the required check is missing, or the token step failed. `PrAuthor.Actions` does not ask those questions. It warns `supply.workflow-files`. A check that does not finish within its timeout fails the run, and the text names that check id. Warnings do not.

`Diagnostic` carries a check id, a level, a title, a summary, an explanation, and a non-empty list of fixes. A fix is a settings URL, a shell command, a local edit, or a list of steps. There is no fix that sets, reads, or deletes a secret.

Supply chain is the first contributor. Its findings cover the histos-deps App, secret names, permissions, the token step, the required check, signed commits, Dependabot overlap, the dependency graph, the default branch, workflow permissions, SHA pin policy, and a disabled schedule. A 401, 403, 404, or 422 on the repository document, including `Resource not accessible by integration`, is `supply.github-refused`. A 404 or 422 on workflow permissions is that refusal too. A 401 or 403 on workflow permissions is not: `supply.default-token` and `supply.allow-prs` warn that this token cannot see the setting. That call needs repository Administration read, which the supply App token and the Actions token do not have. A 200 body that does not decode is the same warning, for a document histos does not understand. A 401, 403, 404, or 422 on the organization installations, the secret lists, or the branch rules is not `supply.github-refused`. Each of those checks reports its fact as unseen (this token cannot see it) or unreadable (GitHub returned a document histos does not understand), and the venue rule above turns that into an error or a skip. A 5xx or a failed request makes the whole GitHub read unreachable, and the same rule applies. A 404 from classic branch protection means classic protection is off. It does not mean the required contexts are empty. The app gate (`supply.secret-missing`, `supply.app-installed`, `supply.app-permissions`, `supply.app-token`, `supply.required-check`) runs only for an author that has a credential. `supply.allow-prs` follows that author. For `PrAuthor.Actions`, a document that says Actions cannot open pull requests is an error, because that is the author the repository chose. An unread document stays the Administration warning for every author. `App`, `OwnerOpens`, and `Pat` stay info when the setting is on, including when the token that read GitHub was the Actions token. `supply.workflow-files` warns for `PrAuthor.Actions` whether or not GitHub was read.

Doctor reads a token the way `gh` does, and it does not require the `gh` binary. `github.com`, `*.ghe.com`, and `localhost` use `GH_TOKEN`, then `GITHUB_TOKEN`. Any other host uses `GH_ENTERPRISE_TOKEN`, then `GITHUB_ENTERPRISE_TOKEN`. The host is `GH_HOST`, or `github.com` when that is unset. When those variables are empty and `gh` is on `PATH`, doctor runs `gh auth token` (with `--hostname` when `GH_HOST` is set). A missing binary is no token. Doctor does not read `hosts.yml` or a keyring. With `GH_HOST` unset, the host stays `github.com` even when `gh auth token` would have answered a single enterprise host in the gh config. The token never appears in a finding. The API root is `https://api.github.com` for `github.com`, and `https://{host}/api/v3` otherwise.

A secret the workflow can read counts. In a generated job, `HISTOS_SECRET_PRESENT_<NAME>` is `secrets.NAME != ''`, which sees an organization secret shared with the repository, and doctor does not call the secret lists. On a laptop, doctor reads repository secrets and organization secrets. Either list is enough. A 401 or 403 is "cannot see whether this secret is set", not "missing". A 404 on organization secrets means that repository has none. The fix for an organization opens the organization Actions secrets and says a repository secret also counts.

For a repository owned by an organization, doctor reads the App installation from `GET /orgs/{org}/installations?per_page=100` and matches the app slug `histos-deps`. That list needs a token with organization administration read (classic scope `admin:read:org`). Doctor reads the answer this way:

- `repository_selection` of `all` proves the repository and carries the App permissions. `admin` satisfies a write permission.
- `selected` names repositories a user token cannot list, so the App stays unseen.
- A complete list without the App is a real answer: `supply.app-installed` is an error on every venue. A truncated list (`total_count` larger than the page) without the App stays unseen.
- A 401, 403, or 404 is unseen.
- A body that does not decode, including an unknown `repository_selection`, is unreadable.
- A 5xx makes the GitHub read unreachable.

A user-owned repository makes no installation call, and the App stays unseen there unless the mint step answered.

`HISTOS_STEP_APP=success` means `actions/create-github-app-token` already minted an installation token with contents, pull requests, and workflows write. Doctor then makes no installation call and treats the App as installed with those three writes. When a needed secret is known to be missing and the installation read saw nothing, doctor reports the missing secret and drops the App checks (`supply.app-installed`, `supply.app-permissions`, `supply.app-token`). An installation read that answered, or whose body did not decode, keeps them.

`supply.app-token` reads the mint step outcome from `HISTOS_STEP_APP`. `success` passes. `failure`, `skipped`, and `cancelled` are the `supply.app-token` error. A missing or unrecognized value is `AppTokenUnreported`: an error in Actions and a skip (needs `JobEnv`) on a laptop. It never passes. The generated `workflow` and `supply-chain` jobs pass the variable for an author with a credential.

A blind check names why it was blind, and the fix follows that reason. With no credential, the fix says to set `GH_TOKEN` or `GITHUB_TOKEN`, or to run `gh auth login`. When the token was refused, the fix says what the token lacks. For the App checks it says to run doctor in the generated workflow job, or to give `gh` a token that can read organization installations with `gh auth refresh -h github.com -s admin:read:org`, and that an App installed on selected repositories cannot be proven by a user token. For an unreadable document the fix says to file an issue at https://github.com/early-effect/histos with the check id. For an unreachable GitHub it says to reach `api.github.com` and run doctor again. These fixes are steps, so `--fix --yes` runs none of them.

The required check is the rule GitHub enforces on the repository's default branch. Doctor reads classic protection and `GET /repos/{owner}/{repo}/rules/branches/{branch}`. A repository ruleset, an organization ruleset, or classic protection can require the check. The finding names that rule. The branch-rules document carries the ruleset id, not its name, and the fix opens that rule: `/{owner}/{repo}/rules/{id}`, the organization rules settings, classic branch settings, or the repository rules page when no rule was seen. It does not open `https://github.com/settings/rules`. A 401 or 403 from the rules endpoint is "cannot see the rule", including when classic protection answered. Signing is reported only when the rules were read. A ruleset `required_signatures` rule counts, and so does classic `required_signatures` when that document was read.

Release checks read the catalog, `releases`, `histos.version`, and the newest release Maven Central lists. They do not open a bump pull request or rewrite a pin.

- `ship.session` is an error when `HISTOS_SESSION` is `snapshot`, `pr`, or `release` and a required variable is missing. The fix completes that session, then `histos run versions`. Development's versions line is `<ship>-ci`. `publish local` and `publish snapshots` name the commit at HEAD and write the snapshot coordinate. `publish release` requires `HISTOS_SESSION=release`.
- `ship.releases` is an error when a line catalog has no readable `releases` file. Create `releases`. An empty file is fine. A line is `<ship> matches`, `<ship> moved`, or `<ship> unknown`.
- `ship.should-bump` is a warning when a catalog number is already `matches` or `moved`, or when Central lists a release of that ship at or above it. `releases` speaks first. Edit `Ship("name").next(...)` in `histos.scala`. Do not open a bump pull request.
- `ship.pin` is a warning when `histos.version` trails a published Histos release: a release pin behind a newer release, or a snapshot pin whose line is released (`0.2.1-SNAPSHOT` after `0.2.1`). The fix is `histos update` for a release pin and `histos update --stable` for a snapshot pin.
- Both registry checks are skipped, not failed, when Central does not answer.

```text
./histos doctor
./histos doctor --area supply-chain
./histos doctor --area toolchain
./histos doctor --json
./histos doctor --preflight supply-chain
./histos doctor --fix
./histos doctor --fix --yes
./histos --explain node.floor
```

`--explain <check-id>` prints the long explanation for that check and does not run it. The toolchain ids are `jdk.source`, `node.floor`, `node.shadowed`, `clang.source`, `runner-tools.cover`, and `runner-tools.agree`.

Toolchain checks report where the JDK, Node, and clang came from.

- `jdk.source` is an error when the JDK is missing or older than 21. The home is `.histos/toolchain.local` when that file exists, and otherwise `java.home`.
- `node.floor` is an error on a Js graph when Node is missing or older than 25. The message names `HISTOS_NODE`, `.histos/node.local`, and how to install Node 25. The binary is `HISTOS_NODE`, then `runnerTools.nodePath`, then `.histos/node.local`, then `PATH`.
- `node.shadowed` is a warning when a node older than 25 is first on `PATH` and a newer node sits further along. The fix is `export HISTOS_NODE=<newer path>`.
- `clang.source` is an error on a Native graph when clang is missing or the major is not the modules' `ClangMajor`. Clang comes from `PATH`.
- `runner-tools.cover` is an error when a Native module has no `runnerTools.llvm`, or a Js module has no `runnerTools.node`. The fix is the `.runnerTools(RunnerTools(...))` call to add. Without it, the generated job drops the install step.
- `runner-tools.agree` is an error when `NodeFloor` is below 25, or the LLVM revision's major is not the modules' `ClangMajor`. The fix raises that value. It does not open a pull request.

`histos toolchain` probes JDK, Node, and clang together. A failure of one does not hide the others.

`--preflight <job>` is offline. It reads the job environment and does not call GitHub. Generated jobs run it after checkout and setup when `DoctorPlacement` is `PreflightPlusWorkflowJob`. `WorkflowJobOnly` omits that step. Neither placement adds a job. The full doctor runs on the `workflow` job.

`--fix` applies local edits. `--yes` confirms those edits and histos commands. A GitHub setting is applied only after an interactive confirmation, which does not happen in CI or when there is no terminal. `--yes` never confirms a GitHub setting.

Process status is 0, 1, or 2, including doctor, deps, and setup.

The same report is the `doctor` MCP tool. `--json` prints one object with `exit`, `timedOut`, and `findings`. In GitHub Actions the text renderer also has an annotation line and a step-summary table. The JSON, the annotations, and the summary table list findings and timed-out checks only. A skipped check appears in the text output, and the last text line counts it (`N errors, N warnings, N ok, N skipped`).
