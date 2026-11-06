# doctor

`histos doctor` runs registered checks and prints how to fix each finding.

A check has an id, an area, the probes it needs, and a run function. Areas are toolchain, build definition, workflows, repository settings, supply chain, site publish, release, and a plugin id. Probes are local files, GitHub read, GitHub admin, an App token, and the job environment. A missing probe is a skip, with the command that would provide it. The supply answers for the secret, the App installation, the App permissions, and the required check do not skip: when nothing can answer them, each one is an error. A check that does not finish within its timeout fails the run, and the text names that check id. Warnings do not.

`Diagnostic` carries a check id, a level, a title, a summary, an explanation, and a non-empty list of fixes. A fix is a settings URL, a shell command, a local edit, or a list of steps. There is no fix that sets, reads, or deletes a secret.

Supply chain is the first contributor. Its findings cover the histos-deps App, secret names, permissions, the token step, the required check, signed commits, Dependabot overlap, the dependency graph, the default branch, workflow permissions, SHA pin policy, and a disabled schedule. A 401, 403, 404, or 422 on workflow permissions or the repository document, including `Resource not accessible by integration`, is `supply.github-refused`. The same status on the installation, the secret lists, or the branch rules is not that refusal. It fails the check closed: this token cannot see the fact, or GitHub returned a document histos does not understand. A 404 from classic branch protection means classic protection is off. It does not mean the required contexts are empty.

Doctor reads a token the way `gh` does, and it does not require the `gh` binary. `github.com`, `*.ghe.com`, and `localhost` use `GH_TOKEN`, then `GITHUB_TOKEN`. Any other host uses `GH_ENTERPRISE_TOKEN`, then `GITHUB_ENTERPRISE_TOKEN`. The host is `GH_HOST`, or `github.com` when that is unset. When those variables are empty and `gh` is on `PATH`, doctor runs `gh auth token` (with `--hostname` when `GH_HOST` is set). A missing binary is no token. Doctor does not read `hosts.yml` or a keyring. With `GH_HOST` unset, the host stays `github.com` even when `gh auth token` would have answered a single enterprise host in the gh config. The token never appears in a finding. The API root is `https://api.github.com` for `github.com`, and `https://{host}/api/v3` otherwise.

A secret the workflow can read counts. In a generated job, `HISTOS_SECRET_PRESENT_<NAME>` is `secrets.NAME != ''`, which sees an organization secret shared with the repository, and doctor does not call the secret lists. On a laptop, doctor reads repository secrets and organization secrets. Either list is enough. A 401 or 403 is "cannot see whether this secret is set", not "missing". A 404 on organization secrets means that repository has none. The fix for an organization opens the organization Actions secrets and says a repository secret also counts.

`GET /repos/{owner}/{repo}/installation` needs the App JWT. A user token and the Actions token are not that JWT, so a 401 is not a contents refusal and it does not drop `supply.app-installed` or `supply.app-permissions`. `HISTOS_STEP_APP=success` means `actions/create-github-app-token` already minted an installation token with contents, pull requests, and workflows write. That answers both checks, and doctor does not send that token back to the installation endpoint. `admin` satisfies a write permission. When every needed secret is known to be missing, doctor reports the missing secret and does not also claim the App is missing, unless the installation call was a real 404 or 200.

The required check is the rule GitHub enforces on the repository's default branch. Doctor reads classic protection and `GET /repos/{owner}/{repo}/rules/branches/{branch}`. A repository ruleset, an organization ruleset, or classic protection can require the check. The finding names that rule. The branch-rules document carries the ruleset id, not its name, and the fix opens that rule: `/{owner}/{repo}/rules/{id}`, the organization rules settings, classic branch settings, or the repository rules page when no rule was seen. It does not open `https://github.com/settings/rules`. A 401 or 403 from the rules endpoint is "cannot see the rule", including when classic protection answered. Signing is reported only when the rules were read. A ruleset `required_signatures` rule counts, and so does classic `required_signatures` when that document was read.

Release checks read the catalog and `releases`. They do not open a bump pull request.

- `ship.session` is an error when `HISTOS_SESSION` is `snapshot`, `pr`, or `release` and a required variable is missing. The fix is `export HISTOS_SESSION=snapshot HISTOS_SHA=$(git rev-parse HEAD) HISTOS_BRANCH=default`, then `histos run versions`. Development compiles `<ship>-ci`. `publish snapshots` and `publish release` in that session fail with the same next step.
- `ship.releases` is an error when a line catalog has no readable `releases` file. Create `releases`. An empty file is fine. A line is `<ship> matches`, `<ship> moved`, or `<ship> unknown`.
- `ship.should-bump` is a warning when a catalog number is already `matches` or `moved`. Edit `Ship("name").next(...)`. Do not open a bump pull request.

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

The same report is the `doctor` MCP tool. `--json` prints one object with `exit` and `findings`. In GitHub Actions the text renderer also has an annotation line and a step-summary table.
