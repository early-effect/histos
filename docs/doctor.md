# doctor

`histos doctor` runs registered checks and prints how to fix each finding.

A check has an id, an area, the probes it needs, and a run function. Areas are toolchain, build definition, workflows, repository settings, supply chain, site publish, release, and a plugin id. Probes are local files, GitHub read, GitHub admin, an App token, and the job environment. A missing probe is a skip, with the command that would provide it. A check that does not finish within its timeout fails the run. Warnings do not.

`Diagnostic` carries a check id, a level, a title, a summary, an explanation, and a non-empty list of fixes. A fix is a settings URL, a shell command, a local edit, or a list of steps. There is no fix that sets, reads, or deletes a secret.

Supply chain is the first contributor. Its findings cover the histos-deps App, secret names, permissions, the token step, the required check, signed commits, Dependabot overlap, the dependency graph, the default branch, workflow permissions, SHA pin policy, and a disabled schedule. A 403, 404, or 422 from GitHub, including `Resource not accessible by integration`, is `supply.github-refused`.

Release checks read the catalog and `releases`. They do not open a bump pull request.

- `ship.session` is an error when `HISTOS_SESSION` is `snapshot`, `pr`, or `release` and a required variable is missing. The fix is `export HISTOS_SESSION=snapshot HISTOS_SHA=$(git rev-parse HEAD) HISTOS_BRANCH=default`, then `histos run versions`. Development compiles `<ship>-ci`. `publish snapshots` and `publish release` in that session fail with the same next step.
- `ship.releases` is an error when a line catalog has no readable `releases` file. Create `releases`. An empty file is fine. A line is `<ship> matches`, `<ship> moved`, or `<ship> unknown`.
- `ship.should-bump` is a warning when a catalog number is already `matches` or `moved`. Edit `Ship("name").next(...)`. Do not open a bump pull request.

```text
./histos doctor
./histos doctor --area supply-chain
./histos doctor --json
./histos doctor --preflight supply-chain
./histos doctor --fix
./histos doctor --fix --yes
```

`--preflight <job>` is offline. It reads the job environment and does not call GitHub. Generated jobs run it after checkout and setup when `DoctorPlacement` is `PreflightPlusWorkflowJob`. `WorkflowJobOnly` omits that step. Neither placement adds a job. The full doctor runs on the `workflow` job.

`--fix` applies local edits. `--yes` confirms those edits and histos commands. A GitHub setting is applied only after an interactive confirmation, which does not happen in CI or when there is no terminal. `--yes` never confirms a GitHub setting.

Exit codes for this command are 0, 1, and 2. The same three codes are what `deps` and `setup` use. The other histos exit codes are unchanged.

The same report is the `doctor` MCP tool. `--json` prints one object with `exit` and `findings`. In GitHub Actions the text renderer also has an annotation line and a step-summary table.
