# The suite host grades the pull request

Status: deferred. Histos stays a local host until this is changed on purpose.

## What CI does

`histos.scala` calls `publishLocalSuite`, which sets `SuiteHost.Local`. The `publish` job runs `./histos publish local` with the pinned toolchain. That compiles this commit, proves the selected JVM members, writes a jar on the runner, and uploads it as the `host` artifact. Nothing is uploaded to the snapshot repository.

`test`, `format`, `scaladoc`, and `client` install that jar and set `HISTOS_PIN`. `./histos` on those jobs is this commit. `affected`, `versions`, `workflow`, `supply-chain`, `dependency-snapshot`, and `cache-seed` stay on the pin. `workflow --check` stays on the pin so a generator change and a yaml edit cannot agree with each other.

`publish snapshots` is a different workflow. `histos-snapshots.yml` runs it only after a green `ci` on `main`.

The pin is already a Linux build. `test`, `format`, and `scaladoc` install the packed jar with `--no-client` and run `java -jar`. Only the `client` job links `histos-client`, and it links this commit's kit.

## The hazard

The judge and the subject are the same commit. A runner that skips a module, a formatter that agrees with its own output, or a compiler that miscompiles the test which would have caught it, all report green. The snapshot of that commit then becomes the next pin, and the lie is the baseline.

One `./histos test` is two processes. The parent is the installed jar (`SuiteLaunch`, `suiteEnv`). The suite JVM's classpath is the class files that parent just compiled. Tests of those class files run this commit either way. The parent is whichever jar was installed. A local host makes the parent this commit.

## The chain to restore

The pin compiles this commit and runs this commit's tests. The pin reports the result. After merge, the snapshot workflow publishes, and that snapshot is the next pin. This commit's program grades the following pull request, not itself.

That is `SuiteHost.Pin`. Drop `publishLocalSuite` from `histos.scala` and regenerate with `./histos workflow --update` using the installed pin, so `workflow --check` agrees. The compatibility job then runs `./histos publish check`. Heddle and specular are already pin hosts.

The histos main ruleset requires the check named `publish`. After the switch that job is gone and the required check is `compatibility`. Update ruleset `25765081` in the same change. Do not edit the heddle or specular rulesets.

`run host` proves the installed program's `histos-client`. On a pin, the generator does not link that client. The client job as written would look for a binary the setup action never builds. Either link the pin's client, or turn the job into a test of the client module that the pin compiles. A generator change and the yaml it emits cannot land together, because `workflow --check` runs the pin.

## What not to do

Do not publish a snapshot of the pull request so CI can pin it. That puts unproved bytes on the channel other repositories install. The local pack was the way to run this commit without that publish. The pack is the step that lets this commit take the bench.
