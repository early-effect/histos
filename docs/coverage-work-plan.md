# Coverage

No version bump and no release.

## Why

`histos test` says whether the suites passed. It does not say which statements they reached. That check is local, and a build can also opt into a CI job that runs the same command.

sbt-scoverage is the feature list: statement coverage, branch coverage, file and class exclusions, six minimums, and one aggregated report in text, XML, and HTML. It is not the design. Histos does not keep a sticky compiler mode. `test`, package, and publish never read instrumented classes.

## Decisions

- One command, `histos coverage [project ...]`. It instruments, runs the tests, aggregates, and writes the report. `histos test` stays the uninstrumented run.
- The first cut measures Scala 3 and Scala 2.13. Consumers compile 2.13, so a Scala 3-only check is not the feature. JS and Native are named in the report and not instrumented.
- Scala 3 uses the compiler's `-coverage-out`. Scala 2.13 uses the scoverage compiler plugin and its runtime on the test classpath. `Invoker` for Scala 3 is already in the standard library.
- The command writes three views of one report: the terminal text, `scoverage.xml`, and an HTML page, all under `.histos/coverage`. The XML is the scoverage document Codecov, Coveralls, and Sonar already read. The HTML is ours, built from the same rates.
- A missed floor fails the command only when a floor is set. The flags are `--min-stmt`, `--min-branch`, `--min-stmt-file`, `--min-branch-file`, `--min-stmt-package`, and `--min-branch-package`, each an integer from 0 to 100.
- The generated workflow grows a coverage job only when the build opts in. The job runs `./histos coverage`. It fails only when that opt-in also names a floor. No opt-in means no job.
- Exclusions are `--exclude-file` and `--exclude-class`, repeatable. The same regexes live on the build as `.coverageExcludes`. That setting does not turn the CI job on. `histos coverage` uses the build list, then adds regexes from the command. A floor flag replaces that one floor from `.coverage()`, and an omitted floor uses the build. When the job is on, the rendered run carries the build floors and the build excludes. A bad regex is a typed error at the command. Each series receives the exclusion form its compiler accepts.

## Dependencies

No library for XML or HTML. The inventory file is line-oriented text (format 3.0), not XML. Histos parses it, then writes the three views from that value. Escaping `&`, `<`, and `>` is a function. scala-xml, the scoverage reporter, and an HTML template library do not earn a place.

Two artifacts, and only for a Scala 2.13 coverage compile:

- `scalac-scoverage-plugin_2.13`, on that compile's plugin path.
- `scalac-scoverage-runtime_2.13`, on that cell's test classpath, because Scala 2 has no `scala.runtime.coverage.Invoker`.

Scala 3 needs neither. The plugin and the runtime are fetched for that compile. They are not a lock row, they are not dependencies of a published histos module, and the runtime is not placed on a Scala 3 classpath. The revision comes from the compiler catalog. Scala 2 arguments are the plugin jar, the data directory, the source root, the exclusions, and `-Yrangepos`.

## Cache

A coverage compile is a different product. `-coverage-out` and the Scala 2 data directory are baked into the class file. `CompileKey` does not see extra scalac arguments, so the coverage identity has to be its own row.

- Classes and analysis live under `.histos/coverage/classes/<module>/<scala>/<platform>` and `.histos/coverage/analysis/...`. The data directory is the classes directory, so `scoverage.coverage` is stored and restored with the class files.
- `CompileInput.Coverage` is present only on a coverage compile. A normal compile's key does not grow a row. The row value is the series, the data directory, the source root, the exclusion regexes, and the plugin revision. Scala 3 records an empty revision.
- Measurement files are deleted before the run. They are written by the test process after the snapshot.
- Statement ids belong to one compilation. An aggregate sums hits and totals. It does not union ids.
- An ignored statement is in neither rate. A floor holds when `hit * 100 >= minimum * total`, in longs. An empty inventory meets only a minimum of 0.
- The command runs in process. It is not a new wire `Call`.

## Checklist

Top to bottom. One item, then its proof. `./histos test` on the modules that item names. Do not check a box the proof has not earned.

1. [x] Report laws. Parse format 3.0, reject a bad version, drop ignored statements, count a branch only as a branch, ignore an id that is not in the inventory, and apply the six floors, including an empty inventory. The text, the XML, and the HTML are three renderings of that one value. Proof: `./histos test engine`.
2. [x] Compile identity. A coverage row changes the key. A normal request does not write the row. Scala 3 argv is `-coverage-out` plus the exclusion flags that series accepts. Scala 2.13 argv is the plugin and its data directory, and the test classpath gains the runtime jar. Proof: `./histos test engine`.
3. [x] The command. Grammar, help, and an in-process run. Scala 3 JVM and Scala 2.13 cells compile into `.histos/coverage` and the tests run against those classes. Other cells are named and not instrumented. Stale measurements are deleted first. Proof: `./histos test app engine`, then `./histos coverage api` prints a statement total greater than zero and leaves `.histos/classes` untouched. The 2.13 proof is one instrumented cell in the engine suite, because this repository's own modules are Scala 3.
4. [x] Opt-in CI. A build that does not ask has no coverage job. A build that asks has a job that runs `./histos coverage`. A floor on that ask is a flag on the job. No floor means the job does not fail on a rate. Proof: `./histos test engine` on the rendered workflow.
5. [x] README. Name the command next to `test`. Say publish does not read `.histos/coverage`. Say the CI job and the floors are opt-in.

## Out of this plan

Coveralls or Codecov upload, and JS or Native instrumentation. The XML is the hand-off. Histos does not upload it.
