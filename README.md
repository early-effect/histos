# Histos

Histos is a Scala build tool. The build is data. The work is a ZIO effect. A person and an agent ask for the same operation and read the same result.

The terminal prints that result and chooses an exit. An agent receives the result as data and sees each line as it is printed. An editor uses BSP. If the work ran, the result is a report, including a compile or a test that failed. If the request never started, the result is a fault.

This is pre-release. The API may break when that makes it more honest. Other repositories take snapshots.

What we still intend is [next](docs/next.md).

## A build

A build file is a Scala program that returns a graph. A module names its platforms (`jvm`, `js`, `native`), its Scala versions, and its dependencies. A literal that is not a version, a dependency, or a name does not compile. This repository is built by [histos.scala](histos.scala). The literals are [Literals.scala](api/src/histos/Literals.scala).

## This repository

Histos builds itself. [histos](histos) and [histos.bat](histos.bat) come from `workflow --update`. Each script finds a JVM and starts one jar.

`HISTOS_ASSEMBLY`, when set, is that jar. Otherwise `dist/histos.jar` in the checkout is that jar. `.histos/assembly.sha256` has to match the file. A checkout without either jar downloads the snapshot in [.histos/launcher.lock](.histos/launcher.lock) and checks its sha256. `dist/` stays uncommitted. CI runs the lock.

[histos.scala](histos.scala) compiles against `bootstrap/histos-api.jar` inside the running jar. `.histos/classes/api` is the api compiled for the modules after that.

`assembly` compiles with the running jar, writes `dist/histos.jar`, and puts the api it just compiled at `bootstrap/histos-api.jar`. It records `.histos/assembly.sha256`. The next `./histos` in this checkout runs that jar.

When the build file calls api the committed pin does not contain, start from a jar that can still compile the build file, then:

```
./histos assembly
./histos publish snapshots
./histos launcher --update
```

`publish snapshots` uploads the manual ship `histos-api_3` and the assembly `rocks.earlyeffect:histos` to Sonatype Central snapshots. Both stay `0.0.1-SNAPSHOT`. The command reads `SONATYPE_USERNAME` and `SONATYPE_PASSWORD`. `launcher --update` writes that assembly's revision and checksum into the lock. Commit the lock. The next run, including CI, compiles the build file against the published api.

## An operation

You ask for one thing: compile, test, run, publish, assembly, clean, resolve, or a contributed plugin command. `testOnly` and `testQuick` are that same test operation. `preview` of compile, test, publish, clean, cleanFull, or run writes nothing. Compile and test name the sources the last analysis says are stale, and a test preview names the suites a quick run would skip. When that analysis or digest cannot be read, it says it does not know. `observe` reads the last tally and does not compile. `cancel` names the run id the command minted. `server` keeps a warm process. `mcp` and `bsp` open a transport. They are not operations.

The tally is source files, class files written this round, and modules.

## Tools already on the machine

Histos does not install a JDK or Node. The running JVM must be JDK 21 or newer. A JavaScript module also needs Node 25 or newer on `PATH`. A newer install meets the floor. `.histos/toolchain.local` and `.histos/node.local` name a home you already chose. A file that points at a missing or too-old home fails. Histos does not pick a different one.

`resolve --update` solves dependencies and writes `.histos/dependencies.lock`. Later runs fetch what the lock recorded and check the checksums. `launcher --update` is the only command that moves [.histos/launcher.lock](.histos/launcher.lock).

## Plugins

A plugin is a Scala 3 library with one [HistosPlugin](api/src/histos/plugin/HistosPlugin.scala). The build file names it in the header and registers it with `use`. It compiles against `histos.api`. It does not see the engine. [PluginSpec](api/test/PluginSpec.scala) locks that contract.

## License

Apache License 2.0. See [LICENSE](LICENSE).
