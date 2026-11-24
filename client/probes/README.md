# Native run-stream probe

`histos/client/RunStreamsFixture.scala` is a standalone executable checkpoint, not a ZIO Test suite.
It calls the actual Native frontend renderer, accepts one JSON stdin line, and writes that line's bytes
unchanged to stdout. Run, compile, cached and progress diagnostics must use stderr. The explicit POSIX
exit boundary matches the Native client. This does not exercise an MCP handler.

This directory is outside the client's conventional Main (`client/src`) and Test (`client/test`) roots.
Do not register it as a test root or add test-framework dependencies to the client. `./histos test client`
should report `NoTests`, not a passing suite.

## Compile and execute explicitly

From the repository root, set `NATIVE_PROBE_CLASSPATH` to the matching checkout's compiled **Native** API
and wire product directories, separated by `:`. Do not use JVM products. The following pins match this
checkpoint's `histos.scala`; this probe does not change module dependencies or install a build tool.

```bash
: "${NATIVE_PROBE_CLASSPATH:?set matching Native API and wire product directories}"

SCALANATIVE_THREAD_STACK_SIZE=8m scala-cli --power package \
  client/src/histos/client/Main.scala \
  client/src/histos/client/StdinRelay.scala \
  client/src/histos/client/UnixPipe.scala \
  wire/src/histos/cli/Show.scala \
  client/probes/histos/client/RunStreamsFixture.scala \
  --server=false --scala 3.10.1 \
  --native --native-version 0.5.13-20261122-fa4437d-SNAPSHOT \
  --native-multithreading=true --native-clang /usr/bin/clang --native-clangpp /usr/bin/clang++ \
  --repository https://central.sonatype.com/repository/maven-snapshots/ \
  --dependency dev.zio:zio_native0.5_3:2.1.27 \
  --dependency dev.zio:zio-json_native0.5_3:1.1.1 \
  --classpath "$NATIVE_PROBE_CLASSPATH" --scalac-option -Wunused:all --scalac-option -Werror \
  --main-class histos.client.RunStreamsFixture --force -o /tmp/histos-native-streams-probe

printf '%s\n' ' {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"text":"π"}} ' | \
  /tmp/histos-native-streams-probe > /tmp/histos-native-streams.stdout 2> /tmp/histos-native-streams.stderr

python3 -c 'import json; raw=open("/tmp/histos-native-streams.stdout","rb").read(); lines=raw.splitlines(); assert len(lines)==1,lines; assert all(json.loads(line) for line in lines); assert raw == " {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"text\":\"π\"}} \n".encode()'
grep -F 'run 12345678-1234-1234-1234-123456789abc' /tmp/histos-native-streams.stderr
grep -F 'compiling' /tmp/histos-native-streams.stderr
grep -F 'skipped unchanged' /tmp/histos-native-streams.stderr
grep -F 'fixture progress' /tmp/histos-native-streams.stderr
```

A JVM-launched Scala CLI (`java -jar "$HOME/.histos/coursier.jar" launch
org.virtuslab.scala-cli:cli_3:1.14.0 -- --power package ...`) can run the same compilation when the
native Scala CLI launcher is unavailable. The byte equality assertion checks whitespace, Unicode and
newline preservation in addition to requiring every stdout line to decode as JSON. Do not call this
checkpoint a client test-suite pass or the installed Heddle initialize gate.
