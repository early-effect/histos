# Scala.js link products

`JsLink.link(JsLinkRequest)` and `JsProducts.link(JsLinkRequest)` use the same product cache.
Callers resolve `JsOptimize.effective(toolDefault, moduleDefault, taskOverride)` before
constructing the request. The request's `optimize` is an effective value, not an optional
override. Task overrides take precedence over module defaults, then the tool default.
Implicit and explicit requests that resolve to Full share one key.

Fast disables the Scala.js optimizer. Full enables it and passes the linked script to
the existing `Splice.full` production minifier. Module kind, entry-point initialization
and program arguments remain independent inputs.

## Identity and output ownership

`JsProducts.work(repo, cell, effective)` supplies a cell- and mode-specific work root.
Under any request's work root, scripts live in `<mode>-<key>/linked/`. Alternating modes
or changing inputs never overwrites a script returned by an earlier link. A restored
script has the same path and bytes as a freshly produced script for that key.

The key includes the Scala and Scala.js revisions, module kind, entry-point policy,
arguments and effective optimization mode. It also includes, in classpath order:

- Every user and Scala.js library classpath entry's normalized path, canonical path and
  content. Jar bytes are hashed, including external dependency IR. Directory files are
  hashed in deterministic relative-path order. Replacing a jar's bytes at the same path
  invalidates the link even when every own class is unchanged.
- The complete running tool classpath's paths and content, not just a nominal linker
  revision. The child uses the Scala 3 linker compiled into that toolchain.
- The selected JDK runtime identity. Full also includes the native minifier version,
  platform pin and expected binary checksum.

Nested symbolic links in input directories are refused rather than silently omitted
from the inventory. Top-level classpath aliases resolve to their canonical target.
Inputs and tools are checked again before cache publication; a change during linking
fails with `JsLinkError.InputsChanged`.

## Cancellation and publication

Linking and Full minification run inside one child JVM owned by `ChildProcess`.
The child uses the ordinary Scala 3 linker and ZIO's foreign-Future adapter. There is
no additional compiler bridge, JavaScript environment or unbounded `Await`.
Interrupting only `ZIO.fromFuture` would leave linker work running. Instead, the parent
owns and closes the process group, including minifier descendants, before returning
from interruption. The IR cache is scoped inside the child as well.

All temporary script output belongs to an invocation-scoped scratch directory. Only a
successful child result with unchanged inputs can be saved and published. Failure or
interruption removes scratch output, publishes no partial script and leaves earlier
products intact. Local and remote hits restore through the existing `LinkReplay`
format, so plugins, programs and DAG link leaves do not need separate link caches.

## Proof

`engine/test/histos/js/JsProductsSpec.scala` executes actual Scala.js output under Node.
It covers same-path dependency IR jar replacement with unchanged own classes,
same-path tool classpath jar replacement, Fast/Full alternation and byte-for-byte restore,
equal effective Full requests,
failed-link publication refusal, and physical process/scratch cleanup after observing
and interrupting a real link child. It uses scoped fibers and subprocess-start
observation, not sleeps. `JsArgsSpec` retains argument-encoding and environment checks.

The integrator owns caller-side effective-mode resolution, DAG ordering and the
assembled plugin/run/test entrypoint gate. Those callers must pass completed Main
classpath products, the selected JDK and the resolved mode to the shared link leaf.
