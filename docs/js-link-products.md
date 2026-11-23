# Scala.js link products

`JsLink.link(JsLinkRequest)` and `JsProducts.link(JsLinkRequest)` use the same product cache.
Callers resolve `JsOptimize.effective(toolDefault, moduleDefault, taskOverride)` before
constructing the request. The request's `optimize` is an effective value, not an optional
override. Task overrides take precedence over module defaults, then the tool default.
Implicit and explicit requests that resolve to Full share one key.

Fast disables the Scala.js optimizer. Full enables it and passes the linked script to
the existing `Splice.full` production minifier. Module kind, entry-point initialization
and program arguments remain independent inputs.

## Input and tool generations

The child never reads the caller's mutable input or host-tool paths. Inputs and tools
are copied into parent-owned scoped trees. Copying hashes the exact bytes written;
those same copies supply the child's IR and JVM classpaths. Canonical source paths,
ordered content identities and deterministic relative directory inventories are in
the key. Nested symbolic links are refused rather than omitted. Top-level aliases are
resolved before copying and are not used by the child.

This binding prevents an A-to-B-to-A overwrite from producing B under A's key. A final
live-input comparison separately rejects a persistent change with
`JsLinkError.InputsChanged`. The comparison is not the proof of consumed-byte identity.
The cache discriminator excludes records produced before snapshot binding.

`JsProducts.session` captures one scoped immutable tool generation. Its
`JsLinkSession.link(request)` reuses the verified identity of those private copies
within the owning build invocation. Recapture on tool/build reload. There is no
path/mtime cache of mutable tools. The one-off `JsLink.link` API captures a fresh tool
generation for each request. Each request still captures its mutable user/library
inputs, including external dependency IR jar bytes at unchanged paths.

The complete key also includes Scala and Scala.js revisions, module kind, entry-point
policy, arguments, effective optimization and the selected JDK runtime identity.
Full includes the minifier version, platform pin and expected binary checksum.
The child uses the Scala 3 linker captured from the running toolchain, without an
additional compiler bridge or JavaScript environment.

## Products and child results

`JsProducts.work(repo, cell, effective)` supplies a cell- and mode-specific work root.
Under any request's work root, scripts live in `<mode>-<key>/linked/`. Different modes
and input keys do not share a returned script path. Fresh and restored products use
the same path and bytes.

The shared `LinkReplay` owner must provide atomic same-key materialization. Key-specific
paths alone do not protect a reader from an in-place rewrite during another hit.
The integrator owns that shared seam and its overlapping-reader/publication proof;
there is no private JS replay format or second product cache.

The child reports a typed success/failure result through the repository's JSON codecs.
Minifier, filesystem, malformed-protocol and linker errors retain their ADT branches.
Process-start/death and missing transport results are process/transport failures.
Stderr is only appended to an already classified Linker diagnostic, never parsed to
choose an error category.

## Cancellation and verification

The existing `ChildProcess` scope owns the JVM process group, including native minifier
descendants. The child's IR cache and the parent's snapshot streams are scoped. Killing
the group, rather than only interrupting `ZIO.fromFuture`, stops physical linker work.
Only a complete successful child result with unchanged live inputs can be published.

The private invocation-observation protocol signals real child boundaries and waits
for a typed parent acknowledgement. The interruption test holds the child with its
real IR cache loaded. It does not enumerate global processes or poll readiness.

The parent creates an execution-temporary root beneath its scoped scratch tree and
sets the child's `java.io.tmpdir` at JVM startup through its dedicated launch environment.
This happens before any child temporary-directory allocation. Paths with spaces and
quote characters are supported. The durable Coursier/esbuild download cache is unchanged.

The internal Splice launch observer receives the real native PID and temporary `Path`
after process creation and before the adapter consumes output or closes the temporary scope. It is an invocation-owned effect,
not a mutable global hook. The late-Full test holds that boundary with a real `in.js`
and real esbuild process, then interrupts the parent. It checks JVM/native process death,
parent scratch and minifier directory removal, and absence of a published product before
returning from the owned effect. The parent removes the tree even when SIGKILL prevents
child finalizers. Ordinary `Splice.full` uses the same minifier path without an observer.

`JsProductsSpec` retains the original dependency replacement, tool replacement,
Fast/Full alternation/restore, failed publication and physical child cleanup proofs.
It adds actual-source gated ABA and persistent-change checks, typed real-child error
classification, startup-root quoting, late-Full physical cleanup and scoped-generation
warm-work measurements. `JsArgsSpec` retains its three argument/environment checks.
`OwnedMinifierSpec` proves typed observer-failure cleanup and parity with ordinary Full.
Tests run actual compiled IR, linked scripts under Node and the verified native minifier.

The integrator owns effective-mode caller wiring, completed Main stage classpaths,
selected JDKs, once-only DAG execution and the assembled plugin/run/test entrypoints.
