# Agents

Use zio-archtect agent ethos when working on or around this project.

Standing rules for this repository. They apply to this agent and to every subagent. Read this file before editing.

## Why this exists

Histos exists because compile, package, and link are expensive, and sbt does not skip them reliably. zipx made that tolerable. It is not the goal. The goal is a fast functional and idiomatic build tool with a cache Russ can trust on his machine and in CI, where he picks local or remote and he picks how a result is pinned, and the tool does the expensive work only when the code it depends on changed. Tests of unchanged code run only when he asks for the full run. A quick test run fulfills that ask - only testing what changes impact. A result that is wrong, or a skip that hides a real change, is a failure even when the command was fast.

The proof is a bench that builds the same graph with sbt and zipx and with Histos. Histos has to be faster where a change should not rebuild the world. Cold may tie. Caching Node, browsers, and the dependency resolve is part of the want and is not the gate. Mill is not in the comparison.

Histos builds histos.

Interview Russ for product guidance - never assume you know how Russ wants to shape this tool.


## Pre-release

Histos is brand new - formative and unreleased. A breaking change is expected when the new signature is the honest API. It will change as we discover opportunities and challenges along the way.

## How the code is written

- Use the "zio-architect" agent ethos always.
- The service pattern is important! Make sure you apply ZLayers best practices for providing capabilities to histos - and example is artifact publishers (like sona/local etc).
- Scala 3 syntax, braceless, ZIO, ZIO Test. Typed error enums/traits. Opaque values. Exhaustive matches. Effects are values.
- Format code before each commit with `./histos fmt`, or the histos MCP `fmt` tool. `fmt --check` is what CI runs.
- If you need a library reach for early-effect github org libraries first. For example heddle for http. If you find a feature gap or bug in a library - pause and discuss it - we will likely file a ticket for enhancement or fix and see if waiting for a release of that library is warranted.
- Plugins are services provided by ZLayers.

## ZIO Test

- Use ZIO Test the way the ZIO docs the way it's docs describe it. Make effective use of the test environment.
- `Warning: A test is using time, but is not advancing the test clock` is a bug.  Fix it.


## Bump a ship

`histos bump` rewrites `Ship("name").next(...)`.

## A change that the workflows run

CI runs the committed workflows, not the generator. A pull request that changes the generator, the setup action, or anything else those workflows run ships the regenerated files in the same pull request. `histos.version` is already a snapshot, so the sequence is one:

1. Commit the change.
2. `./histos publish snapshots`. The moving snapshot pointer resolves to the jar packed from that commit.
3. `./histos update`. The pin stays the snapshot pointer and the installed jar is the one just published.
4. `./histos workflow --update`. That jar rewrites the workflows, and the setup action pin is the commit the jar was packed from.
5. Commit the regenerated workflows and push them onto the same pull request.

Leaving the regeneration for a later pin move means the pull request's own CI runs the old workflows. 
