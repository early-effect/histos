package histos.docs

import specular.*

/** Generated workflows, and how this site publishes. */
object Workflows extends DocSpec:

  def doc = page("CI")(
    md"""
Histos writes the GitHub Actions workflows from the build, instead of asking you to keep
a hand-written YAML file in step with the modules. When you add a module, a test, or a
plugin job, you regenerate the workflows and commit them. CI then runs those files. It
does not run the generator again.

`histos workflow --check` fails when the files in the tree differ from what this build
would write. `histos workflow --update` writes them. Neither command publishes artifacts,
and neither changes the Histos version pinned in `build.properties`.
""",
    cite[histos.cli.Help.type](_.render),
    section("Update Histos, then regenerate")(
      md"""
When a project moves to a newer Histos, regenerate its workflows with that Histos and
commit them. The order is update, then generate:

```sh
histos update
histos workflow --update
```

`histos update` installs the newer toolchain. It rewrites `histos.version` when the pin
moves. A release pin moves to the newest release. A snapshot pin follows its line unless
you pass `--freeze`. `histos workflow --update` then writes the workflow files that
toolchain would write. The setup action in those files names the commit of the Histos
that wrote them. Commit the files and push.

Do this for a release and for a snapshot. A line pointer can keep the same text in
`build.properties` while the installed jar changes. Regenerate anyway. CI runs
`histos workflow --check` against the Histos the pin resolves to. Files written by an
older Histos fail that check.

Changing Histos itself is the extra case. Publish that snapshot before `histos update`,
so the update installs the jar that contains the workflow change. Every other project
starts at `histos update`.
"""
    ),
    section("Which Histos a job runs")(
      md"""
A generated job installs the Histos named by `histos.version` in `build.properties`.
The setup action reads that pin and runs `toolchain install` with no `--client` flag,
so the job stays on `java -jar`. CI does not pack this checkout and hand that jar to
the next job. There is no generated `publish` job, and Histos does not generate a
`client` job.

A catalog with a ship also gets a `compatibility` job. It runs `histos publish check`.
`histos publish local` stays a command you run on a machine. It writes the local Ivy
repository and does not upload, so a generated workflow cannot install its result.
""",
      cite[histos.workflow.CiInstall.type](_.ActionPath),
      cite[histos.workflow.Workflow.type](_.requiredJobs),
    ),
    section("A plugin can add a job")(
      md"""
The generated file is not only compile and test. A plugin can contribute a job. Histos
knows two shapes. A task job runs one Histos task. A site job publishes one directory,
which is how a documentation site reaches GitHub Pages.

A site job has a publish mode. Automatic deploys when you push the default branch.
Manual writes a workflow you run by hand from the Actions tab. Both does the two. The
default on a Histos site, when a plugin does not choose, is both.
""",
      cite[histos.plugin.SiteJob](_.render),
      cite[histos.plugin.Site](_.dir),
    ),
    section("How this site is published")(
      md"""
This site asks for automatic publish only. A push to the default branch builds the site
and deploys it. A pull request does not run that job, so a review does not overwrite the
published pages. The first time it deploys, GitHub Pages has to be set to deploy from
GitHub Actions.
""",
      cite[histos.plugin.SitePublish].signature,
    ),
    section("A library ship publishes snapshots")(
      md"""
A catalog with a ship gets `histos-snapshots.yml`. You run it from the Actions tab.
It publishes the snapshot coordinate of that commit. A library does not have to
package an extra file to get this workflow. A build with no ship does not get the
file, and `histos workflow --update` deletes a stale copy.
""",
      cite[histos.version.Ship](_.release),
    ),
    section("OpenSSL on the native job")(
      md"""
A native module that links `Ssl` or `Crypto` needs the headers and the shared
libraries on the runner. The native test job installs clang, the C++ library,
libgc, libunwind, and libssl. A job that does not link those libraries skips
the install.
""",
      cite[histos.NativeLibrary](_.token),
    ),
    section("Browsers the suite installs")(
      md"""
A test job installs Playwright when the build names a Playwright revision. The
browsers it installs are the ones on `RunnerTools`. Firefox is the default. Name
Chromium or WebKit and those are installed too, including the system libraries they
need. A job that does not run a browser installs none.
""",
      cite[histos.Browser](_.token),
    ),
  )
end Workflows
