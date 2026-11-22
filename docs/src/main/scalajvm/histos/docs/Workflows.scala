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
  )
end Workflows
