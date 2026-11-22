package histos.docs

import specular.*

/** Generated workflows, and how this site publishes. */
object Workflows extends DocSpec:

  def doc = page("CI")(
    md"""
`histos workflow --check` compares the generated workflows with the tree. `histos workflow
--update` writes them. Neither publishes and neither changes the pin.
""",
    cite[histos.cli.Help.type](_.render),
    section("What a plugin job is")(
      md"""
A plugin contributes jobs. Histos ships `TaskJob` and `SiteJob`. A `SiteJob` publishes one
directory. `SitePublish.Automatic` deploys on a push to the default branch.
`SitePublish.Manual` writes a dispatch workflow. `SitePublish.Both` does both, and it is the
default on `Site`.
""",
      cite[histos.plugin.SiteJob](_.render),
      cite[histos.plugin.Site](_.dir),
    ),
    section("This site")(
      md"""
This documentation site asks Specular for `SiteCi.Publish(SitePublish.Automatic)`. That is a
deploy on a push to the default branch. A pull request does not run that job. Specular turns
that decision into one Histos `SiteJob`.
""",
      cite[histos.plugin.SitePublish].signature,
    ),
  )
end Workflows
