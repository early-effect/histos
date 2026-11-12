package ci

import zio.test.*

object LinkClientLawSpec extends ZIOSpecDefault:
  def spec = suite("link client laws")(
    test("the job links when the pin has no client for this platform"):
      assertTrue(
        LinkClient.decide(LinkClient.Installed.Missing) == LinkClient.Decision.Link,
        LinkClient.decide(LinkClient.Installed.Present) == LinkClient.Decision.Skip,
        LinkClient.Decision.Skip.text == "the pin already has a client for this platform.",
        LinkClient.Decision.Link.text == "linked dist/histos-client",
        LinkClient.contribute.tasks.map(_.name.value) == List("link"),
        LinkClient.contribute.jobs.map(job => (job.name.value, job.task.value)) == List("link-client" -> "link"),
      )
  )
