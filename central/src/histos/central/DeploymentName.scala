package histos.central

import histos.Organization
import histos.version.ShipName

/** The name Central shows for one deployment. Zipx sends `organization ship version`. */
opaque type DeploymentName = String

object DeploymentName:
  given CanEqual[DeploymentName, DeploymentName] = CanEqual.derived

  def of(organization: Organization, ship: ShipName, version: String): DeploymentName =
    s"${organization.value} ${ship.value} $version"

  extension (name: DeploymentName) def value: String = name
end DeploymentName
