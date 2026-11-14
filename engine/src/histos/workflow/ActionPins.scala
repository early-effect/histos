package histos.workflow

import histos.plugin.{ActionName, ActionPin}
import histos.version.CommitSha

/** Built-in action pins. A generated `uses` is one of these, or a local `./` path. */
object ActionPins:
  final case class BuiltIn(name: ActionName, pin: ActionPin):
    def line: String = ActionPins.line(name, pin)

  val checkout: BuiltIn =
    built("actions/checkout", "3d3c42e5aac5ba805825da76410c181273ba90b1", "v7.0.1")
  val cache: BuiltIn =
    built("actions/cache", "55cc8345863c7cc4c66a329aec7e433d2d1c52a9", "v6.1.0")
  val appToken: BuiltIn =
    built("actions/create-github-app-token", "64ab729a6294000f5aa4a4bc0b73d1f32bc45510", "v3.3.0")
  val uploadArtifact: BuiltIn =
    built("actions/upload-artifact", "cf430e030ddbb5b0abf93d22962f4752f3646cd9", "v7.0.2")
  val setupJava: BuiltIn =
    built("actions/setup-java", "77dd5b2ce225ea11469928ac317850245a74ae78", "v6.2.0")
  val downloadArtifact: BuiltIn =
    built("actions/download-artifact", "9000827ccba6bdab643e8b6fd33ac0654aef8333", "v8.0.2")
  val setupNode: BuiltIn =
    built("actions/setup-node", "949feb2413d6458794dcd2491c4babbbce0c15c1", "v7.1.0")
  val deployPages: BuiltIn =
    built("actions/deploy-pages", "368f82528645a54fb793d4d04e342629a3f51346", "v5.0.1")
  val uploadPages: BuiltIn =
    built("actions/upload-pages-artifact", "fc324d3547104276b827a68afc52ff2a11cc49c9", "v5.0.0")

  val all: List[BuiltIn] =
    List(checkout, cache, appToken, uploadArtifact, setupJava, downloadArtifact, setupNode, deployPages, uploadPages)

  def line(name: ActionName, pin: ActionPin): String =
    s"${name.value}@${pin.sha.value} # ${pin.tag}"

  /** Catalog key, then a subpath such as `actions/cache/restore` under `actions/cache`. */
  def lookup(catalog: Map[String, ActionPin], action: String): Option[ActionPin] =
    catalog
      .get(action)
      .orElse:
        val keys = catalog.keys.filter(key => action.startsWith(key + "/")).toList
        keys.sortBy(_.length).reverse.headOption.flatMap(catalog.get)

  def catalog(extra: Map[ActionName, ActionPin]): Map[String, ActionPin] =
    val base = all.map(item => item.name.value -> item.pin).toMap
    extra.foldLeft(base): (acc, item) =>
      acc.updated(item._1.value, item._2)

  private def built(name: String, sha: String, tag: String): BuiltIn =
    BuiltIn(ActionName.trusted(name), ActionPin(CommitSha.trusted(sha), tag))
end ActionPins

/** A step `uses` is a pin or a local composite. A bare string does not type-check. */
enum ActionUse:
  case Pinned(name: ActionName, pin: ActionPin)
  case Local(path: String)
