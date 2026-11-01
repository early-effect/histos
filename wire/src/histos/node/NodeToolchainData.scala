package histos.node

import histos.toolchain.{RuntimeIdentity}

/** The Node a Js launch will run. */
final case class NodeSurvey(
    binary: NodeBinary,
    version: NodeVersion,
    identity: RuntimeIdentity,
)
