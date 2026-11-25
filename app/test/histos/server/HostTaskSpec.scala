package histos.server

import histos.lock.{LockMode, LockTransactionError}
import histos.plugin.{Digests, ProposalId}
import histos.task.{HostProof, HostVerdict}
import histos.suite.HistosSpec

import java.nio.charset.StandardCharsets

import zio.test.*

object HostTaskSpec extends HistosSpec:
  private val consent: String =
    LockTransactionError
      .ConsentRequired(ProposalId.trusted(Digests.sha256("host".getBytes(StandardCharsets.UTF_8))))
      .message

  private val frozen: String =
    LockTransactionError.Unapplied(LockMode.Frozen).message

  def spec = suite("host verdict")(
    test("consent and a nonzero exit pass") {
      assertTrue(HostVerdict.verdict(1, consent) == HostVerdict.Consent)
    },
    test("either frozen marker fails") {
      assertTrue(
        HostVerdict.verdict(1, frozen) == HostVerdict.Failed(HostProof.Frozen),
        HostVerdict.verdict(1, "frozen drift") == HostVerdict.Failed(HostProof.Frozen),
        HostVerdict.verdict(0, s"$consent\nfrozen drift") == HostVerdict.Failed(HostProof.Frozen),
      )
    },
    test("exit 0 fails when the consent sentence is present") {
      assertTrue(HostVerdict.verdict(0, consent) == HostVerdict.Failed(HostProof.ExitedZero))
    },
    test("neither sentence fails") {
      assertTrue(
        HostVerdict.verdict(1, "cell app 3.10.1 jvm does not match the dependency lock") ==
          HostVerdict.Failed(HostProof.Neither)
      )
    },
    test("a silent server keeps the launcher line") {
      val path = java.nio.file.Path.of("scratch")
      val note = "launcher pin is absent: scratch/build.properties"
      assertTrue(
        HostProof.ServerSilent(path, note).message.contains(note),
        HostProof.ServerSilent(path, "").message == s"the scratch server wrote no server.json at ${path.toString}",
      )
    },
  )
end HostTaskSpec
