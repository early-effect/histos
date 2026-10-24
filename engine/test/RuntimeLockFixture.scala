package histos.toolchain

import histos.archive.{ArchivePin, ArchiveSource, HostArchive}
import histos.jdk.{JdkLock, JdkVersion, Sha256}
import histos.node.{NodeLock, NodeVersion}

import zio.Chunk

private[histos] object RuntimeLockFixture:
  def jdk(
      version: JdkVersion,
      source: ArchiveSource,
      sha256: Sha256
  ): Either[ToolLockError[Nothing], JdkLock] =
    JdkLock.from(
      version,
      Chunk(HostArchive(HostPlatform.LinuxX86_64, ArchivePin(source, sha256)))
    )

  def node(
      version: NodeVersion,
      source: ArchiveSource,
      sha256: Sha256
  ): Either[ToolLockError[Nothing], NodeLock] =
    NodeLock.from(
      version,
      Chunk(HostArchive(HostPlatform.LinuxX86_64, ArchivePin(source, sha256)))
    )
