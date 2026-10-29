package histos.process

import java.lang.Process as OsProcess

import scala.jdk.CollectionConverters.*

import zio.{UIO, ZIO}

/** A child process and every process below it when the tree was taken. */
private[histos] final case class ProcessTree(leader: OsProcess, below: List[ProcessHandle]):
  /** The leader first, so it starts nothing new. A handle checks its start time, so a reused pid is not killed. */
  def destroy: UIO[Unit] =
    ZIO.attemptBlocking {
      leader.destroyForcibly()
      below.foreach(_.destroyForcibly())
      leader.waitFor()
    }.ignore

private[histos] object ProcessTree:
  /** Taken while the leader lives: once it exits, its children are reparented and are no longer its descendants. */
  def of(leader: OsProcess): UIO[ProcessTree] =
    ZIO
      .attemptBlocking(leader.descendants().iterator().asScala.toList)
      .orElseSucceed(Nil)
      .map(ProcessTree(leader, _))
