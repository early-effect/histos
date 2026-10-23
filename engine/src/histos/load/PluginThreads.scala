package histos.load

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

import zio.{Executor, Runtime, Scope, URIO, ZIO}

/**
 * Daemon threads whose context class loader is the build loader. A fiber can move to another
 * thread at any step, so a context loader set on one thread does not follow build or plugin code.
 */
private[load] final class PluginThreads private (main: Executor, blocking: Executor):
  def run[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.scoped[R](Runtime.setBlockingExecutor(blocking).build *> effect.onExecutor(main))

private[load] object PluginThreads:
  def make(loader: ClassLoader): URIO[Scope, PluginThreads] =
    for
      main     <- pool(Executors.newFixedThreadPool(size, daemon(loader)))
      blocking <- pool(Executors.newCachedThreadPool(daemon(loader)))
    yield PluginThreads(Executor.fromJavaExecutor(main), Executor.fromJavaExecutor(blocking))

  private def size: Int =
    math.max(2, java.lang.Runtime.getRuntime.availableProcessors)

  private def pool(open: => ExecutorService): URIO[Scope, ExecutorService] =
    ZIO.acquireRelease(ZIO.succeed(open))(pool => ZIO.succeed(pool.shutdown()))

  private def daemon(loader: ClassLoader): ThreadFactory =
    runnable =>
      val thread = new Thread(runnable, "histos-plugin")
      thread.setDaemon(true)
      thread.setContextClassLoader(loader)
      thread
