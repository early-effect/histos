package histos.load

import java.net.{URL, URLClassLoader}

/**
 * The classloader API reports a missing class by throwing. The refused binary name is stored
 * on the loader. Callers read that name. They do not read the exception text.
 *
 * Boundary names load from `host`, the loader that defined `HistosBuild`. `ProjectGraph` has to be
 * that loader's class. A second copy from the api jar is a different class, and the build object
 * would not match. Refused `histos.*` names are not delegated, so a plugin cannot link
 * `histos.schedule` through this parent. JDK types resolve on the platform loader. The api jar
 * stays on the compile classpath. It does not define these classes a second time.
 */
private[load] final class ApiBoundary(host: ClassLoader) extends ClassLoader(ClassLoader.getPlatformClassLoader):
  override protected def loadClass(name: String, resolve: Boolean): Class[?] =
    getClassLoadingLock(name).synchronized:
      ClassHome.of(name) match
        case ClassHome.Refused => throw new ClassNotFoundException(name)
        case ClassHome.Boundary =>
          val loaded = host.loadClass(name)
          if resolve then resolveClass(loaded)
          loaded
        case ClassHome.Open => super.loadClass(name, resolve)

/** Build classes plus plugin jars. Refused names are thrown before this loader searches its own URLs. */
private[load] final class BuildLoader(urls: Array[URL], parent: ClassLoader) extends URLClassLoader(urls, parent):
  private val refused = new java.util.concurrent.atomic.AtomicReference[String]

  def takeRefused: Option[String] =
    Option(refused.get)

  override protected def loadClass(name: String, resolve: Boolean): Class[?] =
    getClassLoadingLock(name).synchronized:
      ClassHome.of(name) match
        case ClassHome.Refused =>
          refused.set(name)
          throw new ClassNotFoundException(name)
        case ClassHome.Boundary =>
          val loaded = getParent.loadClass(name)
          if resolve then resolveClass(loaded)
          loaded
        case ClassHome.Open =>
          super.loadClass(name, resolve)

  override protected def findClass(name: String): Class[?] =
    ClassHome.of(name) match
      case ClassHome.Refused =>
        refused.set(name)
        throw new ClassNotFoundException(name)
      case ClassHome.Boundary =>
        throw new ClassNotFoundException(name)
      case ClassHome.Open =>
        super.findClass(name)
