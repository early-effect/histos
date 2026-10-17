package histos

import _root_.scala.annotation.publicInBinary

/** The binary name of an sbt test framework. The engine does not load it. */
opaque type FrameworkClass = String

object FrameworkClass:
  given CanEqual[FrameworkClass, FrameworkClass] = CanEqual.derived

  private val Name = """[A-Za-z_][A-Za-z0-9_$]*(\.[A-Za-z_][A-Za-z0-9_$]*)*""".r

  inline def apply(inline raw: String): FrameworkClass = ${ FrameworkClassMacro.literal('raw) }

  def from(raw: String): Either[FrameworkClassError, FrameworkClass] =
    if raw.isEmpty then Left(FrameworkClassError.Empty)
    else if Name.matches(raw) then Right(trusted(raw))
    else Left(FrameworkClassError.Bad(raw))

  extension (framework: FrameworkClass) def value: String = framework

  @publicInBinary private[histos] def trusted(raw: String): FrameworkClass = raw

enum FrameworkClassError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a test framework is not empty"
    case Bad(raw) => s"'$raw' is not a test framework"

/** The test-scoped library that brings a provider onto the child classpath. The revision is the build's. */
final case class ProviderLibrary private (organization: String, name: String)

object ProviderLibrary:
  def from(organization: String, name: String): Either[ProviderLibraryError, ProviderLibrary] =
    if organization.isEmpty || name.isEmpty then Left(ProviderLibraryError.Empty)
    else if organization.exists(_.isWhitespace) || name.exists(_.isWhitespace) then Left(ProviderLibraryError.Bad(organization, name))
    else Right(trusted(organization, name))

  @publicInBinary private[histos] def trusted(organization: String, name: String): ProviderLibrary =
    new ProviderLibrary(organization, name)

enum ProviderLibraryError:
  case Empty
  case Bad(organization: String, name: String)

  def message: String = this match
    case Empty                 => "a test provider library is not empty"
    case Bad(organization, name) => s"'$organization' '$name' is not a test provider library"

/**
 * A framework the child process runs. `Zio` is the provider `histos test` uses when a module
 * does not name another one and does not name a main.
 */
final case class TestProvider private (framework: FrameworkClass, library: ProviderLibrary)

object TestProvider:
  val Zio: TestProvider = trusted(
    FrameworkClass.trusted("zio.test.sbt.ZTestFramework"),
    ProviderLibrary.trusted("dev.zio", "zio-test-sbt"),
  )

  def apply(framework: FrameworkClass, organization: String, name: String): Either[ProviderLibraryError, TestProvider] =
    ProviderLibrary.from(organization, name).map(library => trusted(framework, library))

  @publicInBinary private[histos] def trusted(framework: FrameworkClass, library: ProviderLibrary): TestProvider =
    new TestProvider(framework, library)
