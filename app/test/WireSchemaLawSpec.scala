package histos.report

import heddle.endpoint.{Schema, SchemaDoc}
import java.nio.file.Path
import zio.json.JsonCodec
import zio.test.*
import zio.{Chunk, Config, NonEmptyChunk}

import WireJson.given
import WireSchema.given

object WireSchemaLawSpec extends ZIOSpecDefault:
  private val hex           = "0123456789abcdef" * 4
  private val home          = Path.of("/work/repo")
  private val core          = histos.ProjectName("core")
  private val scala310      = histos.ScalaVersion("3.10.0")
  private val localIdentity =
    histos.toolchain.RuntimeIdentity.Local(home, histos.jdk.Sha256.trusted(hex))
  private val localJson   = "{\"Local\":{\"home\":\"/work/repo\",\"salt\":\"" + hex + "\"}}"
  private val jdkVersion  = histos.jdk.JdkVersion.trusted("21.0.2", histos.jdk.JdkMajor.trusted(21))
  private val nodeVersion = histos.node.NodeVersion.trusted("22.14.0", histos.node.NodeMajor.trusted(22))
  private val jdkSurvey   = histos.jdk.JdkSurvey(home, jdkVersion, localIdentity)
  private val nodeSurvey  = histos.node.NodeSurvey(histos.node.NodeBinary(home), nodeVersion, localIdentity)
  private val jdkJson     =
    "{\"home\":\"/work/repo\",\"version\":\"21.0.2\",\"identity\":" + localJson + "}"
  private val nodeJson =
    "{\"binary\":{\"path\":\"/work/repo\"},\"version\":\"22.14.0\",\"identity\":" + localJson + "}"
  private val tally = ModuleTally("core", "3.10.0", "jvm", None, None, None)
  private val seen  = Observation(Chunk(tally), None, None, None, 1)

  private val objectNumber  = """(expected '{' got '1')"""
  private val quoteNumber   = """(expected '"' got '1')"""
  private val stringNumber  = "(expected string)"
  private val arrayNumber   = "(expected '[' got '1')"
  private val alienEnum     = "(invalid enumeration value)"
  private val alienSum      = "(invalid disambiguator)"
  private val configOutline =
    "InvalidData(path,message) | MissingData(path,message) | Unsupported(path,message) | " +
      "And(left,right) | Or(left,right) | SourceUnavailable(path,message,cause)"
  private val causeOutline =
    "Empty() | Fail(kind,message) | Die(kind,message) | Interrupt() | Then(left,right) | " +
      "Both(left,right) | Stackless(cause,stackless)"
  private val scheduleOutline =
    "UnknownAction(id) | DuplicateAction(id) | MissingDependency(action,missing) | Cycle(path) | " +
      "DuplicateInput(action,name) | Failed(action,error) | Cache(error) | Remote(error)"

  private def read[A](document: String)(using JsonCodec[A]): Either[String, A] =
    summon[JsonCodec[A]].decodeJson(document)

  private def accepted[A](document: String, value: A)(using JsonCodec[A]): TestResult =
    read[A](document) match
      case Right(got)   => assertTrue(got == value)
      case Left(reason) => assertTrue(reason == "decoded")

  private def rejected[A](document: String, message: String)(using JsonCodec[A]): TestResult =
    read[A](document) match
      case Left(reason) => assertTrue(reason == message)
      case Right(_)     => assertTrue(document == "rejected")

  private def text[A](title: String, good: String, value: A, bad: String, message: String)(using
      Schema[A],
      JsonCodec[A],
  ): TestResult =
    val schema = summon[Schema[A]]
    assertTrue(schema.doc == SchemaDoc.Str(None), schema.named == Some(title)) &&
    accepted[A]("\"" + good + "\"", value) &&
    rejected[A]("\"" + bad + "\"", "(" + message + ")") &&
    rejected[A]("1", stringNumber)

  private def tokens[A](
      title: String,
      names: List[String],
      good: String,
      value: A,
      bad: String,
      message: String,
  )(using Schema[A], JsonCodec[A]): TestResult =
    val schema = summon[Schema[A]]
    assertTrue(schema.doc == SchemaDoc.Enum(Some(title), names), schema.named == None) &&
    accepted[A]("\"" + good + "\"", value) &&
    rejected[A]("\"" + bad + "\"", "(" + message + ")") &&
    rejected[A]("1", stringNumber)

  private def plain[A](title: String, empty: A, word: A)(using Schema[A], JsonCodec[A]): TestResult =
    val schema = summon[Schema[A]]
    assertTrue(schema.doc == SchemaDoc.Str(None), schema.named == Some(title)) &&
    accepted[A]("\"\"", empty) &&
    accepted[A]("\"word\"", word) &&
    rejected[A]("1", stringNumber)

  private def pure[A](title: String, names: List[String], token: String, value: A)(using
      Schema[A],
      JsonCodec[A],
  ): TestResult =
    val schema = summon[Schema[A]]
    assertTrue(schema.doc == SchemaDoc.Enum(Some(title), names), schema.named == Some(title)) &&
    accepted[A]("\"" + token + "\"", value) &&
    rejected[A]("\"__nope__\"", alienEnum) &&
    rejected[A]("1", quoteNumber)

  private def major[A](title: String, good: String, value: A, message: String)(using
      Schema[A],
      JsonCodec[A],
  ): TestResult =
    val schema = summon[Schema[A]]
    assertTrue(schema.doc == SchemaDoc.Integer(Some("int32")), schema.named == Some(title)) &&
    accepted[A](good, value) &&
    rejected[A]("0", "(" + message + ")")

  private def mixed[A](title: String, outline: String, document: String, value: A)(using
      Schema[A],
      JsonCodec[A],
  ): TestResult =
    val schema = summon[Schema[A]]
    assertTrue(schema.named == Some(title), arms(schema.doc).mkString(" | ") == outline) &&
    accepted[A](document, value) &&
    rejected[A]("""{"__nope__":{}}""", alienSum) &&
    rejected[A]("1", objectNumber)

  private def product[A](
      title: String,
      fields: List[String],
      required: List[String],
      document: String,
      value: A,
  )(using Schema[A], JsonCodec[A]): TestResult =
    val schema = summon[Schema[A]]
    val shape  = schema.doc match
      case SchemaDoc.Object(Some(name), params, needed) =>
        assertTrue(
          name == title,
          schema.named == Some(title),
          params.map(_.name) == fields,
          needed == required,
        )
      case other => assertTrue(other.toString == s"object $title")
    shape && accepted[A](document, value) && rejected[A]("1", objectNumber)

  private def arms(doc: SchemaDoc): List[String] =
    doc match
      case SchemaDoc.OneOf(_, variants)      => variants.flatMap(arms)
      case SchemaDoc.Object(None, fields, _) =>
        fields match
          case List(field) =>
            field.doc match
              case SchemaDoc.Object(_, params, _) =>
                List(s"${field.name}(${params.map(_.name).mkString(",")})")
              case _ => List(s"${field.name}(?)")
          case _ => List("wrapper")
      case SchemaDoc.Enum(_, values) => values.map(value => value + "()")
      case other                     => List(other.toString)

  private def wireCause(doc: SchemaDoc): Option[SchemaDoc] =
    doc match
      case SchemaDoc.OneOf(_, variants) =>
        variants.foldLeft(Option.empty[SchemaDoc])((found, variant) => found.orElse(wireCause(variant)))
      case SchemaDoc.Object(None, List(field), _) =>
        if field.name == "SourceUnavailable" then causeField(field.doc) else None
      case _ => None

  private def causeField(doc: SchemaDoc): Option[SchemaDoc] =
    doc match
      case SchemaDoc.Object(_, params, _) =>
        params.foldLeft(Option.empty[SchemaDoc]) { (found, param) =>
          if param.name == "cause" then Some(param.doc) else found
        }
      case _ => None

  private def libraryDoc(doc: SchemaDoc): Option[SchemaDoc] =
    doc match
      case SchemaDoc.Object(_, params, _) =>
        params.foldLeft(Option.empty[SchemaDoc]) { (found, param) =>
          if param.name == "library" then Some(param.doc) else found
        }
      case _ => None

  private def text1: TestResult =
    (
      text[histos.ProjectName](
        "ProjectName",
        "core",
        histos.ProjectName("core"),
        "",
        "a project name is not empty",
      ) &&
        text[histos.ScalaVersion](
          "ScalaVersion",
          "3.10.0",
          scala310,
          "",
          "a Scala version is not empty",
        ) &&
        text[histos.jdk.Sha256](
          "Sha256",
          hex,
          histos.jdk.Sha256.trusted(hex),
          "",
          "sha256 is not 64 hex digits: ",
        ) &&
        text[histos.schedule.Digest](
          "Digest",
          hex,
          histos.schedule.Digest.trusted(hex),
          "",
          "'' is not a digest",
        ) &&
        text[histos.schedule.InputName](
          "InputName",
          "out",
          histos.schedule.InputName.trusted("out"),
          "",
          "an input name is not empty",
        ) &&
        text[histos.task.TaskId](
          "TaskId",
          "inspect",
          histos.task.TaskId.Inspect,
          "",
          "a task id is not empty",
        ) &&
        text[histos.task.RunId](
          "RunId",
          "run-1",
          histos.task.RunId.trusted("run-1"),
          "",
          "a task id is not empty",
        ) &&
        text[histos.version.ShipName](
          "ShipName",
          "histos",
          histos.version.ShipName("histos"),
          "",
          "a ship name is not empty",
        )
    )

  private def text2: TestResult =
    (
      text[histos.version.ShipNumber](
        "ShipNumber",
        "1.2.3",
        histos.version.ShipNumber("1.2.3"),
        "",
        "a ship number is not empty",
      ) &&
        text[histos.version.CiVersion](
          "CiVersion",
          "1.2.3",
          histos.version.CiVersion(histos.version.ShipNumber("1.2.3")),
          "",
          "a ship number is not empty",
        ) &&
        text[histos.Dep](
          "Dep",
          "dev.zio:zio:2.1.27",
          histos.Dep("dev.zio:zio:2.1.27"),
          "",
          "a dependency coordinate is not empty",
        ) &&
        text[histos.plugin.PluginTaskId](
          "PluginTaskId",
          "demo/site",
          histos.plugin.PluginTaskId(histos.plugin.PluginId("demo"), histos.plugin.TaskName("site")),
          "",
          "'' is not a plugin task id",
        ) &&
        text[histos.plugin.ParamName](
          "ParamName",
          "out",
          histos.plugin.ParamName("out"),
          "",
          "a parameter name is not empty",
        ) &&
        text[histos.plugin.FlagName](
          "FlagName",
          "force",
          histos.plugin.FlagName("force"),
          "",
          "a flag name is not empty",
        ) &&
        text[histos.plugin.NoteKey](
          "NoteKey",
          "k",
          histos.plugin.NoteKey("k"),
          "",
          "a note key is not empty",
        ) &&
        text[histos.plugin.OutputName](
          "OutputName",
          "out",
          histos.plugin.OutputName("out"),
          "",
          "an output name is not empty",
        )
    )

  private def text3: TestResult =
    (
      text[histos.plugin.PluginId](
        "PluginId",
        "demo",
        histos.plugin.PluginId("demo"),
        "",
        "a plugin id is not empty",
      ) &&
        text[histos.plugin.EntryName](
          "EntryName",
          "demo.Plugin$",
          histos.plugin.EntryName("demo.Plugin$"),
          "",
          "an entry name is not empty",
        ) &&
        text[histos.plugin.ApiDigest](
          "ApiDigest",
          hex,
          histos.plugin.ApiDigest.trusted(hex),
          "",
          "'' is not a sha256",
        ) &&
        text[histos.plugin.ContentHash](
          "ContentHash",
          hex,
          histos.plugin.ContentHash.trusted(hex),
          "",
          "'' is not a sha256",
        ) &&
        text[histos.jdk.JdkVersion](
          "JdkVersion",
          "21.0.2",
          jdkVersion,
          "nope",
          "not a JDK version: nope",
        ) &&
        text[histos.JsVersion](
          "JsVersion",
          "1.19.0",
          histos.JsVersion("1.19.0"),
          "",
          "a Scala.js toolchain revision is not empty",
        ) &&
        text[histos.NativeVersion](
          "NativeVersion",
          "0.5.9",
          histos.NativeVersion("0.5.9"),
          "",
          "a Scala Native version is not empty",
        ) &&
        text[histos.node.NodeVersion](
          "NodeVersion",
          "22.14.0",
          nodeVersion,
          "0",
          "not a Node version: 0",
        )
    )

  private def text4: TestResult =
    (
      text[histos.plugin.TaskName](
        "TaskName",
        "site",
        histos.plugin.TaskName("site"),
        "",
        "a task name is not empty",
      ) &&
        text[histos.plugin.CommandName](
          "CommandName",
          "site",
          histos.plugin.CommandName("site"),
          "",
          "a command name is not empty",
        ) &&
        text[histos.plugin.JobName](
          "JobName",
          "docs",
          histos.plugin.JobName("docs"),
          "",
          "a job name is not empty",
        ) &&
        text[histos.plugin.ActionName](
          "ActionName",
          "actions/checkout",
          histos.plugin.ActionName("actions/checkout"),
          "",
          "an action name is not empty",
        ) &&
        text[histos.plugin.RunnerName](
          "RunnerName",
          "ubuntu",
          histos.plugin.RunnerName("ubuntu"),
          "",
          "a runner name is not empty",
        ) &&
        text[histos.plugin.SecretName](
          "SecretName",
          "HISTOS_TOKEN",
          histos.plugin.SecretName("HISTOS_TOKEN"),
          "",
          "a secret name is not empty",
        ) &&
        text[histos.plugin.TargetName](
          "TargetName",
          "site",
          histos.plugin.TargetName("site"),
          "",
          "a target name is not empty",
        ) &&
        text[histos.ProjectPath](
          "ProjectPath",
          "modules/core",
          histos.ProjectPath("modules/core"),
          "",
          "a project directory is not empty",
        )
    )

  private def text5: TestResult =
    (
      text[histos.plugin.RepoPath](
        "RepoPath",
        "docs/index.html",
        histos.plugin.RepoPath("docs/index.html"),
        "",
        "a repository path is not empty",
      ) &&
        text[histos.Organization](
          "Organization",
          "rocks.earlyeffect",
          histos.Organization("rocks.earlyeffect"),
          "",
          "'' is not an organization",
        ) &&
        text[histos.ToolRevision](
          "ToolRevision",
          "21.1.8",
          histos.ToolRevision("21.1.8"),
          "",
          "a build tool revision is not empty",
        ) &&
        text[histos.TestMain](
          "TestMain",
          "demo.TestMain",
          histos.TestMain("demo.TestMain"),
          "",
          "a test main is not empty",
        ) &&
        text[histos.FrameworkClass](
          "FrameworkClass",
          "zio.test.ZTestFramework",
          histos.FrameworkClass("zio.test.ZTestFramework"),
          "",
          "a test framework is not empty",
        ) &&
        text[histos.MainClass](
          "MainClass",
          "demo.Exit",
          histos.MainClass("demo.Exit"),
          "",
          "a main class is not empty",
        ) &&
        text[histos.pack.MainClass](
          "MainClass",
          "demo.Exit",
          histos.pack.MainClass.trusted("demo.Exit"),
          "",
          "a main class is not empty",
        ) &&
        text[histos.resolve.JarClassifier](
          "JarClassifier",
          "tests",
          histos.resolve.JarClassifier("tests"),
          "",
          "a jar classifier is not empty",
        )
    )

  private def text6: TestResult =
    (
      text[histos.testrun.SuiteName](
        "SuiteName",
        "demo.Spec",
        histos.testrun.SuiteName("demo.Spec"),
        "",
        "a suite name is not empty",
      ) &&
        text[histos.resolve.RepoUrl](
          "RepoUrl",
          "https://repo1.maven.org/maven2",
          histos.resolve.RepoUrl.trusted("https://repo1.maven.org/maven2"),
          "nope",
          "dependency lock repository is not central, ivy-local, or a maven url: nope",
        ) &&
        text[histos.RepositoryUrl](
          "RepositoryUrl",
          "https://repo1.maven.org/maven2",
          histos.RepositoryUrl("https://repo1.maven.org/maven2"),
          "nope",
          "'nope' is not a repository url",
        ) &&
        text[histos.resolve.Repository](
          "Repository",
          "central",
          histos.resolve.Repository.Central,
          "nope",
          "dependency lock repository is not central, ivy-local, or a maven url: nope",
        ) &&
        text[histos.resolve.ToolVariant](
          "ToolVariant",
          "jvm",
          histos.resolve.ToolVariant.Jvm,
          "Jvm",
          "'Jvm' is not a tool variant",
        )
    )

  private def textLaws: TestResult =
    text1 &&
      text2 &&
      text3 &&
      text4 &&
      text5 &&
      text6

  private def token1: TestResult =
    (
      tokens[histos.Platform](
        "Platform",
        List("jvm", "js", "native"),
        "jvm",
        histos.Platform.Jvm,
        "Jvm",
        "'Jvm' is not a platform (jvm, js, or native)",
      ) &&
        tokens[histos.PomFact](
          "PomFact",
          List("License", "Developer", "Scm"),
          "License",
          histos.PomFact.License,
          "license",
          "'license' is not a pom fact",
        ) &&
        tokens[histos.JsEnv](
          "JsEnv",
          List("node", "jsdom", "firefox"),
          "node",
          histos.JsEnv.Node,
          "Node",
          "'Node' is not a js environment",
        ) &&
        tokens[histos.resolve.Transitivity](
          "Transitivity",
          List("closure", "single"),
          "closure",
          histos.resolve.Transitivity.Closure,
          "Closure",
          "'Closure' is not a transitivity",
        ) &&
        tokens[histos.resolve.ClasspathScope](
          "ClasspathScope",
          List("cell", "test"),
          "cell",
          histos.resolve.ClasspathScope.Compile,
          "Compile",
          "'Compile' is not a classpath scope",
        ) &&
        tokens[histos.plugin.Need](
          "Need",
          List("network", "docker"),
          "network",
          histos.plugin.Need.Network,
          "Network",
          "'Network' is not a need",
        ) &&
        tokens[histos.version.TagRelation](
          "TagRelation",
          List("matches", "moved", "unknown"),
          "matches",
          histos.version.TagRelation.Matches,
          "Matches",
          "'Matches' is not a tag relation",
        ) &&
        tokens[Severity](
          "Severity",
          List("error", "warn", "info"),
          "error",
          Severity.Error,
          "Error",
          "unknown severity Error",
        )
    )

  private def token2: TestResult =
    (
      tokens[histos.resolve.ToolRole](
        "ToolRole",
        List(
          "compiler",
          "semanticdb",
          "jslibraries",
          "jslinker",
          "jsenvironment",
          "jssuite",
          "jstestbridge",
          "nativelibraries",
          "nativelinker",
          "nativesuite",
          "nativetestinterface",
          "scoverage",
          "scalafmtdynamic",
          "scalafmtcore",
        ),
        "compiler",
        histos.resolve.ToolRole.Compiler,
        "Compiler",
        "'Compiler' is not a tool role",
      ) &&
        tokens[histos.resolve.LockField](
          "LockField",
          List(
            "cell",
            "test",
            "plugin",
            "tool",
            "root",
            "resolver",
            "direct",
            "artifact",
            "source",
            "evict",
          ),
          "cell",
          histos.resolve.LockField.Cell,
          "Cell",
          "'Cell' is not a lock field",
        )
    )

  private def tokenLaws: TestResult =
    token1 &&
      token2

  private def pure1: TestResult =
    (
      pure[histos.zinc.ClasspathBan](
        "ClasspathBan",
        List("Scala3Compiler", "Scala2Compiler", "Histos"),
        "Scala3Compiler",
        histos.zinc.ClasspathBan.Scala3Compiler,
      ) &&
        pure[histos.zinc.CompileInput](
          "CompileInput",
          List(
            "Scala",
            "Platform",
            "Jdk",
            "Runtime",
            "Flags",
            "Sources",
            "Artifacts",
            "Apis",
            "Tools",
            "Compiler",
            "Coverage",
          ),
          "Scala",
          histos.zinc.CompileInput.Scala,
        ) &&
        pure[histos.zinc.CompileSeverity](
          "CompileSeverity",
          List("Error", "Warn", "Info"),
          "Error",
          histos.zinc.CompileSeverity.Error,
        ) &&
        pure[histos.zinc.CompilerPart](
          "CompilerPart",
          List(
            "Library",
            "Compiler",
            "Reflect",
            "Bridge",
            "Scala3Compiler",
            "Scala3Bridge",
            "Scala3Library",
          ),
          "Library",
          histos.zinc.CompilerPart.Library,
        ) &&
        pure[histos.process.GroupOp](
          "GroupOp",
          List("Lookup", "Call", "Set", "Pid"),
          "Lookup",
          histos.process.GroupOp.Lookup,
        ) &&
        pure[histos.workspace.IoFault](
          "IoFault",
          List("Read", "Write", "Stat", "Delete", "Create", "Walk", "Temp"),
          "Read",
          histos.workspace.IoFault.Read,
        ) &&
        pure[histos.zinc.JsLibraryMiss](
          "JsLibraryMiss",
          List("Unavailable", "Empty"),
          "Unavailable",
          histos.zinc.JsLibraryMiss.Unavailable,
        ) &&
        pure[histos.central.MetadataFault](
          "MetadataFault",
          List("MissingBuild", "BadBuild", "Saturated", "Unread"),
          "MissingBuild",
          histos.central.MetadataFault.MissingBuild,
        )
    )

  private def pure2: TestResult =
    (
      pure[histos.node.NodeFileFault](
        "NodeFileFault",
        List("Missing", "NotExecutable", "NotAFile"),
        "Missing",
        histos.node.NodeFileFault.Missing,
      ) &&
        pure[histos.node.NodeSource](
          "NodeSource",
          List("Environment", "Config", "Path", "Local"),
          "Environment",
          histos.node.NodeSource.Environment,
        ) &&
        pure[histos.zinc.PositionBase](
          "PositionBase",
          List("Zinc", "Dotty"),
          "Zinc",
          histos.zinc.PositionBase.Zinc,
        ) &&
        pure[histos.central.PublishFault](
          "PublishFault",
          List("Stat", "Write"),
          "Stat",
          histos.central.PublishFault.Stat,
        ) &&
        pure[histos.resolve.RepositoryIo](
          "RepositoryIo",
          List("Fetch", "Metadata"),
          "Fetch",
          histos.resolve.RepositoryIo.Fetch,
        ) &&
        pure[histos.testrun.SuiteNameError](
          "SuiteNameError",
          List("Empty"),
          "Empty",
          histos.testrun.SuiteNameError.Empty,
        ) &&
        pure[histos.resolve.ToolRootFault](
          "ToolRootFault",
          List("Missing", "Duplicated"),
          "Missing",
          histos.resolve.ToolRootFault.Missing,
        )
    )

  private def pureLaws: TestResult =
    pure1 &&
      pure2

  private def sum1: TestResult =
    (
      mixed[histos.assembly.AssembleError](
        "AssembleError",
        (
          "Toolchain(error) | Compile(error) | Resolve(error) | Jar(error) | " +
            "Pack(error) | NoModule(name) | NoClasses(module,version) | Io(path)"
        ),
        """{"Io":{"path":"/work/repo"}}""",
        histos.assembly.AssembleError.Io(home),
      ) &&
        mixed[histos.pack.AssemblyError](
          "AssemblyError",
          (
            "NotADirectory(path) | Empty(path) | Escaped(name) | " +
              "DestinationInside(path) | Overlap(path) | NotAJar(path) | " +
              "MissingMain(main) | NoClasses() | Duplicate(name) | NotAFile(path) | " +
              "LazyVal(error) | Io(op)"
          ),
          """{"NoClasses":{}}""",
          histos.pack.AssemblyError.NoClasses,
        ) &&
        mixed[histos.version.BranchError](
          "BranchError",
          "Unknown(raw)",
          """{"Unknown":{"raw":"nope"}}""",
          histos.version.BranchError.Unknown("nope"),
        ) &&
        mixed[histos.BuildError](
          "BuildError",
          (
            "EmptyName(position) | BadName(raw,position) | " +
              "DuplicateName(name,first,second) | " +
              "DuplicatePlatform(name,platform,position) | " +
              "DuplicateScalaVersion(name,version,position) | " +
              "DuplicateChild(name,child,position) | " +
              "AggregateIncludesItself(name,position) | " +
              "AggregateChild(aggregate,child,position) | " +
              "MissingProject(name,referencedFrom) | " +
              "DependsOnAggregate(module,aggregate,position) | " +
              "SelfDependency(name,position) | " +
              "DuplicateDependency(module,dependency,position) | " +
              "EdgeWithoutPlatform(module,platform,position) | " +
              "UpstreamMissingPlatform(module,dependency,platform,position) | " +
              "Cycle(platform,path) | MissingClang(name,position) | " +
              "EmptyShipName(position) | BadShipName(raw,position) | " +
              "DuplicateShip(name,first,second) | " +
              "RepeatedShipMember(ship,member,position) | " +
              "AggregateShipMember(ship,aggregate,position) | " +
              "MemberInTwoShips(member,first,second,position) | " +
              "DuplicateResolver(token) | MixedTest(name,position) | " +
              "DuplicateDirectory(name,other,directory) | DuplicatePlugin(id) | " +
              "DuplicatePluginTask(plugin,name) | DuplicateCommand(name) | " +
              "DuplicateJob(name) | ReservedJob(name) | DuplicateSite(first,second) | " +
              "Ci(fault) | DuplicateNote(module,plugin,key) | " +
              "MissingNote(module,plugin,key) | PluginMissingModule(plugin,name) | " +
              "PluginMissingCell(plugin,module,version,platform) | " +
              "UnknownPluginTask(plugin,name) | CommandShadow(name) | " +
              "PluginDefect(plugin,binary) | TwoPublications(name,position) | " +
              "AssemblyShape(name,position) | AssetWithoutAssembly(name,position) | " +
              "TwoAssets(name,position) | DuplicateResource(name,resource,position) | " +
              "RemoteNeeds(name,needs,position)"
          ),
          """{"DuplicateResolver":{"token":"central"}}""",
          histos.BuildError.DuplicateResolver("central"),
        ) &&
        mixed[histos.build.BuildEvent](
          "BuildEvent",
          "Compile(feed) | Test(feed) | Out(text) | Err(text)",
          """{"Out":{"text":"x"}}""",
          histos.build.BuildEvent.Out("x"),
        ) &&
        mixed[histos.report.BuildFault](
          "BuildFault",
          (
            "Load(error) | Resolve(error) | Toolchain(error) | Survey(error) | " +
              "Select(error) | Pattern(error) | Compile(error) | Test(error) | " +
              "Publish(error) | Release(detail) | Workflow(error) | Program(error) | " +
              "Platform(error) | Task(error) | Plugin(error) | PluginTask(error) | " +
              "Preview(error) | Assemble(error) | Launcher(error) | Cache(error) | " +
              "Remote(error) | Store(failure) | Io(failure) | Version(error) | " +
              "Watch(error) | Format(error) | Interrupted() | Rejected(rejection)"
          ),
          """{"Interrupted":{}}""",
          histos.report.BuildFault.Interrupted,
        ) &&
        mixed[histos.schedule.CacheError](
          "CacheError",
          "Io(failure) | Corrupt(digest)",
          "{\"Corrupt\":{\"digest\":\"" + hex + "\"}}",
          histos.schedule.CacheError.Corrupt(histos.schedule.Digest.trusted(hex)),
        ) &&
        mixed[histos.schedule.CacheExchange](
          "CacheExchange",
          (
            "Target() | Connect(host,port) | ConnectTimeout(host,port) | " +
              "Tls(host,port) | ReadTimeout(host,port) | Io(host,port) | " +
              "Protocol(host,port) | Pool(host,port) | Trust() | Body()"
          ),
          """{"Target":{}}""",
          histos.schedule.CacheExchange.Target,
        )
    )

  private def sum2: TestResult =
    (
      mixed[histos.report.CallResult](
        "CallResult",
        "Reported(report) | Fault(fault)",
        """{"Fault":{"fault":{"Interrupted":{}}}}""",
        CallResult.Fault(BuildFault.Interrupted),
      ) &&
        mixed[histos.process.ChildError](
          "ChildError",
          (
            "UnsupportedPlatform(platform) | Java(path) | Classes(path) | " +
              "Artifact(path) | Work(path) | Group(op) | Bootstrap(output) | " +
              "NoProgram() | Start(program) | Property(name) | Status() | Console() | " +
              "Io(failure)"
          ),
          """{"NoProgram":{}}""",
          histos.process.ChildError.NoProgram,
        ) &&
        mixed[histos.workflow.CiError](
          "CiError",
          (
            "BadName(raw) | Unknown(name,near) | NotModule(name) | Duplicate(name) | " +
              "Missing(path) | Drift(file,hint) | Io(failure) | " +
              "DuplicateSite(first,second) | IllegalSite(name) | " +
              "UnknownNeed(job,need) | DuplicateNeed(job,need) | " +
              "UnknownModule(job,module) | DuplicateTarget(job,target) | " +
              "UnpinnedAction(job,action) | GrantMissing(job,action) | " +
              "UnknownRunner(job,runner) | UnknownSecret(secret) | " +
              "UnreachableCond(job) | DispatchInputConflict(name) | " +
              "ConcurrencyConflict(file) | UnpinnedWorkflowRun(name) | Pins(text) | " +
              "NoDefaultBranch() | JobTimeout(count) | LlvmChecksum(archive) | SnapshotClients(ship)"
          ),
          """{"NoDefaultBranch":{}}""",
          histos.workflow.CiError.NoDefaultBranch,
        ) &&
        mixed[histos.CiFault](
          "CiFault",
          (
            "IllegalSite(name) | UnknownNeed(job,need) | DuplicateNeed(job,need) | " +
              "UnknownModule(job,module) | DuplicateTarget(job,target) | " +
              "UnpinnedAction(job,action) | GrantMissing(job,action) | " +
              "UnknownRunner(job,runner) | UnknownSecret(secret) | " +
              "UnreachableCond(job) | DispatchInputConflict(name) | " +
              "ConcurrencyConflict(file) | UnpinnedWorkflowRun(name)"
          ),
          """{"DispatchInputConflict":{"name":"x"}}""",
          histos.CiFault.DispatchInputConflict("x"),
        ) &&
        mixed[histos.native.ClangMiss](
          "ClangMiss",
          (
            "Absent() | NotOnPath(wanted) | BadPath(entry) | NoClangPP(clang) | " +
              "Unreadable(path) | Status(path,code) | Text(path,error) | " +
              "WrongMajor(found,wanted,clang) | Mismatched(clang,clangpp)"
          ),
          """{"Absent":{}}""",
          histos.native.ClangMiss.Absent,
        ) &&
        mixed[histos.native.ClangText](
          "ClangText",
          "Empty() | NoVersion() | BadVersion(raw) | NoTarget()",
          """{"Empty":{}}""",
          histos.native.ClangText.Empty,
        ) &&
        mixed[histos.native.ClassShapeError](
          "ClassShapeError",
          "Failed(kind) | Read(path) | ClassFile(label)",
          """{"Failed":{"kind":"nir"}}""",
          histos.native.ClassShapeError.Failed("nir"),
        ) &&
        mixed[histos.resolve.ClassifierError](
          "ClassifierError",
          "Empty() | Invalid(raw)",
          """{"Empty":{}}""",
          histos.resolve.ClassifierError.Empty,
        )
    )

  private def sum3: TestResult =
    (
      mixed[histos.version.CommitShaError](
        "CommitShaError",
        "Empty() | NotHex(raw)",
        """{"Empty":{}}""",
        histos.version.CommitShaError.Empty,
      ) &&
        mixed[histos.zinc.CompileError](
          "CompileError",
          (
            "Resolved(error) | UnsupportedPlatform(platform) | " +
              "UnsupportedScala(version) | NoSources() | Banned(path,ban) | " +
              "Failed(diagnostics) | Toolchain(miss) | Io(failure) | JsLibrary(miss) | " +
              "NativeFetch(error) | NativeOrigin(error) | NativeLibrary(text) | " +
              "Stored(error)"
          ),
          """{"NoSources":{}}""",
          histos.zinc.CompileError.NoSources,
        ) &&
        mixed[histos.zinc.CompileFeed](
          "CompileFeed",
          (
            "Began(module,version,platform) | " +
              "Diagnostic(module,version,platform,problem) | " +
              "Miss(module,version,platform,change) | Done(cell) | Failed(error)"
          ),
          """{"Began":{"module":"core","version":"3.10.0","platform":"jvm"}}""",
          histos.zinc.CompileFeed.Began(core, scala310, histos.Platform.Jvm),
        ) &&
        mixed[histos.DepError](
          "DepError",
          "Empty() | BadShape(raw)",
          """{"Empty":{}}""",
          histos.DepError.Empty,
        ) &&
        mixed[histos.schedule.DevNameError](
          "DevNameError",
          "Segment(raw)",
          """{"Segment":{"raw":"nope"}}""",
          histos.schedule.DevNameError.Segment("nope"),
        ) &&
        mixed[histos.plugin.FlagNameError](
          "FlagNameError",
          "Empty() | Bad(raw)",
          """{"Empty":{}}""",
          histos.plugin.FlagNameError.Empty,
        ) &&
        mixed[histos.zinc.GraphCompileError](
          "GraphCompileError",
          (
            "Runtime(error) | Target(error) | UnknownCell(module,version) | " +
              "NoRoot(module) | NoSources(module,root) | " +
              "MissingUpstream(module,dependency,version,platform) | " +
              "IncompatibleUpstream(module,dependency,version,upstream,platform) | " +
              "Cycle(module) | Lock(error) | LockRead(path) | RootRead(path) | " +
              "Output(path) | Failed(module,version,platform,error) | " +
              "Assets(module,version,platform,error) | Cells(errors)"
          ),
          """{"NoRoot":{"module":"core"}}""",
          histos.zinc.GraphCompileError.NoRoot(core),
        ) &&
        mixed[histos.pack.JarError](
          "JarError",
          (
            "NotADirectory(path) | Empty(path) | Escaped(name) | Duplicate(name) | " +
              "DestinationInside(path) | Io(op) | Scaladoc(text)"
          ),
          """{"Scaladoc":{"text":"boom"}}""",
          histos.pack.JarError.Scaladoc("boom"),
        )
    )

  private def sum4: TestResult =
    (
      mixed[histos.pack.JarIo](
        "JarIo",
        "Read(path) | Write(path) | Move(path)",
        """{"Read":{"path":"/work/repo"}}""",
        histos.pack.JarIo.Read(home),
      ) &&
        mixed[histos.jdk.JdkError](
          "JdkError",
          "BadMajor(raw) | Io(failure)",
          """{"BadMajor":{"raw":0}}""",
          histos.jdk.JdkError.BadMajor(0),
        ) &&
        mixed[histos.js.JsLinkError](
          "JsLinkError",
          (
            "Resolved(error) | Unavailable() | Library(error) | Product(error) | " +
              "Linker(text)"
          ),
          """{"Unavailable":{}}""",
          histos.js.JsLinkError.Unavailable,
        ) &&
        mixed[histos.js.JsRunError](
          "JsRunError",
          "Resolved(error) | Unavailable() | Compile(error) | Write(path)",
          """{"Unavailable":{}}""",
          histos.js.JsRunError.Unavailable,
        ) &&
        mixed[histos.js.JsSuiteError](
          "JsSuiteError",
          "Resolved(error) | Unavailable() | Library(error) | Run(text)",
          """{"Unavailable":{}}""",
          histos.js.JsSuiteError.Unavailable,
        ) &&
        mixed[histos.zinc.KeyMiss](
          "KeyMiss",
          "NoPrior() | Changed(names)",
          """{"NoPrior":{}}""",
          histos.zinc.KeyMiss.NoPrior,
        ) &&
        mixed[histos.publish.LauncherError](
          "LauncherError",
          (
            "Line(raw) | Unknown(raw) | Duplicate() | Missing() | Version(raw) | " +
              "Release(raw) | Repository(raw) | Resolve(detail) | Io(error) | " +
              "InvalidPath(path) | Absent(path)"
          ),
          """{"Duplicate":{}}""",
          histos.publish.LauncherError.Duplicate,
        ) &&
        mixed[histos.pack.LazyValError](
          "LazyValError",
          (
            "StillCalls(entry,method) | MissingHandle(entry,owner,offset) | " +
              "OffsetShape(entry,method) | Frames(entry,failure)"
          ),
          """{"StillCalls":{"entry":"e","method":"m"}}""",
          histos.pack.LazyValError.StillCalls("e", "m"),
        )
    )

  private def sum5: TestResult =
    (
      mixed[histos.pack.LazyValFrame](
        "LazyValFrame",
        (
          "Rejected() | MethodTooLarge(owner,method,descriptor,codeSize) | " +
            "ClassTooLarge(owner,pool) | Limit() | Bounds()"
        ),
        """{"Rejected":{}}""",
        histos.pack.LazyValFrame.Rejected,
      ) &&
        mixed[histos.native.LinkError](
          "LinkError",
          "Interrupted() | Linker(text) | Unexpected(kind)",
          """{"Interrupted":{}}""",
          histos.native.LinkError.Interrupted,
        ) &&
        mixed[histos.load.LoadError](
          "LoadError",
          (
            "Runtime(error) | MissingFile(path) | CompileFailed(diagnostics) | " +
              "NoEntry() | AmbiguousEntry(classes) | BrokenGraph(error) | " +
              "Plugin(error) | Resolve(error) | MissingApi(path) | NoHome() | " +
              "Io(failure) | Jar(error)"
          ),
          """{"NoEntry":{}}""",
          histos.load.LoadError.NoEntry,
        ) &&
        mixed[histos.publish.LocalPublishError](
          "LocalPublishError",
          (
            "Version(error) | Target(error) | Compile(error) | " +
              "Jar(module,version,platform,error) | " +
              "SourceRead(module,version,platform,error) | " +
              "NoSources(module,version,platform) | Publish(error) | Read(path) | " +
              "NoOrganization() | NothingToPublish() | Development(ship,compile) | " +
              "NoCommit() | UnknownShip(ship) | NotPublished(module) | " +
              "SnapshotDependency(ship,dependency) | Unknown(raw) | " +
              "UnpublishedDependency(module,dependency) | Facts(missing) | " +
              "BadDependency(organization,name,revision) | Credentials() | Io(path) | " +
              "Stock(message) | Classpath(error) | Runtime(error) | " +
              "Pack(module,version,platform,error) | " +
              "MissingClasses(module,version,platform) | MissingClient(ship,path)"
          ),
          """{"NoOrganization":{}}""",
          histos.publish.LocalPublishError.NoOrganization,
        ) &&
        mixed[histos.resolve.LockError](
          "LockError",
          (
            "Empty() | BadLine(line) | " +
              "DuplicateCell(module,scalaVersion,platform,scope) | DuplicateTool(id) | " +
              "DuplicatePlugin(organization,name,revision) | Missing(field) | " +
              "Classifier(error) | BadSha256(raw) | BadRepository(raw)"
          ),
          """{"Empty":{}}""",
          histos.resolve.LockError.Empty,
        ) &&
        mixed[histos.pack.MainClassError](
          "MainClassError",
          "Empty() | Bad(raw)",
          """{"Empty":{}}""",
          histos.pack.MainClassError.Empty,
        ) &&
        mixed[histos.testrun.ModuleTestError](
          "ModuleTestError",
          (
            "Tool(module,version,error) | Target(error) | Compile(error) | " +
              "Lock(error) | LockRead(path) | Sources(module,version,error) | " +
              "MissingSources(module,main) | MissingProvider(module,provider) | " +
              "ProviderDependency(module,provider) | NoneDiscovered(module,provider) | " +
              "NoneMatched(patterns) | Mixed(module) | Process(module,version,error) | " +
              "Main(error) | Absent(module) | " +
              "MissingUpstream(module,dependency,version) | Work(path) | Root(path) | " +
              "Runner(path) | LinkFailed(module,version,platform,detail) | " +
              "NoNode(module,version,miss) | Stamp(error) | Record(error) | " +
              "Pattern(error) | Api(module,version,platform)"
          ),
          """{"Absent":{"module":"core"}}""",
          histos.testrun.ModuleTestError.Absent(core),
        ) &&
        mixed[histos.schedule.NamespaceError](
          "NamespaceError",
          "Shape(raw) | Number(error) | Name(error)",
          """{"Shape":{"raw":"nope"}}""",
          histos.schedule.NamespaceError.Shape("nope"),
        )
    )

  private def sum6: TestResult =
    (
      mixed[histos.native.NativeError](
        "NativeError",
        "Clang(miss) | Fetch(error) | Compile(error) | Link(error)",
        """{"Link":{"error":{"Interrupted":{}}}}""",
        histos.native.NativeError.Link(histos.native.LinkError.Interrupted),
      ) &&
        mixed[histos.native.NativeFetch](
          "NativeFetch",
          (
            "Compiler(error) | Resolved(error) | Missing(artifact,revision) | " +
              "Duplicated(artifact)"
          ),
          """{"Duplicated":{"artifact":"jar"}}""",
          histos.native.NativeFetch.Duplicated("jar"),
        ) &&
        mixed[histos.native.NativeSuiteError](
          "NativeSuiteError",
          (
            "Resolved(error) | Unavailable() | Shapes(error) | Compile(error) | " +
              "Write(path)"
          ),
          """{"Unavailable":{}}""",
          histos.native.NativeSuiteError.Unavailable,
        ) &&
        mixed[histos.native.NativeToolchainError](
          "NativeToolchainError",
          "Empty() | Origin(raw) | Revision(raw) | Repository(error)",
          """{"Empty":{}}""",
          histos.native.NativeToolchainError.Empty,
        ) &&
        mixed[histos.node.NodeError](
          "NodeError",
          (
            "BadMajor(raw) | Unreadable(path) | Locations(error) | " +
              "Version(path,error) | Override(error) | Io(failure)"
          ),
          """{"BadMajor":{"raw":0}}""",
          histos.node.NodeError.BadMajor(0),
        ) &&
        mixed[histos.node.NodeMiss](
          "NodeMiss",
          (
            "Unresolved(error) | Rejected(path,reason,source) | " +
              "TooOld(required,found,home,source) | Absent(required,source) | " +
              "Unusable(path,source,fault) | Refused(source,error)"
          ),
          """{"Unresolved":{"error":{"BadMajor":{"raw":0}}}}""",
          histos.node.NodeMiss.Unresolved(histos.node.NodeError.BadMajor(0)),
        ) &&
        mixed[histos.node.NodeOverrideError](
          "NodeOverrideError",
          "Empty() | Multiple() | Relative(raw) | Invalid(raw)",
          """{"Empty":{}}""",
          histos.node.NodeOverrideError.Empty,
        ) &&
        mixed[histos.NodePathError](
          "NodePathError",
          "Empty() | Relative(raw) | NotBinary(raw) | Invalid(raw)",
          """{"Empty":{}}""",
          histos.NodePathError.Empty,
        )
    )

  private def sum7: TestResult =
    (
      mixed[histos.node.NodeReject](
        "NodeReject",
        "Absent() | NoBinary() | UnreadableVersion(error)",
        """{"Absent":{}}""",
        histos.node.NodeReject.Absent,
      ) &&
        mixed[histos.node.NodeVersionError](
          "NodeVersionError",
          (
            "Bad(raw) | Status(code) | Missing() | MissingStatus() | " +
              "Process(error) | NotExecutable(binary)"
          ),
          """{"Missing":{}}""",
          histos.node.NodeVersionError.Missing,
        ) &&
        mixed[histos.report.ObserveError](
          "ObserveError",
          "Missing(path) | Unreadable(path) | Malformed(path)",
          """{"Missing":{"path":"tally.json"}}""",
          ObserveError.Missing("tally.json"),
        ) &&
        mixed[histos.jdk.OverrideError](
          "OverrideError",
          "Empty() | Multiple() | Relative(raw) | Invalid(raw)",
          """{"Empty":{}}""",
          histos.jdk.OverrideError.Empty,
        ) &&
        mixed[histos.plugin.ParamNameError](
          "ParamNameError",
          "Empty() | Bad(raw)",
          """{"Empty":{}}""",
          histos.plugin.ParamNameError.Empty,
        ) &&
        mixed[histos.PlatformError](
          "PlatformError",
          "Unknown(raw)",
          """{"Unknown":{"raw":"Jvm"}}""",
          histos.PlatformError.Unknown("Jvm"),
        ) &&
        mixed[histos.toolchain.PlatformToolError](
          "PlatformToolError",
          (
            "JsLink(error) | JsRun(error) | JsSuite(error) | Native(error) | " +
              "NativeSuite(error) | NativeOrigin(error)"
          ),
          """{"JsLink":{"error":{"Unavailable":{}}}}""",
          histos.toolchain.PlatformToolError.JsLink(histos.js.JsLinkError.Unavailable),
        ) &&
        mixed[histos.plugin.PluginCoordinate](
          "PluginCoordinate",
          "Published(dependency) | Path(path)",
          """{"Path":{"path":"modules/core"}}""",
          histos.plugin.PluginCoordinate.Path(histos.ProjectPath("modules/core")),
        )
    )

  private def sum8: TestResult =
    (
      mixed[histos.load.PluginFault](
        "PluginFault",
        (
          "Task(error) | Compiled(error) | Resolved(error) | Tested(error) | " +
            "Scheduled(error)"
        ),
        """{"Task":{"error":{"Defect":{"binary":"broke"}}}}""",
        histos.load.PluginFault.Task(histos.plugin.PluginRunError.Defect("broke")),
      ) &&
        mixed[histos.plugin.PluginIo](
          "PluginIo",
          "Read(path) | Resolve(path) | Compile(path) | Open(path)",
          """{"Read":{"path":"/work/repo"}}""",
          histos.plugin.PluginIo.Read(home),
        ) &&
        mixed[histos.plugin.PluginLoadError](
          "PluginLoadError",
          (
            "BadCoordinate(raw) | Misplaced(line) | Unlocked(coordinate) | " +
              "NoEntry(coordinate) | AmbiguousEntry(coordinate) | " +
              "Unversioned(coordinate) | " +
              "VersionMismatch(dependency,pluginApi,launcher) | ApiLink(dependency) | " +
              "NoSource(coordinate) | PathIsSource(path) | ReservedPackage(binary) | " +
              "EngineLeak(coordinate,binary) | BuildLeak(binary) | " +
              "ForeignEntry(plugin,entry) | NotEntry(plugin,entry,expected) | " +
              "IdMismatch(declared,found) | Undeclared(plugin) | " +
              "Defect(coordinate,binary) | Io(op)"
          ),
          """{"BadCoordinate":{"raw":"x"}}""",
          histos.plugin.PluginLoadError.BadCoordinate("x"),
        ) &&
        mixed[histos.plugin.PluginRunError](
          "PluginRunError",
          (
            "Failed(detail) | MissingOutput(name) | ExtraOutput(path) | " +
              "MissingArg(name) | MissingNote(module,key) | Unknown(id) | " +
              "Cycle(path) | Defect(binary) | EngineLeak(binary) | Linkage(binary)"
          ),
          """{"Defect":{"binary":"broke"}}""",
          histos.plugin.PluginRunError.Defect("broke"),
        ) &&
        mixed[histos.plugin.PluginTaskError](
          "PluginTaskError",
          "Shape(raw) | Plugin(error) | Task(error)",
          """{"Shape":{"raw":"x"}}""",
          histos.plugin.PluginTaskError.Shape("x"),
        ) &&
        mixed[histos.preview.PreviewError](
          "PreviewError",
          (
            "Target(error) | Cycle(module) | Sources(failure) | Version(error) | " +
              "Publish(error) | Run(miss) | Pattern(error)"
          ),
          """{"Cycle":{"module":"core"}}""",
          histos.preview.PreviewError.Cycle(core),
        ) &&
        mixed[histos.program.ProgramError](
          "ProgramError",
          (
            "Tool(module,version,error) | Absent(name,near) | Aggregate(name) | " +
              "NoMain(name) | Choose(name,platforms) | Unbuilt(name,platform,built) | " +
              "NoClang(name,version) | Main(error) | Compile(error) | Library(error) | " +
              "Lock(error) | LockRead(path) | Link(module,version,platform,detail) | " +
              "Process(module,version,detail) | NoNode(module,version,miss) | " +
              "MissingUpstream(module,dependency,version) | Work(path) | Root(path)"
          ),
          """{"Aggregate":{"name":"core"}}""",
          histos.program.ProgramError.Aggregate(core),
        ) &&
        mixed[histos.ProjectNameError](
          "ProjectNameError",
          "Empty() | Bad(raw)",
          """{"Empty":{}}""",
          histos.ProjectNameError.Empty,
        )
    )

  private def sum9: TestResult =
    (
      mixed[histos.central.PublishError](
        "PublishError",
        (
          "EmptyJar() | NotAJar() | NotADirectory(path) | Escaped(path) | " +
            "Io(path,fault) | Unauthorized() | NotSnapshot(error) | Status(code) | " +
            "Transport(reason) | Metadata(reason)"
        ),
        """{"EmptyJar":{}}""",
        histos.central.PublishError.EmptyJar,
      ) &&
        mixed[histos.version.PullRequestError](
          "PullRequestError",
          "NotANumber(raw) | NotPositive(value)",
          """{"NotANumber":{"raw":"x"}}""",
          histos.version.PullRequestError.NotANumber("x"),
        ) &&
        mixed[histos.testrun.QuickError](
          "QuickError",
          (
            "Save(failure) | Clear(failure) | Read(failure) | Shape(path) | " +
              "Hash(path,error) | Suite(error)"
          ),
          """{"Suite":{"error":"Empty"}}""",
          histos.testrun.QuickError.Suite(histos.testrun.SuiteNameError.Empty),
        ) &&
        mixed[histos.version.Refusal](
          "Refusal",
          (
            "Unchecked() | Shadowed(ship,number) | Unproven(ship,number) | " +
              "NothingToRelease()"
          ),
          """{"Unchecked":{}}""",
          histos.version.Refusal.Unchecked,
        ) &&
        mixed[histos.version.RegistryError](
          "RegistryError",
          "BadLine(line) | Name(error) | Relation(error) | Duplicate(name)",
          """{"BadLine":{"line":"x"}}""",
          histos.version.RegistryError.BadLine("x"),
        ) &&
        mixed[histos.jdk.Reject](
          "Reject",
          "Absent() | NoJavac() | UnreadableVersion(error)",
          """{"Absent":{}}""",
          histos.jdk.Reject.Absent,
        ) &&
        mixed[histos.report.Rejection](
          "Rejection",
          (
            "ResolveAction(raw) | WorkflowAction(raw) | LauncherAction(raw) | " +
              "AssemblyDestination(raw) | TaskArguments(id) | NotATool() | " +
              "Watch(detail) | RunFailed() | WatchStopped() | PreviewProjects() | " +
              "PreviewOperation(raw) | MissingRunName() | Parameter(error) | " +
              "ProjectName(error) | SuitePattern(error) | RunId(error) | " +
              "TestMode(error) | CacheClean(error) | Flag(error) | " +
              "TaskPlatform(name) | CommandHelp(names) | FormatMode(raw) | FormatTargets() | FormatPath(raw)"
          ),
          """{"NotATool":{}}""",
          histos.report.Rejection.NotATool,
        ) &&
        mixed[histos.jdk.ReleaseError](
          "ReleaseError",
          "NoFile() | MissingKey() | Unclosed() | BadVersion(raw)",
          """{"NoFile":{}}""",
          histos.jdk.ReleaseError.NoFile,
        )
    )

  private def sum10: TestResult =
    (
      mixed[histos.schedule.RemoteError](
        "RemoteError",
        (
          "Off() | ReadOnly() | Dangling(action,output) | Text(action) | " +
            "Exchange(fault) | Status(code) | Index(action) | " +
            "Mismatch(action,output) | Catalog(line) | Scheme(raw) | Mode(raw) | " +
            "Address(raw) | Namespace(error) | Rpc(code) | HttpOnly(command)"
        ),
        """{"Off":{}}""",
        histos.schedule.RemoteError.Off,
      ) &&
        mixed[histos.RepositoryUrlError](
          "RepositoryUrlError",
          "Bad(raw)",
          """{"Bad":{"raw":"nope"}}""",
          histos.RepositoryUrlError.Bad("nope"),
        ) &&
        mixed[histos.resolve.ResolveError](
          "ResolveError",
          (
            "ToolRoot(id,root,fault) | NoTool(id) | ToolDrift(id) | " +
              "CompilerVersion(error) | NativeOrigin(error) | Range(dependency) | " +
              "NotFound(organization,name,revision) | Checksum(path,expected,actual) | " +
              "LockDrift(module,scalaVersion,platform,scope,changed) | " +
              "PluginDrift(dependency,changed) | NoLock(path) | " +
              "UnsupportedPlatform(platform) | Lock(error) | Io(failure) | " +
              "Plugin(error) | Repository(op) | Scalafmt(error)"
          ),
          """{"NotFound":{"organization":"dev.zio","name":"zio","revision":"0"}}""",
          histos.resolve.ResolveError.NotFound("dev.zio", "zio", "0"),
        ) &&
        mixed[histos.toolchain.RuntimeIdentity](
          "RuntimeIdentity",
          "Local(home,salt)",
          localJson,
          localIdentity,
        ) &&
        mixed[histos.toolchain.RuntimeLocationError](
          "RuntimeLocationError",
          "InvalidPath(raw) | Io(failure)",
          """{"InvalidPath":{"raw":"nope"}}""",
          histos.toolchain.RuntimeLocationError.InvalidPath("nope"),
        ) &&
        mixed[histos.ScalaVersionError](
          "ScalaVersionError",
          "Empty() | NotFull(raw)",
          """{"Empty":{}}""",
          histos.ScalaVersionError.Empty,
        ) &&
        mixed[histos.publish.ScriptError](
          "ScriptError",
          "Drift(path) | Missing(path) | Io(failure) | Revision() | Pin(error)",
          """{"Revision":{}}""",
          histos.publish.ScriptError.Revision,
        ) &&
        mixed[histos.select.SelectError](
          "SelectError",
          "Duplicate(name) | Unknown(name,near)",
          """{"Duplicate":{"name":"core"}}""",
          histos.select.SelectError.Duplicate(core),
        )
    )

  private def sum11: TestResult =
    (
      mixed[histos.version.SessionError](
        "SessionError",
        (
          "Unknown(raw) | MissingSha() | MissingBranch() | MissingPullRequest() | " +
            "Sha(error) | BadBranch(error) | PullRequest(error)"
        ),
        """{"MissingSha":{}}""",
        histos.version.SessionError.MissingSha,
      ) &&
        mixed[histos.jdk.Sha256Error](
          "Sha256Error",
          "Bad(raw)",
          """{"Bad":{"raw":"nope"}}""",
          histos.jdk.Sha256Error.Bad("nope"),
        ) &&
        mixed[histos.version.ShipNameError](
          "ShipNameError",
          "Empty() | Bad(raw)",
          """{"Empty":{}}""",
          histos.version.ShipNameError.Empty,
        ) &&
        mixed[histos.version.ShipNumberError](
          "ShipNumberError",
          "Empty() | NotAShip(raw)",
          """{"Empty":{}}""",
          histos.version.ShipNumberError.Empty,
        ) &&
        mixed[histos.central.SnapshotRevisionError](
          "SnapshotRevisionError",
          "NotSnapshot(revision)",
          """{"NotSnapshot":{"revision":"1.2.3"}}""",
          histos.central.SnapshotRevisionError.NotSnapshot("1.2.3"),
        ) &&
        mixed[histos.testrun.StampError](
          "StampError",
          "Io(failure) | Class(path) | Missing(name)",
          """{"Missing":{"name":"demo"}}""",
          histos.testrun.StampError.Missing("demo"),
        ) &&
        mixed[histos.testrun.SuitePatternError](
          "SuitePatternError",
          "Empty() | Blank(raw)",
          """{"Empty":{}}""",
          histos.testrun.SuitePatternError.Empty,
        ) &&
        mixed[histos.toolchain.SurveyError](
          "SurveyError",
          (
            "Jdk(error) | Node(jdk,error) | Undeclared(reached,name) | " +
              "Split(reached,left,right) | Clang(reached,error) | " +
              "Incomplete(report,detail)"
          ),
          """{"Incomplete":{"report":"report","detail":"detail"}}""",
          histos.toolchain.SurveyError.Incomplete("report", "detail"),
        )
    )

  private def sum12: TestResult =
    (
      mixed[histos.version.TagRelationError](
        "TagRelationError",
        "Unknown(raw)",
        """{"Unknown":{"raw":"Matches"}}""",
        histos.version.TagRelationError.Unknown("Matches"),
      ) &&
        mixed[histos.task.TaskError](
          "TaskError",
          (
            "EmptyId() | NoRunner(id) | UnknownRun(id) | Version(error) | " +
              "Plugin(error) | Doc(lines)"
          ),
          """{"EmptyId":{}}""",
          histos.task.TaskError.EmptyId,
        ) &&
        mixed[histos.testrun.TestFeed](
          "TestFeed",
          (
            "Begin(module,version,platform) | Out(module,version,platform,text) | " +
              "Err(module,version,platform,text) | Done(cell) | " +
              "Cached(module,version,platform,suite)"
          ),
          """{"Begin":{"module":"core","version":"3.10.0","platform":"jvm"}}""",
          histos.testrun.TestFeed.Begin(core, scala310, histos.Platform.Jvm),
        ) &&
        mixed[histos.testrun.TestStatus](
          "TestStatus",
          "NoTests() | Unmatched() | Passed() | Failed(exit)",
          """{"NoTests":{}}""",
          histos.testrun.TestStatus.NoTests,
        ) &&
        mixed[histos.jdk.ToolchainError](
          "ToolchainError",
          (
            "Probe(error) | Override(error) | TooOld(required,found,home) | " +
              "Absent(required) | NoUserHome() | NoArchitecture() | Denied() | " +
              "Config(error) | Io(failure) | Rejected(path,reason)"
          ),
          """{"NoUserHome":{}}""",
          histos.jdk.ToolchainError.NoUserHome,
        ) &&
        mixed[histos.zinc.ToolchainMiss](
          "ToolchainMiss",
          (
            "Fetch(version) | Missing(version,part) | Duplicated(version,part) | " +
              "Unexpected(version,jars) | Library(requested,found) | Plugin(version) | " +
              "Coverage(version)"
          ),
          """{"Plugin":{"version":"3.10.0"}}""",
          histos.zinc.ToolchainMiss.Plugin(scala310),
        ) &&
        mixed[histos.central.TransportFault](
          "TransportFault",
          (
            "Target(raw) | Connect(host,port) | ConnectTimeout(host,port) | " +
              "Tls(host,port) | ReadTimeout(host,port) | Io(host,port) | " +
              "Protocol(host,port) | Pool(host,port) | Trust() | Body()"
          ),
          """{"Trust":{}}""",
          histos.central.TransportFault.Trust,
        ) &&
        mixed[histos.version.VersionError](
          "VersionError",
          (
            "Session(error) | Registry(error) | UnknownShip(name) | " +
              "Refused(refusals) | Unreadable() | Config(error)"
          ),
          """{"Unreadable":{}}""",
          histos.version.VersionError.Unreadable,
        )
    )

  private def sum13: TestResult =
    (
      mixed[histos.watch.WatchError](
        "WatchError",
        "Read(path)",
        """{"Read":{"path":"/work/repo"}}""",
        histos.watch.WatchError.Read(home),
      ) &&
        mixed[histos.report.WorkflowFault](
          "WorkflowFault",
          "Files(error) | Scripts(error)",
          """{"Files":{"error":{"NoDefaultBranch":{}}}}""",
          WorkflowFault.Files(histos.workflow.CiError.NoDefaultBranch),
        )
    )

  private def sumLaws: TestResult =
    sum1 &&
      sum2 &&
      sum3 &&
      sum4 &&
      sum5 &&
      sum6 &&
      sum7 &&
      sum8 &&
      sum9 &&
      sum10 &&
      sum11 &&
      sum12 &&
      sum13

  private def pathLaw: TestResult =
    val schema = summon[Schema[Path]]
    assertTrue(schema.doc == SchemaDoc.Str(None), schema.named == Some("Path")) &&
    accepted[Path]("\"/work/repo\"", home) &&
    rejected[Path]("1", stringNumber) &&
    rejected[Path]("\"\\u0000\"", "(invalid path \u0000)")

  private def takenLaws: TestResult =
    taken(histos.testrun.SuitePattern.from("App*")) { pattern =>
      text[histos.testrun.SuitePattern]("SuitePattern", "App*", pattern, "", "a test pattern is not empty")
    } &&
      taken(histos.native.ClangVersion.from("21.0.0")) { version =>
        text[histos.native.ClangVersion]("ClangVersion", "21.0.0", version, "nope", "not a clang version: nope")
      } &&
      taken(histos.native.ClangTarget.from("arm64-apple-darwin")) { target =>
        text[histos.native.ClangTarget](
          "ClangTarget",
          "arm64-apple-darwin",
          target,
          "",
          "clang --version did not report a target",
        )
      }

  private def taken[E, A](read: Either[E, A])(use: A => TestResult): TestResult =
    read match
      case Right(value) => use(value)
      case Left(error)  => assertTrue(s"$error" == "the parser rejected a sample")

  private def plainLaws: TestResult =
    plain[histos.load.CompilerMessage](
      "CompilerMessage",
      histos.load.CompilerMessage.fromCompiler(""),
      histos.load.CompilerMessage.fromCompiler("word"),
    ) &&
      plain[histos.load.BinaryName](
        "BinaryName",
        histos.load.BinaryName.trusted(""),
        histos.load.BinaryName.trusted("word"),
      ) &&
      plain[histos.zinc.CompileDiagnostic](
        "CompileDiagnostic",
        histos.zinc.CompileDiagnostic.fromCompiler(""),
        histos.zinc.CompileDiagnostic.fromCompiler("word"),
      ) &&
      plain[histos.plugin.PluginDetail](
        "PluginDetail",
        histos.plugin.PluginDetail.trusted(""),
        histos.plugin.PluginDetail.trusted("word"),
      ) &&
      plain[histos.process.ProcessOutput](
        "ProcessOutput",
        histos.process.ProcessOutput.from(""),
        histos.process.ProcessOutput.from("word"),
      )

  private def majorLaws: TestResult =
    major[histos.jdk.JdkMajor]("JdkMajor", "21", histos.jdk.JdkMajor.trusted(21), "0 is not a JDK major") &&
      major[histos.ClangMajor]("ClangMajor", "21", histos.ClangMajor(21), "0 is not a clang major") &&
      major[histos.node.NodeMajor]("NodeMajor", "22", histos.node.NodeMajor.trusted(22), "0 is not a Node major")

  private def actionLaw: TestResult =
    taken(histos.schedule.ActionId.from("compile")) { id =>
      text[histos.schedule.ActionId]("ActionId", "compile", id, "", "an action id is not empty") &&
      mixed[histos.schedule.ScheduleError[histos.load.PluginFault]](
        "ScheduleError",
        scheduleOutline,
        """{"UnknownAction":{"id":"compile"}}""",
        histos.schedule.ScheduleError.UnknownAction(id),
      )
    }

  private def productLaws: TestResult =
    products1 && products2 && products3

  private def products1: TestResult =
    (
      product[histos.testrun.TestedCell](
        "TestedCell",
        List("module", "version", "platform", "status"),
        List("module", "version", "platform", "status"),
        """{"module":"core","version":"3.10.0","platform":"jvm","status":{"NoTests":{}}}""",
        histos.testrun.TestedCell(core, scala310, histos.Platform.Jvm, histos.testrun.TestStatus.NoTests),
      ) &&
        product[histos.workspace.IoFailure](
          "IoFailure",
          List("path", "fault"),
          List("path", "fault"),
          """{"path":"/work/repo","fault":"Read"}""",
          histos.workspace.IoFailure(home, histos.workspace.IoFault.Read),
        ) &&
        product[histos.SourcePos](
          "SourcePos",
          List("path", "line", "column"),
          List("path", "line", "column"),
          """{"path":"Histos.scala","line":4,"column":2}""",
          histos.SourcePos("Histos.scala", 4, 2),
        ) &&
        product[histos.resolve.ToolId](
          "ToolId",
          List("role", "scalaVersion", "variant"),
          List("role", "scalaVersion", "variant"),
          """{"role":"compiler","scalaVersion":"3.10.0","variant":"jvm"}""",
          histos.resolve.ToolId(histos.resolve.ToolRole.Compiler, scala310, histos.resolve.ToolVariant.Jvm),
        ) &&
        product[histos.resolve.ToolRoot](
          "ToolRoot",
          List("organization", "name", "revision", "transitivity", "origin"),
          List("organization", "name", "revision", "transitivity"),
          """{"organization":"dev.zio","name":"zio","revision":"2.1.27","transitivity":"closure"}""",
          histos.resolve.ToolRoot("dev.zio", "zio", "2.1.27", histos.resolve.Transitivity.Closure),
        ) &&
        product[histos.node.NodeBinary](
          "NodeBinary",
          List("path"),
          List("path"),
          """{"path":"/work/repo"}""",
          histos.node.NodeBinary(home),
        ) &&
        product[histos.ProviderLibrary](
          "ProviderLibrary",
          List("organization", "name"),
          List("organization", "name"),
          """{"organization":"dev.zio","name":"zio-test"}""",
          histos.ProviderLibrary("dev.zio", "zio-test"),
        ) &&
        product[histos.zinc.SourceCount](
          "SourceCount",
          List("compiled", "skipped"),
          List("compiled", "skipped"),
          """{"compiled":3,"skipped":1}""",
          histos.zinc.SourceCount(3, 1),
        )
    )

  private def products2: TestResult =
    (
      product[histos.jdk.JdkSurvey](
        "JdkSurvey",
        List("home", "version", "identity"),
        List("home", "version", "identity"),
        jdkJson,
        jdkSurvey,
      ) &&
        product[histos.node.NodeSurvey](
          "NodeSurvey",
          List("binary", "version", "identity"),
          List("binary", "version", "identity"),
          nodeJson,
          nodeSurvey,
        ) &&
        product[histos.toolchain.Reached](
          "Reached",
          List("jdk", "node"),
          List("jdk"),
          "{\"jdk\":" + jdkJson + "}",
          histos.toolchain.Reached(jdkSurvey, None),
        ) &&
        product[histos.zinc.CompileProblem](
          "CompileProblem",
          List("message", "path", "severity", "startLine", "startColumn", "endLine", "endColumn", "base"),
          List("message", "severity", "base"),
          """{"message":"boom","severity":"Error","base":"Zinc"}""",
          histos.zinc.CompileProblem(
            "boom",
            None,
            histos.zinc.CompileSeverity.Error,
            None,
            None,
            None,
            None,
            histos.zinc.PositionBase.Zinc,
          ),
        ) &&
        product[histos.zinc.CompiledCell](
          "CompiledCell",
          List(
            "module",
            "version",
            "platform",
            "classes",
            "classFiles",
            "key",
            "changed",
            "sources",
            "classesWritten",
          ),
          List("module", "version", "platform", "classes", "classFiles", "changed"),
          "{\"module\":\"core\",\"version\":\"3.10.0\",\"platform\":\"jvm\"," +
            "\"classes\":\"/work/repo\",\"classFiles\":1,\"changed\":true}",
          histos.zinc.CompiledCell(core, scala310, histos.Platform.Jvm, home, 1, None, true, None, None),
        )
    )

  private def products3: TestResult =
    (
      product[ModuleTally](
        "ModuleTally",
        List("module", "version", "platform", "compiled", "skipped", "classes"),
        List("module", "version", "platform"),
        """{"module":"core","version":"3.10.0","platform":"jvm"}""",
        tally,
      ) &&
        product[Observation](
          "Observation",
          List("modules", "compiled", "skipped", "classes", "moduleCount"),
          List("modules", "moduleCount"),
          """{"modules":[{"module":"core","version":"3.10.0","platform":"jvm"}],"moduleCount":1}""",
          seen,
        ) &&
        product[Diagnostic](
          "Diagnostic",
          List("path", "line", "column", "endLine", "endColumn", "severity", "message"),
          List("severity", "message"),
          """{"path":"Histos.scala","severity":"error","message":"boom"}""",
          Diagnostic(Some("Histos.scala"), None, None, None, None, Severity.Error, "boom"),
        ) &&
        product[Report](
          "Report",
          List("out", "err", "run", "observation", "diagnostics", "tests", "program"),
          List("out", "err", "diagnostics", "tests"),
          """{"out":[],"err":[],"diagnostics":[],"tests":[]}""",
          Report.empty,
        )
    )

  private def collections: TestResult =
    val strings = summon[Schema[NonEmptyChunk[String]]]
    val sets    = summon[Schema[Set[String]]]
    val cons    = summon[Schema[::[String]]]
    val nested  = summon[Schema[NonEmptyChunk[histos.zinc.GraphCompileError]]]
    val docs    = assertTrue(
      strings.doc == SchemaDoc.Array(SchemaDoc.Str(None)),
      strings.named == None,
      sets.doc == SchemaDoc.Array(SchemaDoc.Str(None)),
      sets.named == None,
      cons.doc == SchemaDoc.Array(SchemaDoc.Str(None)),
      cons.named == None,
      nested.doc == SchemaDoc.Array(SchemaDoc.Ref("GraphCompileError")),
      nested.named == None,
    )
    docs &&
    accepted[NonEmptyChunk[String]]("""["a"]""", NonEmptyChunk("a")) &&
    rejected[NonEmptyChunk[String]]("[]", "(empty chunk)") &&
    rejected[NonEmptyChunk[String]]("1", arrayNumber) &&
    accepted[::[String]]("""["a"]""", ::("a", Nil)) &&
    rejected[::[String]]("[]", "(empty list)") &&
    rejected[::[String]]("1", arrayNumber) &&
    accepted[NonEmptyChunk[histos.zinc.GraphCompileError]](
      """[{"NoRoot":{"module":"core"}}]""",
      NonEmptyChunk(histos.zinc.GraphCompileError.NoRoot(core)),
    ) &&
    rejected[NonEmptyChunk[histos.zinc.GraphCompileError]]("[]", "(empty chunk)") &&
    rejected[NonEmptyChunk[histos.zinc.GraphCompileError]]("1", arrayNumber)

  private def configLaw: TestResult =
    val schema = summon[Schema[Config.Error]]
    val named  = assertTrue(schema.named == Some("ConfigError"), arms(schema.doc).mkString(" | ") == configOutline)
    val cause  = wireCause(schema.doc) match
      case Some(doc) => assertTrue(arms(doc).mkString(" | ") == causeOutline)
      case None      => assertTrue("missing" == "WireCause")
    named && cause &&
    accepted[Config.Error](
      """{"MissingData":{"path":[],"message":"gone"}}""",
      Config.Error.MissingData(Chunk.empty, "gone"),
    ) &&
    rejected[Config.Error]("1", objectNumber)

  private def providerLaw: TestResult =
    val schema = summon[Schema[histos.TestProvider]]
    val shape  = schema.doc match
      case SchemaDoc.Object(Some(name), params, needed) =>
        assertTrue(
          name == "TestProvider",
          schema.named == Some("TestProvider"),
          params.map(_.name) == List("framework", "library"),
          needed == List("framework", "library"),
        )
      case other => assertTrue(other.toString == "object TestProvider")
    val library = libraryDoc(schema.doc) match
      case Some(SchemaDoc.Object(_, params, _)) => assertTrue(params.map(_.name) == List("organization", "name"))
      case other                                => assertTrue(s"$other" == "library object")
    shape && library &&
    accepted[histos.TestProvider](
      """{"framework":"zio.test.sbt.ZTestFramework","organization":"dev.zio","name":"zio-test-sbt"}""",
      histos.TestProvider.Zio,
    ) &&
    rejected[histos.TestProvider]("1", objectNumber)

  def spec = suite("wire schema laws")(
    test("a text document names its type and rejects a number or a bad word"):
      textLaws && plainLaws && pathLaw && takenLaws
    ,
    test("a token document lists its words and rejects the case name"):
      tokenLaws
    ,
    test("a major rejects zero"):
      majorLaws
    ,
    test("a pure enumeration is the case names"):
      pureLaws
    ,
    test("a sum lists every case"):
      sumLaws && actionLaw
    ,
    test("a product lists every field"):
      productLaws
    ,
    test("collections, config, and the test provider keep their documents"):
      collections && configLaw && providerLaw,
  )
end WireSchemaLawSpec
