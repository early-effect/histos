package histos

import histos.central.MetadataFault
import histos.doctor.{Diagnostic, Fix}
import histos.node.NodeFileFault
import histos.zinc.JsLibraryMiss

import zio.test.*

import CodecSamples.*

/** The sentence is the contract. A wrong arm or a dropped field fails. */
object MessageLawSpec extends ZIOSpecDefault:
  private val digest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  private val other  = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdee"
  private val root   =
    "'core' has no source root. Expected core/src, or src when it is the only module"
  private val missing   = "dev.zio:zio:0 was not found"
  private val sources   = "compile has no sources"
  private val read      = "could not read /work/repo"
  private val unchecked =
    "the release registry was not checked, so nothing publishes. Create releases (an empty file is fine) or add a line: <ship> matches, <ship> moved, or <ship> unknown."
  private val unreadable =
    "could not read releases. Create releases (an empty file is fine) or add a line: <ship> matches, <ship> moved, or <ship> unknown."
  private val develop =
    "publishing histos 1.2.3 needs export HISTOS_SESSION=release. Then run histos run versions."
  private val noCommit =
    "could not read the commit at HEAD. A snapshot coordinate is that commit."

  def spec = suite("message laws")(
    test("every select error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(selectErrors),
        selectErrors.map(_.message) == List(
          "'core' is listed twice",
          "'core' is not a project",
        ),
        selectErrors.map(error => noted(error.diagnose)) == List(
          ("project.duplicate", "histos graph"),
          ("project.unknown", "histos compile api"),
        ),
      )
    ,
    test("every child error names its sentence"):
      assertTrue(
        EnumLaw.covered(childErrors),
        childErrors.map(_.message) == List(
          "a jvm child is required, not native",
          "no java executable at /work/repo",
          "no class directory at /work/repo",
          "no classpath artifact at /work/repo",
          "work directory is not an empty directory: /work/repo",
          "process group failed while lookup",
          "child bootstrap failed: out",
          "a child needs a program",
          "could not start java",
          "could not read system property user.dir",
          "the child did not report a status",
          "could not read the child's output",
          read,
        ),
      )
    ,
    test("every plugin load error names its sentence"):
      assertTrue(
        EnumLaw.covered(pluginLoads),
        pluginLoads.map(_.message) == List(
          "plugin demo is not in the lock",
          "plugin demo does not contain META-INF/histos/version",
          s"plugin dev.zio:zio:2.1.27 was built against api $digest and this process loads $other",
          "plugin dev.zio:zio:2.1.27 uses LinkStrategy.Api, which is refused",
          "plugin dev.zio:zio:2.1.27 needs sources, and this loader does not have them",
          "'scala.None' is in a reserved package",
          "plugin dev.zio:zio:2.1.27 links 'histos.engine.X'",
          "the build links 'histos.build.X', and no plugin class asked for it",
          "plugin demo loaded as other",
          "plugin dev.zio:zio:2.1.27 failed while loading (boom)",
          "plugin read failed at /work/repo",
          "plugin construction failed (broke)",
        ),
      )
    ,
    test("every plugin run error names its sentence"):
      assertTrue(
        EnumLaw.covered(pluginRuns),
        pluginRuns.map(_.message) == List(
          "broke",
          "plugin output out was not written",
          "plugin wrote 'extra', which was not declared",
          "plugin argument out is missing",
          "module core has no note k",
          "plugin task demo/site is not in the graph",
          "plugin task cycle demo/site",
          "plugin task failed (boom)",
          "plugin task links 'histos.engine.X'",
          "plugin task is missing class missing.Class",
        ),
      )
    ,
    test("every plugin task error names its sentence"):
      assertTrue(
        EnumLaw.covered(pluginTasks),
        pluginTasks.map(_.message) == List(
          "'nope' is not a plugin task id",
          "a plugin id is not empty",
          "a task name is not empty",
        ),
        pluginTaskExtras.map(_.message) == List(
          "'1' is not a plugin id",
          "'1' is not a task name",
        ),
      )
    ,
    test("every resolve error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(resolveErrors),
        resolveErrors.map(_.message) == List(
          "tool compiler 3.10.0 jvm has missing root dev.zio:zio:2.1.27",
          "tool compiler 3.10.0 jvm is missing from the dependency lock",
          "tool compiler 3.10.0 jvm does not match the dependency lock",
          "'3' is not a full Scala version (major.minor.patch)",
          "HISTOS_NATIVE is empty",
          "dev.zio:zio:2.1.27 is not an exact revision",
          missing,
          "checksum of /work/repo does not match the lock",
          "cell core 3.10.0 jvm does not match the dependency lock",
          "dev.zio:zio:2.1.27 does not match the dependency lock",
          "no dependency lock at /work/repo",
          "resolve does not fetch js",
          "dependency lock is empty",
          read,
          "plugin x is not in the lock",
          "resolve failed while fetch",
          "could not download:\nhttps://repo1.maven.org/x.jar: download error: read timed out",
          "/work/repo has no top-level version line",
        ),
        resolveErrors.map(error => noted(error.diagnose)) == List(
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.tool-drift", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.not-found", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.lock-drift", "histos resolve --update"),
          ("resolve.plugin-drift", "histos resolve --update"),
          ("resolve.no-lock", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.failed", "histos resolve --update"),
          ("resolve.download", "histos resolve"),
          ("format.config", "histos resolve --update"),
        ),
      )
    ,
    test("every lock error names its sentence"):
      assertTrue(
        EnumLaw.covered(lockErrors),
        lockErrors.map(_.message) == List(
          "dependency lock is empty",
          "dependency lock line is not a field: nope",
          "dependency lock repeats test core 3.10.0 jvm",
          "dependency lock repeats tool compiler 3.10.0 jvm",
          "dependency lock repeats plugin dev.zio zio 2",
          "dependency lock is missing cell",
          "a jar classifier is not empty",
          "dependency lock sha256 is not 64 hex digits: zz",
          "dependency lock repository is not central, ivy-local, or a maven url: nope",
        ),
      )
    ,
    test("every toolchain miss names its sentence"):
      assertTrue(
        EnumLaw.covered(toolchainMisses),
        toolchainMisses.map(_.message) == List(
          "the 3.10.0 compiler could not be fetched",
          "the 3.10.0 fetch has no scala-library",
          "the 3.10.0 fetch has more than one scala-compiler",
          "the 3.10.0 fetch included repo",
          "the 3.10.0 fetch brought Scala library 2.13.16",
          "semanticdb-scalac for 3.10.0 could not be fetched",
          "scoverage for 3.10.0 could not be selected",
        ),
      )
    ,
    test("every native fetch names its sentence"):
      assertTrue(
        EnumLaw.covered(nativeFetches),
        nativeFetches.map(_.message) == List(
          sources,
          missing,
          "the Scala Native toolchain has no nativelib 0.5.9",
          "the Scala Native toolchain has more than one nativelib",
        ),
      )
    ,
    test("every compile error names its sentence"):
      assertTrue(
        EnumLaw.covered(compileErrors),
        compileErrors.map(_.message) == List(
          missing,
          "compile does not support Native",
          "compile supports Scala 3 and Scala 2.13, not 2.13.16",
          sources,
          "/work/repo is outside /other",
          "/work/repo is histos and is not a user dependency",
          "boom",
          "the 3.10.0 compiler could not be fetched",
          read,
          "the Scala.js library resolve was empty",
          "the Scala Native toolchain has no nativelib 0.5.9",
          "HISTOS_NATIVE is empty",
          "missing",
          read,
        ),
      )
    ,
    test("every graph compile error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(graphErrors),
        graphErrors.map(_.message) == List(
          "the process environment could not be read",
          "'core' is listed twice",
          "'core' 3.10.0 is not a cell on the requested platform",
          root,
          "'core' has no main or test sources under /work/repo",
          "'core' depends on 'api', which has no 3.10.0 jvm classes",
          "'core' 3.10.0 cannot read 'api' 2.13.16 js",
          "'core' is in a compile cycle",
          missing,
          read,
          "could not read source roots under /work/repo",
          "could not write /work/repo",
          s"'core' 3.10.0 jvm failed to compile\n$sources",
          "'core' 3.10.0 jvm could not copy resources\nresource 'Demo.class' is already a class file",
          root,
        ),
        graphErrors.map(error => noted(error.diagnose)) == List(
          ("compile.failed", "histos help compile"),
          ("project.duplicate", "histos graph"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("resolve.not-found", "histos resolve --update"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
          ("compile.failed", "histos help compile"),
        ),
      )
    ,
    test("every ci fault names its sentence"):
      assertTrue(
        EnumLaw.covered(ciFaults),
        ciFaults.map(_.message) == List(
          "job docs does not render one site",
          "job docs needs release, which is not a job on this graph",
          "job docs needs release twice",
          "job docs names module core, which is not a module",
          "job docs names target site twice",
          "job docs uses actions/checkout, which has no pin",
          "job docs uses actions/checkout without its grant",
          "job docs names runner ubuntu, which is not in the catalog",
          "secret HISTOS_TOKEN is not declared",
          "job docs has a condition its events do not reach",
          "workflow_dispatch input name is declared twice with different types",
          "jobs in ci.yml do not share a concurrency group",
          "workflow build is not rendered by this graph",
        ),
      )
    ,
    test("every plugin graph fault names its sentence"):
      assertTrue(
        EnumLaw.covered(pluginGraphFaults),
        pluginGraphFaults.map(_.message) == List(
          "plugin demo is registered twice",
          "plugin demo declares task site twice",
          "command site is declared twice",
          "job docs is declared twice",
          "job docs is reserved",
          "job docs and job release both name a site",
          "module core repeats note demo/k",
          "module core has no note demo/k",
          "plugin demo names 'core', which is not a module",
          "plugin demo names Scala 3.10.0 on jvm of 'core', and 'core' does not build it",
          "plugin demo does not declare task site",
          "command site is a built-in",
          "plugin demo failed while contributing (boom)",
          "'missing' is not a module in this graph (1:1)",
          "'core' is an aggregate, and a plugin coordinate names a module (1:1)",
          "plugin core is named twice (1:1)",
        ),
      )
    ,
    test("every build error names its sentence"):
      assertTrue(
        EnumLaw.covered(buildErrors),
        buildErrors.map(_.message) == List(
          "a project name is not empty (1:1)",
          "'1core' is not a project name (1:1)",
          "project 'core' is declared twice (2:3)",
          "project 'core' lists jvm twice",
          "project 'core' lists Scala 3.10.0 twice",
          "aggregate 'core' lists 'api' twice",
          "aggregate 'core' includes itself",
          "aggregate 'core' lists 'api', which is not a module",
          "'missing' is referenced but not passed to Projects (1:1)",
          "'core' depends on aggregate 'api'",
          "'core' depends on itself",
          "'core' depends on 'api' twice",
          "'core' depends on js and does not build it",
          "'core' depends on 'api' on native, and 'api' does not build it",
          "dependency cycle on jvm core -> api",
          "project 'core' targets Native and does not name a clang major (1:1)",
          "a ship name is not empty (1:1)",
          "'1ship' is not a ship name (1:1)",
          "ship 'histos' is declared twice (2:3)",
          "ship 'histos' lists 'core' twice (1:1)",
          "ship 'histos' lists aggregate 'core' (1:1)",
          "'core' is in ship 'histos' and ship 'other' (1:1)",
          "repository central is listed twice",
          "'core' names a test main and a test provider (1:1)",
          "'core' and 'api' both use directory modules",
          "plugin demo names Scala 3.10.0 on jvm of 'core', and 'core' does not build it",
          "job docs does not render one site",
          "'core' publishes two artifacts (1:1)",
          "'core' publishes an assembly, which is one jvm jar (1:1)",
          "'core' names an asset and does not publish an assembly (1:1)",
          "'core' names two assets (1:1)",
          "'core' names resource 'note.txt' twice (1:1)",
          "'core' stores remote test results and needs docker, network (1:1)",
        ),
      )
    ,
    test("every load error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(loadErrors),
        loadErrors.map(_.message) == List(
          root,
          "no build file at /work/repo",
          "boom",
          "histos.scala does not define one object that extends HistosBuild",
          "histos.scala defines more than one HistosBuild: demo.Build$",
          "a project name is not empty (1:1)",
          "plugin x is not in the lock",
          missing,
          "/work/repo has no bootstrap api jar",
          "no user home, so the api jar cannot be cached",
          read,
          "/work/repo has nothing to jar",
        ),
        loadErrors.map(error => noted(error.diagnose)) == List(
          ("load.failed", "histos help"),
          ("load.missing", "histos init"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
          ("resolve.not-found", "histos resolve --update"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
          ("load.failed", "histos help"),
        ),
      )
    ,
    test("every cache error names its sentence"):
      assertTrue(
        EnumLaw.covered(cacheErrors),
        cacheErrors.map(_.message) == List(
          read,
          s"cache entry $digest failed verification",
        ),
      )
    ,
    test("every cache exchange names its sentence"):
      assertTrue(
        EnumLaw.covered(exchanges),
        exchanges.map(_.message) == List(
          "the remote cache target is not a url",
          "could not connect to the remote cache at repo.example:443",
          "the remote cache at repo.example:443 did not accept a connection",
          "tls failed with the remote cache at repo.example:443",
          "the remote cache at repo.example:443 stopped answering",
          "the remote cache at repo.example:443 closed the exchange",
          "the remote cache at repo.example:443 sent an invalid response",
          "no free connection to the remote cache at repo.example:443",
          "the remote cache trust material is unusable",
          "the remote cache response body failed",
        ),
      )
    ,
    test("every namespace error names its sentence"):
      assertTrue(
        EnumLaw.covered(namespaceErrors),
        namespaceErrors.map(_.message) == List(
          "'other' is not a cache namespace. A namespace is main, pr/<n>, or dev/<name>.",
          "0 is not a positive pull request number",
          "'' is not a dev cache name. A name is one segment of letters, digits, '-' or '_'.",
        ),
      )
    ,
    test("every remote error names its sentence"):
      assertTrue(
        EnumLaw.covered(remoteErrors),
        remoteErrors.map(_.message) == List(
          "remote cache is not configured",
          "remote cache is read-only",
          s"remote cache action $digest names missing output $other",
          s"remote cache output for $digest is not utf-8",
          "the remote cache response body failed",
          "remote cache answered 500",
          s"remote cache action $digest is not an action result",
          s"remote cache action $digest returned bytes that are not $other",
          "'not a line' is not an action digest",
          "'ftp' is not a remote cache scheme. A scheme is http, https, grpc, or grpcs.",
          "'nope' is not a remote cache mode. A mode is read or write.",
          "'nope' is not a remote cache address.",
          "'other' is not a cache namespace. A namespace is main, pr/<n>, or dev/<name>.",
          "remote cache answered INTERNAL",
          "put is an http cache operation",
        ),
      )
    ,
    test("every jdk error names its sentence"):
      assertTrue(
        EnumLaw.covered(jdkErrors),
        jdkErrors.map(_.message) == List(
          "0 is not a JDK major",
          read,
        ),
      )
    ,
    test("every release error names its sentence"):
      assertTrue(
        EnumLaw.covered(releaseErrors),
        releaseErrors.map(_.message) == List(
          "no release file",
          "release file has no JAVA_VERSION",
          "release file has an unclosed JAVA_VERSION",
          "release file has a bad JAVA_VERSION: nope",
        ),
      )
    ,
    test("every jdk reject names its sentence"):
      assertTrue(
        EnumLaw.covered(rejects),
        rejects.map(_.message) == List(
          "not a JDK home",
          "no javac",
          "no release file",
        ),
      )
    ,
    test("every toolchain override names its sentence"):
      assertTrue(
        EnumLaw.covered(overrideErrors),
        overrideErrors.map(_.message) == List(
          "toolchain.local is empty",
          "toolchain.local has more than one path",
          "toolchain.local path is not absolute: jdk",
          "toolchain.local path is invalid: ::",
        ),
      )
    ,
    test("every toolchain error names its sentence"):
      assertTrue(
        EnumLaw.covered(toolchainErrors),
        toolchainErrors.map(_.message) == List(
          "0 is not a JDK major",
          "toolchain.local is empty",
          "JDK 17.0.2 at /work/repo is older than 21.0.2",
          "no JDK found; Histos needs 21.0.2 or newer",
          "user.home is not set",
          "os.arch is not set",
          "the process environment could not be read",
          "configuration: jdk bad",
          read,
          "JDK at /work/repo was rejected: not a JDK home",
        ),
      )
    ,
    test("every node version error names its sentence"):
      assertTrue(
        EnumLaw.covered(nodeVersionErrors),
        nodeVersionErrors.map(_.message) == List(
          "not a Node version: nope",
          "node --version exited 1",
          "node binary is missing",
          "node --version did not report a status",
          "a child needs a program",
          "/work/repo is not executable",
        ),
      )
    ,
    test("every node override names its sentence"):
      assertTrue(
        EnumLaw.covered(nodeOverrideErrors),
        nodeOverrideErrors.map(_.message) == List(
          "node.local is empty",
          "node.local has more than one path",
          "node.local path is not absolute: node",
          "node.local path is invalid: ::",
        ),
      )
    ,
    test("every node reject names its sentence"):
      assertTrue(
        EnumLaw.covered(nodeRejects),
        nodeRejects.map(_.message) == List(
          "not a Node home",
          "no node",
          "node binary is missing",
        ),
      )
    ,
    test("every node path error names its sentence"):
      assertTrue(
        EnumLaw.covered(nodePathErrors),
        nodePathErrors.map(_.message) == List(
          "is empty",
          "is not absolute: node",
          "is not a node binary: /bin/sh",
          "is not a path: ::",
        ),
      )
    ,
    test("every node error names its sentence"):
      assertTrue(
        EnumLaw.covered(nodeErrors),
        nodeErrors.map(_.message) == List(
          "0 is not a Node major",
          read,
          "invalid runtime search path: ::",
          "could not read the Node version at /work/repo: not a Node version: nope",
          "node.local is empty",
          read,
        ),
      )
    ,
    test("every node miss names its sentence"):
      assertTrue(
        EnumLaw.covered(nodeMisses),
        nodeMisses.map(_.message) == List(
          "0 is not a Node major",
          "Node from HISTOS_NODE at /work/repo was rejected: not a Node home",
          "Node 20.0.0 from PATH at /work/repo is older than 22.14.0",
          "no Node on config; Histos needs 22.14.0 or newer",
          "node.local names /work/repo, which is missing",
          "config is empty",
        ),
      )
    ,
    test("every runtime location error names its sentence"):
      assertTrue(
        EnumLaw.covered(locationErrors),
        locationErrors.map(_.message) == List(
          "invalid runtime search path: ::",
          read,
        ),
      )
    ,
    test("every clang text names its sentence"):
      assertTrue(
        EnumLaw.covered(clangTexts),
        clangTexts.map(_.message) == List(
          "clang --version said nothing",
          "clang --version did not report a version",
          "not a clang version: nope",
          "clang --version did not report a target",
        ),
      )
    ,
    test("every clang miss names its sentence"):
      assertTrue(
        EnumLaw.covered(clangMisses),
        clangMisses.map(_.message) == List(
          "clang is not on PATH",
          "clang is not on PATH. Install clang 21",
          "'nope' is not a path",
          "clang++ is not beside /work/repo",
          read,
          "/work/repo --version exited 1",
          "/work/repo --version: not a clang version: nope",
          "clang 21.1.8 at /work/repo is not major 21. Install clang 21",
          "clang 21.1.8 and clang++ 20.1.0 are not the same compiler",
        ),
      )
    ,
    test("every native origin names its sentence"):
      assertTrue(
        EnumLaw.covered(nativeOrigins),
        nativeOrigins.map(_.message) == List(
          "HISTOS_NATIVE is empty",
          "'nope' is not HISTOS_NATIVE. Use ivy-local or file:/path",
          "'nope' puts a revision in HISTOS_NATIVE. Name nativeVersion on the module",
          "'nope' is not a repository url",
        ),
      )
    ,
    test("every survey error names its sentence"):
      assertTrue(
        EnumLaw.covered(surveyErrors),
        surveyErrors.map(_.message) == List(
          "the process environment could not be read",
          "no Node on config; Histos needs 22.14.0 or newer",
          "project 'core' targets Native and does not name a clang major",
          "projects 'core' and 'api' name different clang majors",
          "clang is not on PATH",
          "detail",
        ),
      )
    ,
    test("every link error names its sentence"):
      assertTrue(
        EnumLaw.covered(linkErrors),
        linkErrors.map(_.message) == List(
          "the native link was interrupted",
          "ld",
          "the native link failed (signal)",
        ),
      )
    ,
    test("every class shape error names its sentence"):
      assertTrue(
        EnumLaw.covered(shapeErrors),
        shapeErrors.map(_.message) == List(
          "could not read class files (nir)",
          "could not read class files under /work/repo",
          "could not read the class file bad",
        ),
      )
    ,
    test("every js link error names its sentence"):
      assertTrue(
        EnumLaw.covered(jsLinkErrors),
        jsLinkErrors.map(_.message) == List(
          missing,
          "the Scala.js linker could not be fetched",
          sources,
          sources,
          "link",
        ),
      )
    ,
    test("every js run error names its sentence"):
      assertTrue(
        EnumLaw.covered(jsRunErrors),
        jsRunErrors.map(_.message) == List(
          missing,
          "the JavaScript environment could not be fetched",
          sources,
          "could not write /work/repo",
        ),
      )
    ,
    test("every js suite error names its sentence"):
      assertTrue(
        EnumLaw.covered(jsSuiteErrors),
        jsSuiteErrors.map(_.message) == List(
          missing,
          "the Scala.js test adapter could not be fetched",
          sources,
          "boom",
        ),
      )
    ,
    test("every native error names its sentence"):
      assertTrue(
        EnumLaw.covered(nativeErrors),
        nativeErrors.map(_.message) == List(
          "clang is not on PATH",
          "the Scala Native toolchain has no nativelib 0.5.9",
          sources,
          "the native link was interrupted",
        ),
      )
    ,
    test("every native suite error names its sentence"):
      assertTrue(
        EnumLaw.covered(nativeSuiteErrors),
        nativeSuiteErrors.map(_.message) == List(
          missing,
          "the Scala Native test runner could not be fetched",
          "could not read class files (nir)",
          sources,
          "could not write /work/repo",
        ),
      )
    ,
    test("every platform tool error names its sentence"):
      assertTrue(
        EnumLaw.covered(platformTools),
        platformTools.map(_.message) == List(
          "the Scala.js linker could not be fetched",
          "the JavaScript environment could not be fetched",
          "the Scala.js test adapter could not be fetched",
          "the native link was interrupted",
          "the Scala Native test runner could not be fetched",
          "HISTOS_NATIVE is empty",
        ),
      )
    ,
    moduleTestsLaw,
    test("every program error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(programErrors),
        programErrors.map(_.message) == List(
          "the native link was interrupted",
          "'gone' is not a module",
          "'core' is an aggregate",
          "'core' does not name a main",
          "'core' builds jvm or js. Name one: histos run core jvm",
          "'core' does not build native. It builds jvm",
          "'core' 3.10.0 native does not name a clang major",
          "'1' is not a main class",
          root,
          sources,
          missing,
          read,
          "'core' 3.10.0 native failed to link\nld",
          "'core' 3.10.0 did not start\ndied",
          "'core' 3.10.0 js has no Node to run on: no Node on PATH; Histos needs 22.14.0 or newer",
          "'core' depends on 'api', which has no 3.10.0 classes",
          "could not prepare the run directory /work/repo",
          read,
        ),
        programErrors.map(error => noted(error.diagnose)) == List(
          ("run.failed", "histos help run"),
          ("run.absent", "histos run api"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("resolve.not-found", "histos resolve --update"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
        ),
      )
    ,
    test("every transport fault names its sentence"):
      assertTrue(
        EnumLaw.covered(transports),
        transports.map(_.message) == List(
          "could not reach the snapshot repository: 'nope' is not a target",
          "could not connect to the snapshot repository at repo.example:443",
          "the snapshot repository at repo.example:443 did not accept a connection",
          "tls failed with the snapshot repository at repo.example:443",
          "the snapshot repository at repo.example:443 stopped answering",
          "the snapshot repository at repo.example:443 closed the exchange",
          "the snapshot repository at repo.example:443 sent an invalid response",
          "no free connection to the snapshot repository at repo.example:443",
          "the snapshot repository trust material is unusable",
          "the snapshot repository response body failed",
        ),
      )
    ,
    test("every publish error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(publishErrors),
        publishErrors.map(_.message) == List(
          "a published jar is not empty",
          "a published jar is a zip",
          "/work/repo is not a directory",
          "/work/repo leaves the repository",
          "could not stat /work/repo",
          "sonatype rejected the credentials",
          "'1.2.3' is not a snapshot",
          "publish failed with HTTP 500",
          "the snapshot repository trust material is unusable",
          "snapshot metadata has no build number",
        ),
        publishErrors.map(error => noted(error.diagnose)) == List(
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.rejected", "Check the token at central.sonatype.com."),
          ("publish.failed", "histos help publish"),
          ("publish.status", "histos help publish"),
          ("publish.transport", "histos help publish"),
          ("publish.metadata", "histos help publish"),
        ),
      )
    ,
    test("every jar error names its sentence"):
      assertTrue(
        EnumLaw.covered(jarErrors),
        jarErrors.map(_.message) == List(
          "/work/repo is not a directory of classes",
          "/work/repo has nothing to jar",
          "'..' leaves the jar",
          "'A.class' is already a jar entry",
          "/work/repo is inside the classes directory",
          "jar failed while read /work/repo",
          "boom",
        ),
      )
    ,
    test("every lazy val frame names its sentence"):
      assertTrue(
        EnumLaw.covered(lazyFrames),
        lazyFrames.map(_.message) == List(
          "the bytecode was rejected",
          "demo.Exit.m ()V is 1 bytes",
          "demo.Exit has 1 constant pool entries",
          "an ASM limit was exceeded",
          "the frame merge was out of bounds",
        ),
      )
    ,
    test("every lazy val error names its sentence"):
      assertTrue(
        EnumLaw.covered(lazyErrors),
        lazyErrors.map(_.message) == List(
          "demo.Exit still calls Unsafe lazy vals from m",
          "demo.Exit has no VarHandle for demo.Exit.0",
          "demo.Exit cannot rewrite the offset load in m",
          "demo.Exit could not compute stack map frames: the bytecode was rejected",
        ),
      )
    ,
    test("every assembly error names its sentence"):
      assertTrue(
        EnumLaw.covered(assemblyErrors),
        assemblyErrors.map(_.message) == List(
          "/work/repo is not a directory of classes",
          "/work/repo has no class files",
          "'..' leaves the assembly",
          "/work/repo is inside the classes directory",
          "/work/repo is the assembly destination",
          "/work/repo is not a jar",
          "demo.Exit is not in the assembly",
          "an assembly needs a classes directory",
          "'A.class' is already in the assembly",
          "/work/repo is not a file",
          "demo.Exit still calls Unsafe lazy vals from m",
          "assembly failed while write(/work/repo)",
        ),
      )
    ,
    test("every local publish error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(localPublishes),
        localPublishes.map(_.message) == List(
          unreadable,
          "'core' is listed twice",
          root,
          "'core' 3.10.0 jvm failed to jar\n/work/repo has nothing to jar",
          s"'core' 3.10.0 jvm failed to read sources\n$sources",
          "'core' 3.10.0 jvm has no sources to publish",
          "a published jar is not empty",
          read,
          "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")",
          "nothing publishes",
          develop,
          noCommit,
          "ship 'histos' is not in the catalog",
          "'core' is not in a ship",
          "histos depends on snapshot dev.zio:zio:2.1.27-SNAPSHOT, so it is not published",
          "'nope' is not a ship, an aggregate, or a module",
          "'core' depends on 'api', which is not published",
          "a release needs a license",
          "dev.zio:zio:2 is not a publishable dependency",
          "SONATYPE_USERNAME and SONATYPE_PASSWORD are required",
          "could not write /work/repo",
          "broke",
          missing,
          sources,
          "'core' 3.10.0 jvm failed to assemble\nan assembly needs a classes directory",
          "histos builds clients on snapshots, and /work/repo is not a file. The snapshots workflow downloads each client there.",
        ),
        localPublishes.map(error => noted(error.diagnose)) == List(
          ("publish.failed", "histos help publish"),
          ("project.duplicate", "histos graph"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.credentials", "Export SONATYPE_USERNAME and SONATYPE_PASSWORD."),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("resolve.not-found", "histos resolve --update"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
          ("publish.failed", "histos help publish"),
        ),
      )
    ,
    test("every run miss names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(runMisses),
        runMisses.map(_.message) == List(
          "'gone' is not a module",
          "'core' is an aggregate",
          "'core' does not name a main",
          "'core' builds jvm, js. Name one.",
          "'core' does not build native. It builds jvm",
          "'nope' is not a task",
          "nope",
        ),
        runMisses.map(error => noted(error.diagnose)) == List(
          ("run.absent", "histos run api"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.failed", "histos help run"),
          ("run.unknown-task", "histos help run"),
          ("run.failed", "histos help run"),
        ),
      )
    ,
    test("every preview error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(previewErrors),
        previewErrors.map(_.message) == List(
          "'core' is listed twice",
          "'core' is in a compile cycle",
          read,
          unreadable,
          "nothing publishes",
          "'gone' is not a module",
          "'a b' is not a test pattern",
        ),
        previewErrors.map(error => noted(error.diagnose)) == List(
          ("project.duplicate", "histos graph"),
          ("preview.failed", "histos help preview"),
          ("preview.failed", "histos help preview"),
          ("preview.failed", "histos help preview"),
          ("publish.failed", "histos help publish"),
          ("run.absent", "histos run api"),
          ("preview.failed", "histos help preview"),
        ),
      )
    ,
    test("every watch error names its sentence"):
      assertTrue(
        EnumLaw.covered(watchErrors),
        watchErrors.map(_.message) == List(
          "could not read sources under /work/repo"
        ),
      )
    ,
    test("every commit sha error names its sentence"):
      assertTrue(
        EnumLaw.covered(commitErrors),
        commitErrors.map(_.message) == List(
          "a commit sha is not empty",
          "'zz' is not a 40-hex commit",
        ),
      )
    ,
    test("every branch error names its sentence"):
      assertTrue(
        EnumLaw.covered(branchErrors),
        branchErrors.map(_.message) == List(
          "'main' is not a branch. A branch is default or other."
        ),
      )
    ,
    test("every pull request error names its sentence"):
      assertTrue(
        EnumLaw.covered(pullErrors),
        pullErrors.map(_.message) == List(
          "'01' is not a pull request number",
          "0 is not a positive pull request number",
        ),
      )
    ,
    test("every session error names its sentence"):
      assertTrue(
        EnumLaw.covered(sessionErrors),
        sessionErrors.map(_.message) == List(
          "'nope' is not a session. Sessions are development, snapshot, pr, and release.",
          "a snapshot session needs HISTOS_SHA, a 40-hex commit",
          "a snapshot session needs HISTOS_BRANCH, default or other",
          "a pull request session needs HISTOS_PR, a positive number",
          "a commit sha is not empty",
          "'main' is not a branch. A branch is default or other.",
          "0 is not a positive pull request number",
        ),
      )
    ,
    test("every registry error names its sentence"):
      assertTrue(
        EnumLaw.covered(registryErrors),
        registryErrors.map(_.message) == List(
          "'x' is not a registry line. A line is '<ship> matches', '<ship> moved', or '<ship> unknown'.",
          "a ship name is not empty",
          "'nope' is not a tag relation. A relation is matches, moved, or unknown.",
          "ship 'histos' is listed twice in releases",
        ),
      )
    ,
    test("every version error names its sentence"):
      assertTrue(
        EnumLaw.covered(versionErrors),
        versionErrors.map(_.message) == List(
          "a snapshot session needs HISTOS_SHA, a 40-hex commit",
          "'x' is not a registry line. A line is '<ship> matches', '<ship> moved', or '<ship> unknown'.",
          "ship 'histos' is in releases and not in the catalog",
          unchecked,
          unreadable,
          "configuration: jdk bad",
        ),
      )
    ,
    test("every task error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(taskErrors),
        taskErrors.map(_.message) == List(
          "a task id is not empty",
          "no runner for 'inspect'",
          "no run 'run-1'",
          unreadable,
          "broke",
          "scaladoc",
        ),
        taskErrors.map(error => noted(error.diagnose)) == List(
          ("task.failed", "histos help run"),
          ("task.failed", "histos help run"),
          ("run.unknown", "histos help cancel"),
          ("task.failed", "histos help run"),
          ("task.failed", "histos help run"),
          ("task.failed", "histos help run"),
        ),
      )
    ,
    test("every ci error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(ciErrors),
        ciErrors.map(_.message) == List(
          "'1job' is not a project name",
          "'core' is not a project",
          "'core' is not a module",
          "'core' is listed twice",
          "no workflow at /work/repo",
          "ci.yml does not match the graph",
          read,
          "job docs and job release both name a site",
          "job docs needs release, which is not a job on this graph",
          "x",
          "the repository default branch is not set. refs/remotes/origin/HEAD is absent, and origin did not answer HEAD",
          "a job timeout is between 1 and 360 minutes, not 2",
          "no sha256 is pinned for clang.tar.gz",
          "ship histos builds clients on snapshots, and it is not the one asset ship with a native client and an LLVM pin",
        ),
        ciErrors.map(error => noted(error.diagnose)) == List(
          ("workflow.failed", "histos workflow --update"),
          ("workflow.unknown-project", "histos test api"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.missing", "histos workflow --update"),
          ("workflow.drift", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
          ("workflow.failed", "histos workflow --update"),
        ),
      )
    ,
    test("every script error names its sentence"):
      assertTrue(
        EnumLaw.covered(scriptErrors),
        scriptErrors.map(_.message) == List(
          "launcher script does not match the ship: /work/repo",
          "no launcher script at /work/repo",
          read,
          "the launcher script needs one ship version",
          "launcher properties are missing histos.version",
        ),
      )
    ,
    test("every assemble error names its sentence"):
      assertTrue(
        EnumLaw.covered(assembleErrors),
        assembleErrors.map(_.message) == List(
          sources,
          root,
          missing,
          "/work/repo has nothing to jar",
          "an assembly needs a classes directory",
          "'nope' is not a module",
          "'core' has no 3.10.0 classes",
          "could not write /work/repo",
        ),
      )
    ,
    test("every launcher error names its sentence"):
      assertTrue(
        EnumLaw.covered(launcherErrors),
        launcherErrors.map(_.message) == List(
          "launcher properties line is not a field: nope",
          "launcher properties field is unknown: nope",
          "launcher properties repeat histos.version",
          "launcher properties are missing histos.version",
          "launcher version is not a release, a snapshot pointer, or a timestamped revision: nope",
          "launcher --update follows snapshots, and nope is a release pin. Run 'histos update'.",
          "launcher repository is not a supported base: nope",
          "missing",
          read,
          "launcher path is not a readable regular file: /work/repo",
          "launcher pin is absent: /work/repo",
        ),
      )
    ,
    test("every schedule error names its sentence"):
      assertTrue(
        EnumLaw.covered(scheduleErrors),
        scheduleErrors.map(_.message) == List(
          "no action 'compile'",
          "action 'compile' is declared twice",
          "'compile' depends on 'link', which is not in the plan",
          "action cycle compile -> link",
          "'compile' declares input 'sources' twice",
          "'compile' failed: Task(Failed(broke))",
          read,
          "remote cache is not configured",
        ),
      )
    ,
    test("every plugin fault names its sentence"):
      assertTrue(
        EnumLaw.covered(pluginFaults),
        pluginFaults.map(_.message) == List(
          "broke",
          root,
          missing,
          "'core' was compiled and is not a module in the graph",
          "'link' failed: Tested(Absent(core))",
        ),
      )
    ,
    test("every project name error names its sentence"):
      assertTrue(
        EnumLaw.covered(nameErrors),
        nameErrors.map(_.message) == List(
          "a project name is not empty",
          "'1core' is not a project name",
        ),
      )
    ,
    test("every scala version error names its sentence"):
      assertTrue(
        EnumLaw.covered(scalaErrors),
        scalaErrors.map(_.message) == List(
          "a Scala version is not empty",
          "'3' is not a full Scala version (major.minor.patch)",
        ),
      )
    ,
    test("every ship name error names its sentence"):
      assertTrue(
        EnumLaw.covered(shipNameErrors),
        shipNameErrors.map(_.message) == List(
          "a ship name is not empty",
          "'1ship' is not a ship name",
        ),
      )
    ,
    test("every ship number error names its sentence"):
      assertTrue(
        EnumLaw.covered(shipNumberErrors),
        shipNumberErrors.map(_.message) == List(
          "a ship number is not empty",
          "'01.2.3' is not a ship number. A ship number is major.minor.patch with no leading zeros.",
        ),
      )
    ,
    test("every dependency error names its sentence and its check"):
      assertTrue(
        EnumLaw.covered(depErrors),
        depErrors.map(_.message) == List(
          "a dependency coordinate is not empty",
          "'nope' is not an organization:name:revision coordinate\nUse organization:name:revision, organization::name:revision, or organization:::name:revision.",
          "'org.example:demo:1.+' is not a single revision",
        ),
        depErrors.map(error => noted(error.diagnose)) == List(
          ("dep.empty", "histos help"),
          ("dep.shape", "histos help"),
          ("dep.ranged", "histos help"),
        ),
      )
    ,
    test("every parameter error names its sentence"):
      assertTrue(
        EnumLaw.covered(paramErrors),
        paramErrors.map(_.message) == List(
          "a parameter name is not empty",
          "'1' is not a parameter name",
        ),
      )
    ,
    test("every flag error names its sentence"):
      assertTrue(
        EnumLaw.covered(flagErrors),
        flagErrors.map(_.message) == List(
          "a flag name is not empty",
          "'1flag' is not a flag name",
        ),
      )
    ,
    test("every sha256 error names its sentence"):
      assertTrue(
        EnumLaw.covered(shaErrors),
        shaErrors.map(_.message) == List(
          "sha256 is not 64 hex digits: zz"
        ),
      )
    ,
    test("every suite pattern error names its sentence"):
      assertTrue(
        EnumLaw.covered(patternErrors),
        patternErrors.map(_.message) == List(
          "a test pattern is not empty",
          "'a b' is not a test pattern",
        ),
      )
    ,
    test("every classifier error names its sentence"):
      assertTrue(
        EnumLaw.covered(classifierErrors),
        classifierErrors.map(_.message) == List(
          "a jar classifier is not empty",
          "'has space' is not a jar classifier",
        ),
      )
    ,
    test("every repository url error names its sentence"):
      assertTrue(
        EnumLaw.covered(repoUrlErrors),
        repoUrlErrors.map(_.message) == List(
          "'nope' is not a repository url"
        ),
      )
    ,
    test("every packed main error names its sentence"):
      assertTrue(
        EnumLaw.covered(packMainErrors),
        packMainErrors.map(_.message) == List(
          "a main class is not empty",
          "'1' is not a main class",
        ),
      )
    ,
    test("every tag relation error names its sentence"):
      assertTrue(
        EnumLaw.covered(tagErrors),
        tagErrors.map(_.message) == List(
          "'Matches' is not a tag relation. A relation is matches, moved, or unknown."
        ),
      )
    ,
    test("every suite name error names its sentence"):
      assertTrue(
        EnumLaw.covered(suiteNameErrors),
        suiteNameErrors.map(_.message) == List(
          "a suite name is not empty"
        ),
      )
    ,
    test("every dev name error names its sentence"):
      assertTrue(
        EnumLaw.covered(devNameErrors),
        devNameErrors.map(_.message) == List(
          "'' is not a dev cache name. A name is one segment of letters, digits, '-' or '_'."
        ),
      )
    ,
    test("every snapshot revision error names its sentence"):
      assertTrue(
        EnumLaw.covered(snapshotErrors),
        snapshotErrors.map(_.message) == List(
          "'1.2.3' is not a snapshot"
        ),
      )
    ,
    test("every platform error names its sentence"):
      assertTrue(
        EnumLaw.covered(platformErrors),
        platformErrors.map(_.message) == List(
          "'Jvm' is not a platform (jvm, js, or native)"
        ),
      )
    ,
    test("every stamp error names its sentence"):
      assertTrue(
        EnumLaw.covered(stampErrors),
        stampErrors.map(_.message) == List(
          read,
          "could not read class /work/repo",
          "stamp index has no App",
        ),
      )
    ,
    test("every quick error names its sentence"):
      assertTrue(
        EnumLaw.covered(quickErrors),
        quickErrors.map(_.message) == List(
          read,
          read,
          read,
          "quick record /work/repo is not a stored success",
          "quick record /work/repo sha256 is not 64 hex digits: zz",
          "a suite name is not empty",
        ),
      )
    ,
    test("every js library miss names its sentence"):
      val samples = JsLibraryMiss.values.toList
      assertTrue(
        EnumLaw.covered(samples),
        samples.map(_.message) == List(
          "the Scala.js library could not be fetched",
          "the Scala.js library resolve was empty",
        ),
      )
    ,
    test("every node file fault names its sentence"):
      val samples = NodeFileFault.values.toList
      assertTrue(
        EnumLaw.covered(samples),
        samples.map(_.message) == List(
          "is missing",
          "is not executable",
          "is not a file",
        ),
      )
    ,
    test("every metadata fault names its sentence"):
      val samples = MetadataFault.values.toList
      assertTrue(
        EnumLaw.covered(samples),
        samples.map(_.message) == List(
          "snapshot metadata has no build number",
          "snapshot metadata build number is not a positive int",
          "snapshot metadata build number cannot increase",
          "snapshot metadata was not a complete body",
        ),
      ),
  )

  private val moduleTestsLaw: Spec[Any, Nothing] =
    test("every module test error names its sentence and its check"):
      val samples = moduleTests
      assertTrue(
        EnumLaw.covered(samples),
        samples.map(_.message) == List(
          "the JavaScript environment could not be fetched",
          "'core' is listed twice",
          root,
          missing,
          read,
          s"'core' 3.10.0 failed to compile tests\n$sources",
          "'core' names demo.TestMain and has no test sources",
          "'core' runs zio.test.ZTestFramework and has no test sources",
          "'core' runs zio.test.ZTestFramework and testDependencies do not include dev.zio::zio-test",
          "'core' ran zio.test.ZTestFramework and it found no tests",
          "no tests matched App*",
          "'core' names a test main and a test provider",
          "'core' 3.10.0 did not start\na child needs a program",
          "a main class is not empty",
          "'core' was compiled and is not a module in the graph",
          "'core' depends on 'api', which has no 3.10.0 classes",
          "could not prepare the test work directory /work/repo",
          "could not read test roots under /work/repo",
          "could not write the test provider runner at /work/repo",
          "'core' 3.10.0 js failed to link\nlink",
          "'core' 3.10.0 js has no Node to run on: no Node on config; Histos needs 22.14.0 or newer",
          "stamp index has no App",
          "quick record /work/repo is not a stored success",
          "a test pattern is not empty",
          "'core' 3.10.0 native has no readable API digest",
        ),
        samples.map(error => noted(error.diagnose)) == List(
          ("test.failed", "histos help test"),
          ("project.duplicate", "histos graph"),
          ("compile.failed", "histos help compile"),
          ("resolve.not-found", "histos resolve --update"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
          ("test.failed", "histos help test"),
        ),
      )

  /** Check id, then the text of the first fix. */
  private def noted(diagnostic: Diagnostic): (String, String) =
    (diagnostic.check.value, fixText(diagnostic.fixes))

  /** A `::` always has a head, so the first fix and the first step are fields, not lookups. */
  private def fixText(fixes: ::[Fix]): String =
    fixes.head match
      case Fix.Command(line)           => line.value
      case Fix.Histos(command)         => command.line
      case Fix.Settings(url, click, _) => s"${url.value}  $click"
      case Fix.Config(file, _)         => file.value
      case Fix.Steps(items)            => items.head
end MessageLawSpec
