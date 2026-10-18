package histos.zinc

import histos.jdk.{JdkVersion, ReleaseError}
import histos.pack.MainClass
import histos.testrun.{TestClasspath, TestProcess, TestRequest}
import histos.{Platform, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object Scala213Spec extends ZIOSpecDefault:
  private val v213 = Scala213.version
  private val v390 = JvmCompile.Scala390

  private val layout = suite("sources")(
    test("2.13 keeps its directories and drops Scala 3 and other platforms"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(write(root, "src/main/scala/Shared.scala", shared))
          _ <- ZIO.attempt(write(root, "scala-2.13/Only213.scala", only("Only213")))
          _ <- ZIO.attempt(write(root, "scala-2.13.16/Exact.scala", only("Exact")))
          _ <- ZIO.attempt(write(root, "scala-2.13.15/Other.scala", only("Other")))
          _ <- ZIO.attempt(write(root, "scala-3/Only3.scala", "enum Only3 { case A }\n"))
          _ <- ZIO.attempt(write(root, "jvm/OnJvm.scala", only("OnJvm")))
          _ <- ZIO.attempt(write(root, "js/OnJs.scala", only("OnJs")))
          _ <- ZIO.attempt(write(root, "src/main/java/JavaOnly.java", "class JavaOnly {}\n"))
          _ <- ZIO.attempt(write(root, "js/Skip.java", "class Skip {}\n"))
          for213 <- Sources.select(root, v213, Platform.Jvm)
          for3 <- Sources.select(root, v390, Platform.Jvm)
          forJs <- Sources.select(root, v213, Platform.Js)
          names213 = for213.map(root.relativize(_).toString)
          names3 = for3.map(root.relativize(_).toString)
          namesJs = forJs.map(root.relativize(_).toString)
        yield assertTrue(
          names213 == List(
            "jvm/OnJvm.scala",
            "scala-2.13.16/Exact.scala",
            "scala-2.13/Only213.scala",
            "src/main/java/JavaOnly.java",
            "src/main/scala/Shared.scala",
          ),
          names3 == List(
            "jvm/OnJvm.scala",
            "scala-3/Only3.scala",
            "src/main/java/JavaOnly.java",
            "src/main/scala/Shared.scala",
          ),
          namesJs == List(
            "js/OnJs.scala",
            "scala-2.13.16/Exact.scala",
            "scala-2.13/Only213.scala",
            "src/main/scala/Shared.scala",
          ),
        )
    ,
    test("a package named native is shared, and src/main filters still apply"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(write(root, "histos/native/Bridge.scala", only("Bridge")))
          _ <- ZIO.attempt(write(root, "src/histos/native/Nested.scala", only("Nested")))
          _ <- ZIO.attempt(write(root, "native/OnlyNative.scala", only("OnlyNative")))
          _ <- ZIO.attempt(write(root, "main/scala-2.13/FromMain.scala", only("FromMain")))
          _ <- ZIO.attempt(write(root, "src/main/scala/Shared.scala", shared))
          _ <- ZIO.attempt(write(root, "src/main/scala-2.13/Only213.scala", only("Only213")))
          _ <- ZIO.attempt(write(root, "src/main/scala-3/Only3.scala", "enum Only3 { case A }\n"))
          _ <- ZIO.attempt(write(root, "src/main/jvm/OnJvm.scala", only("OnJvm")))
          _ <- ZIO.attempt(write(root, "src/main/js/OnJs.scala", only("OnJs")))
          for213 <- Sources.select(root, v213, Platform.Jvm)
          for3 <- Sources.select(root, v390, Platform.Jvm)
          forNative <- Sources.select(root, v390, Platform.Native)
          forJs <- Sources.select(root, v213, Platform.Js)
          names213 = for213.map(root.relativize(_).toString)
          names3 = for3.map(root.relativize(_).toString)
          namesNative = forNative.map(root.relativize(_).toString)
          namesJs = forJs.map(root.relativize(_).toString)
        yield assertTrue(
          names213 == List(
            "histos/native/Bridge.scala",
            "main/scala-2.13/FromMain.scala",
            "src/histos/native/Nested.scala",
            "src/main/jvm/OnJvm.scala",
            "src/main/scala-2.13/Only213.scala",
            "src/main/scala/Shared.scala",
          ),
          names3 == List(
            "histos/native/Bridge.scala",
            "src/histos/native/Nested.scala",
            "src/main/jvm/OnJvm.scala",
            "src/main/scala-3/Only3.scala",
            "src/main/scala/Shared.scala",
          ),
          namesNative == List(
            "histos/native/Bridge.scala",
            "native/OnlyNative.scala",
            "src/histos/native/Nested.scala",
            "src/main/scala-3/Only3.scala",
            "src/main/scala/Shared.scala",
          ),
          namesJs == List(
            "histos/native/Bridge.scala",
            "main/scala-2.13/FromMain.scala",
            "src/histos/native/Nested.scala",
            "src/main/js/OnJs.scala",
            "src/main/scala-2.13/Only213.scala",
            "src/main/scala/Shared.scala",
          ),
        )
  )

  private val compile = suite("scala 2.13.16")(
    test("compiles the 2.13 sources and the subprocess runs the class"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(write(root, "src/main/scala/Shared.scala", "object Shared { def n: Int = 1 }\n"))
          _ <- ZIO.attempt(write(root, "scala-2.13/Hello.scala", hello))
          _ <- ZIO.attempt(write(root, "scala-3/Later.scala", "enum Later { case A }\n"))
          sources <- Sources.select(root, v213, Platform.Jvm)
          found <- Scala213.jars(v213)
          jdk <- ZIO.fromEither(running)
          classes = root.resolve("classes")
          compiled <- JvmCompile.compile(CompileRequest(
                        CompileToolchain(v213, Platform.Jvm, jdk, ScalacFlags.empty),
                        sources,
                        Nil,
                        classes,
                        root.resolve("analysis"),
                        Nil,
                      )).either
          names = sources.flatMap(path => Option(path.getFileName).map(_.toString)).sorted
          classFile = Files.isRegularFile(classes.resolve("Hello.class"))
          work <- ZIO.attempt(Files.createDirectories(root.resolve("work")))
          main = MainClass.from("Hello")
          ran <- (compiled, main) match
            case (Right(_), Right(name)) =>
              TestProcess.run(TestRequest(
                Platform.Jvm,
                javaBin,
                TestClasspath(classes, found.runtime),
                name,
                work,
              )).either.map(Some(_))
            case _ => ZIO.succeed(None)
        yield assertTrue(
          names == List("Hello.scala", "Shared.scala"),
          found.runtime == List(found.library),
          !found.runtime.contains(found.compiler),
          !found.compilerClasspath.contains(found.library),
          found.compilerClasspath.contains(found.compiler),
          main.map(_.binary) == Right("Hello"),
          compiled.map(_.classes) == Right(classes),
          classFile,
          ran.map(_.map(_.exit.value)) == Some(Right(0)),
          ran.map(_.map(_.out.text)) == Some(Right("1\nhi\n")),
        )
    ,
    test("a declared Scala 2 compiler is a library"):
      ZIO.scoped:
        for
          root <- temp
          found <- Scala213.jars(v213)
          jdk <- ZIO.fromEither(running)
          compiler = found.compiler
          out <- JvmCompile.compile(CompileRequest(
                   CompileToolchain(v213, Platform.Jvm, jdk, ScalacFlags.empty),
                   Nil,
                   List(compiler),
                   root.resolve("classes"),
                   root.resolve("analysis"),
                   Nil,
                 )).either
        yield assertTrue(out == Left(CompileError.NoSources))
    ,
    test("the engine classpath cannot see the Scala 2 compiler"):
      val loader = getClass.getClassLoader
      assertTrue(
        !ClassMarker.NscGlobal.visibleTo(loader),
        !ClassMarker.XsbtBridge.visibleTo(loader),
        !ClassMarker.JavaUniverse.visibleTo(loader),
      )
    ,
    test("compiled code cannot see the compiler, reflect, jline, or diffutils"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(write(root, "Leak.scala", leak))
          jdk <- ZIO.fromEither(running)
          out <- JvmCompile.compile(CompileRequest(
                   CompileToolchain(v213, Platform.Jvm, jdk, ScalacFlags.empty),
                   List(root.resolve("Leak.scala")),
                   Nil,
                   root.resolve("classes"),
                   root.resolve("analysis"),
                   Nil,
                 )).either
          text = out match
            case Left(error)   => error.message
            case Right(output) => output.classes.toString
        yield assertTrue(
          text == """object tools is not a member of package scala
                    |object runtime is not a member of package reflect
                    |object jline is not a member of package org
                    |object github is not a member of package com""".stripMargin,
        )
    ,
    test("2.13.18 compiles"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(write(root, "Hello.scala", "object Hello { def greet: String = \"hi\" }\n"))
          found <- Scala213.jars(ScalaVersion.trusted("2.13.18"))
          jdk <- ZIO.fromEither(running)
          compiled <- JvmCompile.compile(CompileRequest(
                        CompileToolchain(ScalaVersion.trusted("2.13.18"), Platform.Jvm, jdk, ScalacFlags.empty),
                        List(root.resolve("Hello.scala")),
                        Nil,
                        root.resolve("classes"),
                        root.resolve("analysis"),
                        Nil,
                      ))
          names <- ZIO.attempt {
                     val walk = Files.walk(compiled.classes)
                     try
                       walk.iterator.asScala.filter(Files.isRegularFile(_)).map(path => compiled.classes.relativize(path).toString).toList.sorted
                     finally walk.close()
                   }
        yield assertTrue(
          found.runtime == List(found.library),
          names == List("Hello$.class", "Hello.class"),
        )
  )

  private val hello =
    """object Hello {
      |  def main(args: Array[String]): Unit = {
      |    println(Shared.n)
      |    println("hi")
      |  }
      |}
      |""".stripMargin

  private val shared = "object Shared { def n: Int = 1 }\n"

  private val leak =
    """object Leak {
      |  def compiler = classOf[scala.tools.nsc.Global]
      |  def reflect = classOf[scala.reflect.runtime.JavaUniverse]
      |  def line = classOf[org.jline.reader.LineReader]
      |  def diff = classOf[com.github.difflib.DiffUtils]
      |}
      |""".stripMargin

  private def only(name: String): String = s"object $name { def n: Int = 1 }\n"

  private val javaBin = Path.of(java.lang.System.getProperty("java.home")).resolve("bin").resolve("java")

  private def running: Either[ReleaseError, ResolvedJdk] =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then Files.readString(home.resolve("release")) else ""
    JdkVersion.fromRelease(text).map(version => ResolvedJdk(home, version))

  private def write(root: Path, name: String, text: String): Path =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
    path

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-213"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("scala 2.13")(layout, compile) @@ TestAspect.sequential @@ TestAspect.timeout(3.minutes)
