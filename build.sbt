import scala.xml.transform._
import scala.xml.{Node => XNode, NodeSeq}
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.CrossPlugin.autoImport._

val commonSettings = Seq(
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", "2.12.8", "2.13.0-RC1"),
  organization := "org.http4s",
  homepage := Some(new URL("http://parboiled.org")),
  description := "Fork of parboiled2 for http4s, sans shapeless dependency",
  startYear := Some(2009),
  licenses := Seq("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  unmanagedResources in Compile += baseDirectory.value.getParentFile.getParentFile / "LICENSE",
  scmInfo := Some(ScmInfo(url("https://github.com/sirthias/parboiled2"), "scm:git:git@github.com:sirthias/parboiled2.git")),

  scalaVersion := "2.12.10",
  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1"),

  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:_",
    "-unchecked",
    "-Xlint:_,-missing-interpolator",
    "-Ywarn-dead-code",
    //"-Ywarn-numeric-widen",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => Seq(
        "-Yno-adapted-args",
        // "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Xfuture",
      )
      case Some((2, 12)) => Seq(
        "-Yno-adapted-args",
        // "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
        "-Ycache-macro-class-loader:last-modified",
        "-Ybackend-parallelism", "8",
        // "-Xfatal-warnings",
        "-Xfuture",
        "-Xsource:2.13", // new warning: deprecate assignments in argument position
      )
      case Some((2, 13)) => Seq(
        "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
        "-Ycache-macro-class-loader:last-modified",
        "-Ybackend-parallelism", "8",
      )
      case x => sys.error(s"unsupported scala version: $x")
    }
  },
)

lazy val publishingSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := (_ ⇒ false),
  publishTo := sonatypePublishTo.value,
  developers := List(
    Developer("sirthias", "Mathias Doenitz", "devnull@bullet.io", url("https://github.com/sirthias")),
    Developer("alexander-myltsev", "Alexander Myltsev", "", url("http://www.linkedin.com/in/alexandermyltsev"))
  )
)

val utestSettings = Seq(testFrameworks := Seq(new TestFramework("utest.runner.Framework")))

/////////////////////// DEPENDENCIES /////////////////////////

val utestVersion = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 11 =>
      "0.6.8"
    case _ =>
      "0.6.9"
  }
)

val utest            = Def.setting("com.lihaoyi"    %%% "utest"         % utestVersion.value % Test)
val scalaCheck       = Def.setting("org.scalacheck" %%% "scalacheck"    % "1.14.3"           % Test)
val `scala-reflect`  = Def.setting("org.scala-lang" %   "scala-reflect" % scalaVersion.value % "provided")
val `specs2-common`  = Def.setting("org.specs2"     %%% "specs2-common" % "4.5.1"            % Test)

/////////////////////// PROJECTS /////////////////////////

lazy val root = project.in(file("."))
  .aggregate(parboiledJVM, parboiledJS)
  .aggregate(parboiledCoreJVM, parboiledCoreJS)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(
    publishArtifact := false,
  )

lazy val parboiledJVM = parboiled.jvm
lazy val parboiledJS = parboiled.js
lazy val parboiled = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(parboiledCore)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(utestSettings)
  .jvmSettings(
    mappings in (Compile, packageBin) ++= (mappings in (parboiledCoreJVM.project, Compile, packageBin)).value,
    mappings in (Compile, packageSrc) ++= (mappings in (parboiledCoreJVM.project, Compile, packageSrc)).value,
    mappings in (Compile, packageDoc) ++= (mappings in (parboiledCoreJVM.project, Compile, packageDoc)).value
  )
  .jsSettings(
    mappings in (Compile, packageBin) ++= (mappings in (parboiledCoreJS.project, Compile, packageBin)).value,
    mappings in (Compile, packageSrc) ++= (mappings in (parboiledCoreJS.project, Compile, packageSrc)).value,
    mappings in (Compile, packageDoc) ++= (mappings in (parboiledCoreJS.project, Compile, packageDoc)).value
  )
  .settings(
    libraryDependencies ++= Seq(`scala-reflect`.value, utest.value),
    mappings in (Compile, packageBin) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    mappings in (Compile, packageDoc) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    pomPostProcess := { // we need to remove the dependency onto the parboiledCore module from the POM
      import scala.xml.transform._
      import scala.xml.{NodeSeq, Node => XNode}

      val filter = new RewriteRule {
        override def transform(n: XNode) = if ((n \ "artifactId").text.startsWith("parboiledcore")) NodeSeq.Empty else n
      }
      new RuleTransformer(filter).transform(_).head
    }
  )

lazy val generateActionOps = taskKey[Seq[File]]("Generates the ActionOps boilerplate source file")

lazy val parboiledCoreJVM = parboiledCore.jvm
lazy val parboiledCoreJS = parboiledCore.js
lazy val parboiledCore = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("parboiled-core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(skip in publish := true)
  .settings(utestSettings)
  .settings(
    libraryDependencies ++= Seq(`scala-reflect`.value, `specs2-common`.value, utest.value),
    generateActionOps := ActionOpsBoilerplate((sourceManaged in Compile).value, streams.value),
    (sourceGenerators in Compile) += generateActionOps.taskValue
  )
  .jvmSettings(libraryDependencies += scalaCheck.value)
  .jsSettings(libraryDependencies += scalaCheck.value)
