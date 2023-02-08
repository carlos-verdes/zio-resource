import Dependencies._
import Libraries._
import ReleaseTransformations._

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / homepage := Some(url("https://github.com/carlos-verdes/zio-arangodb"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/carlos-verdes/zio-arangodb"), "git@github.com:carlos-verdes/zio-arangodb.git"))
ThisBuild / developers := List(Developer("carlos-verdes", "Carlos Verdes", "cverdes@gmail.com", url("https://github.com/carlos-verdes")))
ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("releases")

// release plugin
//import ReleaseTransformations._
import xerial.sbt.Sonatype._

ThisBuild / releaseCrossBuild := true
ThisBuild / releasePublishArtifactsAction := PgpKeys.publishSigned.value

Test / fork := true
IntegrationTest / fork := true

inThisBuild(
  Seq(
    organization := "io.funkode",
    scalaVersion :="3.2.2-RC1",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0",
    publishTo := sonatypePublishToBundle.value,
    sonatypeCredentialHost :="s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    startYear := Some(2023),
    licenses += ("MIT", new URL("https://opensource.org/licenses/MIT")),
))

ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    //"-explain",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
//    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Ykind-projector",
//    "-Ysafe-init", // experimental (I've seen it cause issues with circe)
    "-Yretain-trees"
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future-migration")


lazy val commonLibs = Seq(scalaUri, logBack, zioPrelude, jansi, zioConfMagnolia, zioConfTypesafe)
lazy val zioLibs = Seq(zio, zioHttp, zioJson, zioConcurrent, zioConfMagnolia, zioConfTypesafe)
lazy val testLibs = Seq(tapirSttpStubServer, zioTest, zioTestSbt, sttpClient, zioJGolden).map(_ % "it, test")

lazy val root =
  project
    .in(file("."))
    .configs(IntegrationTest)
    .settings(Defaults.itSettings)
    .settings(headerSettings(Test, IntegrationTest))
    .settings(Seq(
      name := "zio-resource",
      libraryDependencies ++= (commonLibs ++ testLibs ++ Seq(zioSchema, zioArangodb, shapeless)),
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      headerLicense := Some(HeaderLicense.MIT("2023", "Carlos Verdes", HeaderLicenseStyle.SpdxSyntax))
    )
    .enablePlugins(AutomateHeaderPlugin)

ThisBuild / coverageExcludedFiles := ".*Main.*;zio\\.json\\.*"

addCommandAlias("ll", "projects")
addCommandAlias("checkFmtAll", ";scalafmtSbtCheck;scalafmtCheckAll")
addCommandAlias("testAll", ";compile;test;stryker")
//addCommandAlias("sanity", ";compile;scalafmtAll;test;stryker")
addCommandAlias("sanity", ";clean;coverage;compile;headerCreate;scalafixAll;scalafmtAll;test;it:test;coverageAggregate;coverageOff")

ThisBuild / publishMavenStyle.withRank(KeyRanks.Invisible) := true
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

//see: https://github.com/sbt/sbt-release#release-process && https://stackoverflow.com/a/65519285
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,              // : ReleaseStep
  inquireVersions,                        // : ReleaseStep
  runClean,                               // : ReleaseStep
  runTest,                                // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  //commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
  //tagRelease,                             // : ReleaseStep
  publishArtifacts,                       // : ReleaseStep, checks whether `publishTo` is properly set up
  //releaseStepTask(publish in Docker),     // : ReleaseStep, publish the docker image in your specified repository(e.i. Nexus)
  //setNextVersion,                         // : ReleaseStep
  //commitNextVersion,                      // : ReleaseStep
  //pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
)
