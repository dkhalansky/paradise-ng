name := "paradise-ng"

lazy val commonSettings = Def.settings(
    crossScalaVersions := Seq("2.11.11", "2.12.4"),
    libraryDependencies += "org.scalameta" %% "scalameta" % "3.3.0"
)

// RUNTIME LIBRARY /////////////////////

lazy val paradiseNgLib = (project in file("lib")).settings(commonSettings)

// PLUGIN //////////////////////////////

lazy val paradiseNgPlugin = (project in file("plugin")).
    dependsOn(paradiseNgLib).
    settings(
        commonSettings,
        libraryDependencies +=
            scalaOrganization.value % "scala-compiler" % scalaVersion.value,
        assemblyOption.in(assembly) ~= { _.copy(includeScala = false) }
    )

// The description of the jar file where the plugin resides.
lazy val jar = assembly in paradiseNgPlugin

// Compiler options needed to enable a compiler plugin.
def pluginOptions(jar: File): Seq[String] = Seq(
    "-Xplugin:" + jar,
    "-Jdummy="  + jar.lastModified
)

// TESTS ///////////////////////////////////////////////////////////////////////

lazy val testSettings = Def.settings(
        commonSettings,
        publishArtifact := false,
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1",
        scalacOptions in Test ++= pluginOptions(jar.value)
    )

// TESTS OF COMPILE-TIME EXPANSIONS ///

lazy val expansionTests = (project in file("tests/expansion")).
    dependsOn(paradiseNgLib).
    settings(testSettings)

// AGGREGATE TEST PROJECT /////////////

lazy val tests = project.aggregate(expansionTests)

// MAIN PROJECT ///////////////////////////////////////////////////////////////

lazy val `paradise-ng` = (project in file(".")).dependsOn(paradiseNgLib).
    aggregate(paradiseNgLib, paradiseNgPlugin, tests).
    settings(commonSettings, scalacOptions ++= pluginOptions(jar.value))
