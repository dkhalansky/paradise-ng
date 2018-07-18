name := "paradise-ng"

lazy val commonSettings = Def.settings(
    crossScalaVersions := Seq("2.11.11", "2.12.4"),
    libraryDependencies += "org.scalameta" %% "scalameta" % "3.3.0"
)

lazy val paradiseNgLib = (project in file("lib")).settings(commonSettings)

lazy val paradiseNgPlugin = (project in file("plugin")).
    dependsOn(paradiseNgLib).
    settings(
        commonSettings,
        libraryDependencies +=
            scalaOrganization.value % "scala-compiler" % scalaVersion.value,
        assemblyOption.in(assembly) ~= { _.copy(includeScala = false) }
    )

lazy val tests = (project in file("tests")).dependsOn(paradiseNgLib).
    settings(
        commonSettings,
        publishArtifact := false,
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1",
        scalacOptions in Test ++= Seq(
            "-Xplugin:" + (assembly in paradiseNgPlugin).value,
            "-Jdummy="  + (assembly in paradiseNgPlugin).value.lastModified
        )
    )

lazy val `paradise-ng` = (project in file(".")).dependsOn(paradiseNgLib).
    aggregate(
        paradiseNgLib,
        paradiseNgPlugin,
        tests
    ).
    settings(
        commonSettings,
        scalacOptions ++= Seq(
            "-Xplugin:" + (assembly in paradiseNgPlugin).value,
            "-Jdummy="  + (assembly in paradiseNgPlugin).value.lastModified
        )
    )
