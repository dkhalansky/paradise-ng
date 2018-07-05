name := "paradise-ng"
crossScalaVersions := Seq("2.11.11", "2.12.4")

// Otherwise tests are run during the assembly phase, before the resulting huge
// jar file is produced.
test in assembly := {}

libraryDependencies += "org.scalameta" %% "scalameta" % "3.3.0"
libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value
scalacOptions in Test += "-Xplugin:" + (assembly in Compile).value
scalacOptions in Test += "-Jdummy="  + (assembly in Compile).value.lastModified
scalacOptions in Test += "-Yrangepos"
