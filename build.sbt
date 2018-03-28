name := "paradise-ng"
crossScalaVersions := Seq("2.11.11", "2.12.4")
libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value
scalacOptions in Test += "-Xplugin:" + (packageBin in Compile).value
scalacOptions in Test += "-Jdummy="  + (packageBin in Compile).value.lastModified
scalacOptions in Test += "-Yrangepos"
