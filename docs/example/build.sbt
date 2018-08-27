scalaVersion := "2.12.4"
autoCompilerPlugins := true

lazy val root: Project = project.in(file(".")).aggregate(macros, core)

lazy val macros: Project = project.in(file("macros")).settings(
    libraryDependencies += "com.github.dkhalansky" % "paradisenglib" % "0.1.3" cross CrossVersion.full
)

lazy val core: Project = project.in(file("core")).dependsOn(macros).settings(
    addCompilerPlugin("com.github.dkhalansky" % "paradisengplugin" % "0.1.3" cross CrossVersion.full)
)
