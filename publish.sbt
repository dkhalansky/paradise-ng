PgpKeys.useGpg in Global := true
PgpKeys.gpgCommand in Global := "gpg2"
pgpSecretRing := pgpPublicRing.value

ThisBuild / organization := "com.github.dkhalansky"
ThisBuild / organizationName := "example"
ThisBuild / organizationHomepage := Some(url("http://example.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/dkhalansky/paradise-ng"),
    "scm:dkhalansky:dkhalansky/paradise-ng.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "rouanth",
    name  = "Dmitry Khalansky",
    email = "rouanth@gmail.com",
    url   = url("http://github.com/dkhalansky")
  )
)

ThisBuild / description := "Macro annotation compiler plugin for scalac."
ThisBuild / licenses := Seq("AGPL" -> url("https://www.gnu.org/licenses/agpl-3.0.en.html"))
ThisBuild / homepage := Some(url("https://github.com/dkhalansky/paradise-ng"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true
