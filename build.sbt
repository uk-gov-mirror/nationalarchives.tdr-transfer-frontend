
name := """tdr-transfer-frontend"""
organization := "tna"
maintainer := "TDRTeam@nationalarchives.gov.uk"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

watchSources ++= (baseDirectory.value / "npm/src" ** "*").get

resolvers += "TDR Releases" at "s3://tdr-releases-mgmt"

scalaVersion := "2.13.3"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3" % Test

val playPac4jVersion = "10.0.2"
val pac4jVersion = "4.2.0"
val akkaVersion = "2.6.3"
val sttpVersion = "2.2.4"

libraryDependencies ++= Seq(
  "org.pac4j" %% "play-pac4j" % playPac4jVersion,
  "org.pac4j" % "pac4j-http" % pac4jVersion exclude("com.fasterxml.jackson.core", "jackson-databind"),
  "org.pac4j" % "pac4j-oidc" % pac4jVersion exclude("commons-io", "commons-io") exclude("com.fasterxml.jackson.core", "jackson-databind"),
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",
  "com.softwaremill.sttp.client" %% "core" % sttpVersion,
  "com.softwaremill.sttp.client" %% "circe" % sttpVersion,
  "com.softwaremill.sttp.client" %% "async-http-client-backend-future" % sttpVersion,
  "uk.gov.nationalarchives" %% "tdr-graphql-client" % "0.0.15",
  "uk.gov.nationalarchives" %% "tdr-auth-utils" % "0.0.12",
  "uk.gov.nationalarchives" %% "tdr-generated-graphql" % "0.0.79",
  ws,
  "com.github.tomakehurst" % "wiremock-jre8" % "2.26.0" % Test,
  "org.mockito" % "mockito-core" % "3.3.0" % Test
)
scalacOptions ++= Seq("-language:implicitConversions")

libraryDependencies += play.sbt.PlayImport.cacheApi
libraryDependencies += "com.github.karelcemus" %% "play-redis" % "2.6.0"

pipelineStages := Seq(digest)

envVars in Test := Map("COGNITO_ROLE_ARN" -> "roleArn")

