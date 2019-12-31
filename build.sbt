ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "com.example"

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"

lazy val main = (project in file("."))
  .aggregate(core)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "Main",
    libraryDependencies += scalaTest % Test,
  )

lazy val core = (project in file("core"))
  .settings(
    name := "Core",
    libraryDependencies += scalaTest % Test,
  )