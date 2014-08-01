import sbt._
import sbt.Keys._

object TypeClassesBuild extends Build {

  val baseDependencies = Seq(
    "com.chuusai" %% "shapeless" % "2.1.0-SNAPSHOT",
    "org.scala-lang" % "scala-library" % "2.11.2",
    "org.scala-lang" % "scala-reflect" % "2.11.2"
  )

  val baseResolvers = Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )

  lazy val baseSettings =
    Defaults.coreDefaultSettings ++
      Seq(
        organization := "org.pierzchalski",
        scalaVersion := "2.11.2",
        incOptions := incOptions.value.withNameHashing(nameHashing = true),
        resolvers ++= baseResolvers,
        libraryDependencies ++= baseDependencies
      )

  lazy val typeclasses = Project (
    id = "experimental",
    base = file("."),
    settings = baseSettings
  )
}

