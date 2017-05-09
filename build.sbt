name := "applicative"

version := "1.0"

lazy val applicative = (project in file("."))
  .settings(
    scalaVersion := "2.12.0",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.2" % Test,
      "org.typelevel" %% "cats" % "0.9.0"
    )
  )


