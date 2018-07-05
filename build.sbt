lazy val root = (project in file("."))
  .settings(
    name := "Nubank Merger",
    version := "0.0.0.1",
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xlint"
    )
  )
