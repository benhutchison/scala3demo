val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 Demo",
    description := "Demos of Scala 3 features",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    // scalacOptions ++= Seq("-explain-types"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0-M4",
      "org.typelevel" %% "cats-effect" % "1.3.1",
      "co.fs2" %% "fs2-core" % "1.1.0-M1",
      "org.typelevel" %% "mouse" % "0.21",
      "com.github.julien-truffaut" %% "monocle-core" % "1.6.0",
    ).map(_.withDottyCompat(dottyVersion))

    
  )