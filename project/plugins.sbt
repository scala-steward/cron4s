resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "0.6.29")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "1.6.0")
addSbtPlugin("com.codacy"         % "sbt-codacy-coverage"      % "3.0.3")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.3.7")
addSbtPlugin("com.47deg"          % "sbt-microsites"           % "0.9.7")
addSbtPlugin("com.eed3si9n"       % "sbt-unidoc"               % "0.4.2")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"          % "0.6.1")
addSbtPlugin("de.heikoseeberger"  % "sbt-header"               % "5.2.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.0.6")
addSbtPlugin("com.dwijnand"       % "sbt-travisci"             % "1.2.0")
addSbtPlugin("com.geirsson"       % "sbt-ci-release"           % "1.4.31")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.15.0-0.6")
