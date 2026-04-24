import sbt.Keys.libraryDependencies
import scala.collection.Seq

ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "3.3.5"

// Apache Pekko
val pekkoVersion           = "1.4.0"
val pekkoHttpVersion       = "1.3.0"
val jacksonVersion         = "2.21.2"
val kryoVersion            = "1.5.0"

// Logs
val logbackVersion = "1.5.32"

// Scala test
val scalaTestVersion = "3.2.20"

// ClickHouse
val clickhouseVersion = "0.7.0"

// ND4J (PCA)
val nd4jVersion = "1.0.0-M2.1"

// Apache Commons Math (transitive via tslearn4j, explicit for safety)
val commonsMathVersion = "3.6.1"

// Prometheus Metrics
val prometheusVersion = "0.16.0"

resolvers += Resolver.mavenLocal

lazy val root = (project in file("."))
  .settings(
    name := "simedape",
    idePackagePrefix := Some("org.interscity.htc"),
    assembly / assemblyJarName := "simedape.jar",
    assembly / mainClass := Some("org.interscity.htc.main"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", _*)             => MergeStrategy.discard
      case "reference.conf"                     => MergeStrategy.concat
      case "application.conf"                   => MergeStrategy.concat
      case "logback.xml"                        => MergeStrategy.first
      case _                                    => MergeStrategy.first
    },
    libraryDependencies ++= Seq(
      // Apache Pekko — typed actors + streams + HTTP
      "org.apache.pekko" %% "pekko-http-cors"             % pekkoHttpVersion,
      "org.apache.pekko" %% "pekko-actor-typed"           % pekkoVersion,
      "org.apache.pekko" %% "pekko-stream"               % pekkoVersion,
      "org.apache.pekko" %% "pekko-http"                 % pekkoHttpVersion,
      "org.apache.pekko" %% "pekko-http-spray-json"      % pekkoHttpVersion,
      "org.apache.pekko" %% "pekko-serialization-jackson" % pekkoVersion,
      "org.apache.pekko" %% "pekko-slf4j"                % pekkoVersion,

      // Jackson (CBOR + Scala module for actor message serialization)
      "com.fasterxml.jackson.module" %% "jackson-module-scala"   % jacksonVersion,
      "com.fasterxml.jackson.core"   %  "jackson-databind"       % jacksonVersion,
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % jacksonVersion,

      // Kryo (alternative fast serializer)
      "io.altoo" %% "pekko-kryo-serialization" % kryoVersion,

      // ClickHouse JDBC — reads/writes time series from TimeSeriesDB
      // "all" classifier is an uber-jar; exclude transitive deps to avoid SNAPSHOT resolution
      ("com.clickhouse" % "clickhouse-jdbc" % clickhouseVersion classifier "all")
        .excludeAll(ExclusionRule("com.clickhouse")),
      "com.zaxxer"     % "HikariCP"         % "5.1.0",

      // tslearn4j — KShape clustering + preprocessing
      "org.htc" % "tslearn4j" % "1.0",

      // Apache Commons Math (transitive via tslearn4j; explicit for reliability)
      "org.apache.commons" % "commons-math3" % commonsMathVersion,

      // ND4J — accelerated linear algebra for PCA estimation
      "org.nd4j" % "nd4j-native-platform" % nd4jVersion,

      // Logs
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "ch.qos.logback" % "logback-core"    % logbackVersion,
      "org.slf4j"      % "slf4j-api"       % "2.0.17",

      "com.typesafe" % "config" % "1.4.6",

      // Prometheus Metrics
      "io.prometheus" % "simpleclient"            % prometheusVersion,
      "io.prometheus" % "simpleclient_hotspot"    % prometheusVersion,
      "io.prometheus" % "simpleclient_httpserver" % prometheusVersion,

      // CSV parsing (loading legacy file-based simulations)
      "com.github.tototoshi" %% "scala-csv" % "2.0.0",

      // Test
      "org.scalatest" %% "scalatest"                    % scalaTestVersion % Test,
      "org.apache.pekko" %% "pekko-actor-testkit-typed" % pekkoVersion     % Test,
    ),

    dependencyOverrides ++= Seq(
      "com.fasterxml.jackson.core"   % "jackson-core"       % jacksonVersion,
      "com.fasterxml.jackson.core"   % "jackson-databind"   % jacksonVersion,
      "com.fasterxml.jackson.module" % "jackson-module-scala_3" % jacksonVersion,
    ),
  )
