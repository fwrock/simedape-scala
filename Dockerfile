FROM eclipse-temurin:21-jre-jammy

WORKDIR /app

RUN mkdir -p /app/logs /tmp/simedape/warping_paths /data/simulations

# Fat JAR produced by `sbt assembly`
COPY target/scala-3.3.9/simedape.jar app.jar

# REST API
EXPOSE 8080

# JVM flags:
#   -XX:+ZGenerational   — low-latency GC (JDK 21+)
#   -Djdk.attach.allowAttachSelf=true  — uncomment to profile memory
CMD ["java", \
     "-XX:+UseZGC", \
     "-XX:+ZGenerational", \
     "-XX:MaxRAMPercentage=75.0", \
     "-jar", "app.jar"]