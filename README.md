# SimEDaPE Scala

SimEDaPE pipeline on Apache Pekko, targeting Java 21. The build uses Scala
3.3.9 LTS, sbt 1.13.1, Pekko 1.7.1 and Pekko HTTP 1.4.2.

## Build and test

Build the sibling `tslearn4j` first. Its local Maven publication supplies
K-Shape and the native OpenBLAS/LAPACK backend:

```bash
cd ../tslearn4j
./gradlew test publishToMavenLocal
cd ../simedape-scala
sbt test assembly
```

Use JDK 21 or newer for sbt; emitted bytecode targets Java 21. The Docker runtime
uses Eclipse Temurin 21. `./build-and-run.sh` builds the assembly and Docker image
and starts the configured Compose services. See [API.md](API.md) for the API.

## Numerical backend

K-Shape computes covariance using native OpenBLAS and symmetric eigenvectors
using LAPACK through JavaCPP. PCA uses the same double-precision backend instead
of ND4J. PCA operates on feature covariance (`T × T`), suited to many short series;
very long series still require substantial covariance memory. Setting the
estimation method to `PCA` routes the estimation worker through this backend.

For concurrent actor workers, start with `OPENBLAS_NUM_THREADS=1` to avoid nested
BLAS and actor parallelism, then benchmark on the deployment machine. The warping
supervisor enforces `worker-parallelism` and uses the compute dispatcher.

Regression tests cover centroid dimensions, population normalization, relaxed
DTW endpoints, native PCA reconstruction, PCA actor routing and sequential worker
slots. The obsolete Kafka test referenced classes absent from this repository
and has been replaced by tests of the actual pipeline.

## Input compatibility

The Scala ClickHouse loader currently reads `link_vehicle_counts.peak_count`
and resamples recorded rows by event index. Python's regular occupancy reader
integrates time-weighted occupancy and includes catalogue links with no events.
These input definitions differ: align them before comparing full training time
or estimation accuracy. The language-review fixtures under the parent
`experiments/simedape-language-review` compare numerical kernels on identical
inputs; they are not a Toulouse end-to-end benchmark.
