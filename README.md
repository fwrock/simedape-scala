# SimEDaPE Scala

Native SimEDaPE on Java 21+, Scala 3.3.9 LTS, sbt 1.13.1, Apache Pekko 1.7.1 and Pekko HTTP 1.4.2.

The Python-shaped pipeline now has native ClickHouse/CSV loading, preprocessing,
K-Shape, simulation points, DTW and PCA training, all 100 estimation variants,
all nine traffic metrics, comparisons, sweeps, ten selection strategies,
portable models, charts and report exports. All 16 pipeline stages run natively,
including masked and reduced multivariable experiments and paired residual calibration.
The 28 core API route declarations are implemented in Scala. The 66 optional HTC
control-route declarations integrate existing services through an explicitly configured
backend; their business logic has not been ported. See [the parity inventory](docs/python-parity.json)
and [implementation details and limits](docs/NATIVE_PARITY.md).
[Validation evidence](docs/VALIDATION.md): 135 Java tests and 34 Scala tests pass.

## Build

Build the sibling Java library first; Scala imports it as an ordinary Maven dependency.

```bash
cd ../tslearn4j
./gradlew test publishToMavenLocal
cd ../simedape-scala
sbt test assembly
```

The Java library supplies K-Shape, native OpenBLAS/LAPACK linear algebra, PCA,
interpolation, reproducible sampling and exact constrained DTW. The standalone
`simedape-warping-path` application uses approximate FastDTW and is not a dependency.
Python is not required for native SimEDaPE computation or the core API.
Optional HTC control routes depend on the configured external service. Development-only Python
scripts regenerate numerical reference fixtures.

## Library dependencies

```text
simedape-scala -> org.htc:tslearn4j:1.1.0-SNAPSHOT -> OpenBLAS/LAPACK (JavaCPP)
```

Scala calls the Java library directly in the same JVM. Exact constrained warping
paths are provided by `org.tslearn.metrics.ConstrainedDTW` in `tslearn4j`.
The separate `simedape-warping-path` repository is a standalone approximate FastDTW
application; it is not imported or invoked by this native pipeline. The Python
SimEDaPE implementation retains its Python numerical dependencies.

## Run a pipeline

```bash
sbt 'run run examples/native-clickhouse.yml'
sbt 'run run examples/native-estimation.yml'
sbt 'run run examples/native-multivariable-fixture.yml'
# Or, after assembly:
java -jar target/scala-3.3.9/simedape.jar run examples/native-clickhouse.yml
```

Set the scenario IDs and street metadata path in the examples first. Connection
settings default to `HTC_CLICKHOUSE_HOST`, `HTC_CLICKHOUSE_PORT`,
`HTC_CLICKHOUSE_DATABASE`, `HTC_CLICKHOUSE_USER` and `HTC_CLICKHOUSE_PASSWORD`.

JSON and YAML accept the Python `config`/`stages` shape, including per-stage
`params`. Legacy `.properties` keys are supported; pass stage names as trailing
arguments when the file contains only configuration. A model can be reused via
`config.input_dir`. PCA models use portable `.simedape` files instead of Python
`joblib` objects.

## API

```bash
SIMEDAPE_STATE_DIR=state sbt run
```

The new pipeline lives at `/api`, including configuration CRUD, asynchronous
Pekko-managed runs, stage progress, model download, estimation pagination,
comparisons, all eleven JSON chart types, sweeps, PDF, LaTeX and article ZIP
exports. State survives service restarts; interrupted jobs are recorded as
failed rather than silently marked complete. Set `SIMEDAPE_PLATFORM_API_URL=http://host:5000`
to integrate the existing HTC control API. Without it, control routes return HTTP 503;
`GET /api/platform/status` reports the boundary and configuration. See [native API examples](docs/NATIVE_PARITY.md).

The older `/api/v1` actor interface remains available and uses its original
configuration and loaders. Its input semantics differ from the new Python-compatible
pipeline; [API.md](API.md) documents that interface.

## Memory and parallelism

* Loading streams ordered ClickHouse rows and retains event batches for one link.
  Regular occupancy integrates physical time and includes empty catalogue links.
* Preprocessing uses flat primitive arrays and `SIMEDAPE_PROCESS_WORKERS`
  (default: up to eight cores). It does not fork or copy the dataset into processes.
* K-Shape avoids retained training copies and uses thin native SVD when clusters
  contain fewer series than time points. PCA uses thin native SVD.
* DTW stores only its admissible band. Training uses bounded `n_jobs` workers and
  writes compressed typed arrays; estimation shares path decompression and translation
  between normalization variants.
* Variant `workers` share immutable prepared arrays. Metrics stream to disk instead
  of accumulating a hundred large boxed result maps. Model downloads stream bytes.
* `SIMEDAPE_JOB_WORKERS` controls simultaneous pipeline jobs (default one). Start
  with `OPENBLAS_NUM_THREADS=1` when using concurrent jobs or variant workers.

These changes remove specific copies, allocations and repeated work. No speedup
against the user's 17-minute dataset has been measured; fixture timings are not a
large-city benchmark.
