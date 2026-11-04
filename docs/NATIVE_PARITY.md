# Native Python parity work

The implementation is in `src/main/scala/parity`; `core.SimEDaPE` mounts its
Pekko HTTP routes alongside the older `/api/v1` interface. Java numerical kernels
are imported from `org.htc:tslearn4j:1.1.0-SNAPSHOT`, published from the sibling
repository with Gradle. There is no Python subprocess or numerical API delegation in the native pipeline.
Optional HTC control routes use an explicit external-service integration described below.

## Implemented paths

| Area | Native implementation | Verification |
|---|---|---|
| Sources | Streaming authenticated ClickHouse JSONEachRow, typed query parameters, CSV, regular time-weighted occupancy and legacy event-index samples | Python occupancy fixture; isolated HTTP source test |
| Preprocessing | Six interpolators, per-series population normalization, primitive buffers, shared-memory workers | All variant fixtures; interpolation/kernel tests |
| Training | K-Shape, optional reduced clustering resolution, occupied-cluster remapping, L1 simulation points, raw-count PCA and exact constrained DTW | Python K-Shape assignments/PCA subspaces/report diagnostics; DTW reference cases |
| Estimation | All 100 DTW/PCA/PCA-delta variants, six interpolators, Wiener eigenvalue filters, individual/SP/stretched PCA | Python-produced model and outputs, numerical tolerance; batched DTW regression |
| Metrics | Nine traffic metrics; label pairing, error distributions, WAPE/sMAPE/bias/RMSE/MAE/correlations and breakdowns | Python result and comparison fixtures |
| Selection | All ten vehicle/person strategies, forced links, topology, unit mappings, frozen-mode decisions, selective instantiation | 120 Python selection fixtures |
| Models | `.simedape` ZIP/NPY interoperability, disk-backed DTW parts, PCA weights, clustering diagnostics and vehicle side data | Python reads Scala bundles; Scala reads Python bundles; reload/inference test |
| Pipeline | All sixteen stages, strict names/parameters, callbacks, cancellation checks, durable jobs and configs, CLI YAML/JSON/properties | Native training-to-report integration; isolated Pekko HTTP tests |
| API artifacts | Model metadata/download, estimates/pagination, comparisons, eleven charts, sweep tables, artifacts and reports | Python API response fixtures; HTTP integration |
| Multivariable | Masked PCA, joint/separate/grouped pattern estimators, weighted DTW, five representative selectors, paired residual calibration, provenance and simulator receipt checks, masked/reduced runners | Python golden predictions, diagnostics, masks and aggregation; isolated adapter runner tests |
| Observation data | Native DuckDB CSV/Parquet, JSONL, physical time bins, thirteen variables, exact large IDs, missing-observation semantics | Python fixtures across four source formats and three aggregation configurations |
| HTC integration | Explicit external service adapter for 66 optional route declarations, streamed requests/artifacts, external HTC run ownership and browser CORS | Isolated mock validates every route declaration; not service business-logic acceptance |
| Reports | Native vector PDF charts/tables, embedded Unicode font, LaTeX tables, article ZIP | PDF parsing/text checks, ZIP members and rendered visual review |

Reference fixtures are generated from the Python source with
`tools/parity/regenerate.sh` using a development image with NumPy, SciPy,
tslearn, dtaidistance and the Python pipeline. The image is not used by production
Scala. `docs/python-parity.json` inventories every Python route and stage,
including externally integrated platform endpoints; an implemented numerical core is not a
claim that every platform endpoint has been ported.

## Native API examples

Start a run with Python-shaped configuration and stage parameters:

```http
POST /api/runs
Content-Type: application/json

{
  "name": "Train occupancy model",
  "config": {
    "scenario_name": "occupancy-model",
    "data_source": "clickhouse",
    "street_info_file": "input/street_info.csv",
    "output_dir": "output/occupancy-model",
    "clickhouse": {"simulation_id": "full-training-run"},
    "intervals": {"start_time": 0, "end_time": 86400, "interval_size": 3600, "sample_period": 60},
    "clustering": {"n_clusters": 36, "clustering_size": 60}
  },
  "stages": ["data_load", "process_dataset", "clustering", "simulation_points", "pca_train", "warping_paths", "model_export"],
  "stage_params": {"warping_paths": {"n_jobs": 4}, "pca_train": {"n_components": 5}}
}
```

The response is HTTP 202 with `run_id`. Useful resources:

```text
GET    /api/runs/<run_id>
GET    /api/runs/<run_id>/progress
DELETE /api/runs/<run_id>
GET    /api/runs/<run_id>/artifacts
GET    /api/runs/<run_id>/model
GET    /api/runs/<run_id>/model/download
GET    /api/runs/<run_id>/clustering-report?cluster=0&near=5&far=5
GET    /api/estimation-variants
GET    /api/runs/<run_id>/estimation?method=avg&metric=average_speed&offset=0&limit=100
GET    /api/runs/<run_id>/comparison
GET    /api/runs/<run_id>/charts
GET    /api/runs/<run_id>/charts/mape_by_method
GET    /api/runs/<run_id>/sweep
GET    /api/runs/<run_id>/report.pdf?lang=en&names=short&top=10
GET    /api/runs/<run_id>/latex?table=ranking
GET    /api/runs/<run_id>/export.zip
GET    /api/runs/<run_id>/reports
GET    /api/runs/<run_id>/reports/compare_consolidated.pdf
```

Configuration CRUD is `/api/configs`; ClickHouse simulation discovery is
`/api/simulations?config=<saved-name>`. Artifact names are constrained to their
run directory. Concurrent jobs cannot own the same output directory.

## Multivariable experiments

The `multivariable_experiment` stage accepts `params.experiment_config` and an optional
`params.base_dir`. Inputs can be a portable `simedape.series.v1` NPZ dataset or
CSV/Parquet/JSONL microscopic observations plus aggregation configuration. Supply
explicit disjoint `training_runs` and `test_runs`. The runner supports `masked` and
`reduced` experiments, multiple models, seeds, fractions and selection policies;
it exports per-case predictions, masks, diagnostics, per-run metrics, seed summaries
and portable models. See `ExperimentRunnerSpec` for complete bounded examples.

Model methods are `pca`, `dtw`, `mean`, `baseline` and `paired_residual`. Pattern
models support joint, separate and grouped variables. Paired models require genuine
reduced-run calibration, checked provenance and completion receipts. The command
adapter uses an argv array and a bounded timeout; no shell command interpolation.
Masks support random, prefix, block and representatives with NumPy-compatible seeds.

Occupancy bundles use `simedape.model.v1`; multivariable models use native portable
`simedape.pattern.v1` or `simedape.paired.v1` ZIP/NPY archives. These multivariable
archives reload without refitting; they do not claim compatibility with Python joblib.

DuckDB uses streamed JDBC results, up to eight threads by default, and a 512 MB
memory budget. Configure `SIMEDAPE_DUCKDB_THREADS` and `SIMEDAPE_DUCKDB_MEMORY` as
needed. Aggregation still materializes its validated observation collection; this is
not a claim of a fully out-of-core training algorithm.

## HTC control services

Set `SIMEDAPE_PLATFORM_API_URL=http://host:5000` (an optional deployment base path
or trailing `/api` is accepted). The 66 platform route declarations for locations,
engines, scenario editing, OSM import, modifications, simulation/live/replay and
experiment campaigns are forwarded to that service. They require its existing
business logic and dependencies. They are **service integrations, not native ports**.

The 28 core SimEDaPE route declarations and all 16 numerical pipeline stages run in
Scala. POST `/api/runs` never delegates training to the external service. HTC jobs
created through POST `/api/htc/runs` retain their service ownership and appear in
`/api/runs`; their status, artifacts and cancellation are fetched from that service.
The adapter does not import the backend's full historical SimEDaPE job inventory.
GET `/api/platform/status` reports whether the backend is configured. With no backend,
platform control requests return an explicit HTTP 503. Shared services have not been
restarted or switched to Scala.

## Compatibility and evidence limits

* Portable `.simedape` is the occupancy cross-language model format. Native PCA
  does not load Python `joblib`/pickle models; convert with the Python exporter.
* Reports use Apache PDFBox vector graphics and embedded fonts. Layout is not a
  pixel-for-pixel reproduction of Matplotlib. Legacy ordinal Spearman semantics
  are preserved rather than silently replaced by a different statistic.
* The original `/api/v1` loader retains its historical event-index semantics.
  Use `/api` or the native CLI for the parity pipeline.
* Fixture agreement is bounded numerical/contract evidence, not a real-city
  throughput, memory, accuracy or UI acceptance campaign. Native numerical calls
  observe cancellation at stage/work-unit boundaries, not inside a BLAS call.
