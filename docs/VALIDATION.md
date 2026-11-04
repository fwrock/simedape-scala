# Native parity validation — 2026-11-04

Reference: Python SimEDaPE `28f1e56`. Java kernels: tslearn4j
`26b7e34603daff06f8a2b471545531178af6bd4b` (`1.1.0-SNAPSHOT`).
Local runtime: IBM Semeru Java 21.0.8, Scala 3.3.9, sbt 1.13.1;
OpenBLAS/LAPACK through JavaCPP. The CI workflow builds the pinned Java commit
before running Scala tests, without requiring a Python installation for inference.

Validated with `./gradlew test publishToMavenLocal` in tslearn4j and
`sbt -batch -J-Xmx3G test assembly` in simedape-scala:

* Java: 135 tests passed, zero failures/errors/skips.
* Scala: 34 tests in 14 suites passed, zero failures/errors/skips.
* All 100 occupancy estimation variants checked against Python outputs, including
  normalization, interpolation, PCA delta and eigenvalue filtering.
* Exact DTW: 50 Python-produced cases covering unequal lengths, ties, narrow bands
  and relaxed endpoints. Selection: 120 cases spanning ten strategies.
* Multivariable: joint/separate/grouped patterns, all four model families,
  masked PCA, NumPy-compatible masks, paired calibration, provenance and reloads.
  The complete held-out runner is checked against a twelve-model Python fixture.
* Microscopic CSV, Parquet, Parquet-directory and JSONL aggregation checked against
  Python over three configurations, including identities beyond float64 precision.
* Isolated ClickHouse-compatible HTTP fixture tests authentication, typed queries,
  occupancy integration, empty links and inconsistent source rejection.
* Native pipeline trains, exports/reloads, estimates, compares and produces reports.
  Sweep plots and partial-source failure isolation are exercised too.
* All 66 external HTC control-route declarations are exercised against an isolated
  mock. Native job ownership, query/body forwarding, external job registration,
  CORS and an unconfigured backend's explicit 503 response are checked.
* PDF and LaTeX ZIP exports checked structurally; the final 14-page fixture PDF
  was rendered with Poppler and visually inspected for cover/table layout.

Reference regeneration is development-only: `tools/parity/regenerate.sh` uses
an existing image containing the Python numerical stack. Golden outputs are
checked in, so Scala verification does not start Docker or Python.

These checks establish bounded fixture and contract agreement. They do not measure
speedup on the user's long-running dataset, validate every real-world numerical
input, run a real-city accuracy campaign, certify the external HTC service business
logic, or establish browser/UI acceptance. The optional HTC routes are integrations;
those Python business implementations have not been ported into Scala. Shared
services and the reference Python implementation were left running unchanged.
