package org.interscity.htc
package data

import spray.json.*
import spray.json.DefaultJsonProtocol.*
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import org.slf4j.LoggerFactory

import core.TimeWindow
import Models.*

/**
 * Saves and loads intermediate pipeline results to/from disk as JSON.
 *
 * File layout: {checkpointDir}/{baseSimId}[_{windowLabel}]/{phase}.json
 *
 * Supported checkpoints:
 *  - preprocessed_base.json : PreprocessedDataset (after z-score normalisation)
 *  - clustering.json        : ClusteringResult (labels, centroids, sim-point indices)
 *  - warping_paths.json     : WarpingPathsResult (DTW paths per cluster)
 *
 * Use startFromPhase / stopAfterPhase in the pipeline config to control which
 * phases are skipped or halted, combined with checkpoints for resumption.
 */
object CheckpointManager:

  private val log = LoggerFactory.getLogger(getClass)

  // ─── Local spray-json formats ──────────────────────────────────────────────

  private given RootJsonFormat[WarpingStep] = jsonFormat4(WarpingStep.apply)

  private given RootJsonFormat[ClusteringResult] = new RootJsonFormat[ClusteringResult]:
    def write(r: ClusteringResult): JsValue = JsObject(
      "nClusters"       -> r.nClusters.toJson,
      "labels"          -> r.labels.toList.toJson,
      "centroids"       -> r.centroids.map(_.toList).toList.toJson,
      "simPointIndices" -> r.simPointIndices.toList.toJson,
    )
    def read(v: JsValue): ClusteringResult =
      val obj = v.asJsObject
      ClusteringResult(
        nClusters       = obj.fields("nClusters").convertTo[Int],
        labels          = obj.fields("labels").convertTo[List[Int]].toArray,
        centroids       = obj.fields("centroids").convertTo[List[List[Double]]].map(_.toArray).toArray,
        simPointIndices = obj.fields("simPointIndices").convertTo[List[Int]].toArray,
      )

  private given RootJsonFormat[WarpingPathsResult] = new RootJsonFormat[WarpingPathsResult]:
    def write(r: WarpingPathsResult): JsValue =
      JsObject(r.paths.map { case (clusterId, inner) =>
        clusterId.toString -> JsObject(inner.map { case (seriesIdx, path) =>
          seriesIdx.toString -> path.toJson
        })
      })
    def read(v: JsValue): WarpingPathsResult =
      val outer = v.asJsObject.fields.map { case (cId, inner) =>
        cId.toInt -> inner.asJsObject.fields.map { case (sIdx, path) =>
          sIdx.toInt -> path.convertTo[List[WarpingStep]]
        }
      }
      WarpingPathsResult(outer)

  private given RootJsonFormat[PreprocessedDataset] = new RootJsonFormat[PreprocessedDataset]:
    def write(r: PreprocessedDataset): JsValue = JsObject(
      "simulationId" -> r.simulationId.toJson,
      "keys"         -> r.keys.toList.toJson,
      "normalised"   -> r.normalised.map(_.toList).toList.toJson,
      "means"        -> r.means.toList.toJson,
      "stds"         -> r.stds.toList.toJson,
    )
    def read(v: JsValue): PreprocessedDataset =
      val obj = v.asJsObject
      PreprocessedDataset(
        simulationId = obj.fields("simulationId").convertTo[String],
        keys         = obj.fields("keys").convertTo[List[String]].toIndexedSeq,
        normalised   = obj.fields("normalised").convertTo[List[List[Double]]].map(_.toArray).toArray,
        means        = obj.fields("means").convertTo[List[Double]].toArray,
        stds         = obj.fields("stds").convertTo[List[Double]].toArray,
      )

  // ─── Path helpers ──────────────────────────────────────────────────────────

  private def checkpointPath(
      dir: String,
      baseSimId: String,
      window: Option[TimeWindow],
      filename: String,
  ): Path =
    val windowSuffix = window.fold("")(w => s"_${w.label}")
    Paths.get(dir, s"$baseSimId$windowSuffix", filename)

  private def saveJson[A: JsonWriter](path: Path, value: A): Unit =
    try
      Files.createDirectories(path.getParent)
      Files.writeString(path, value.toJson.prettyPrint, StandardCharsets.UTF_8)
      log.info("Checkpoint saved: {}", path)
    catch
      case e: Exception =>
        log.error("Failed to save checkpoint {}: {}", path, e.getMessage)

  private def loadJson[A: JsonReader](path: Path): Option[A] =
    if Files.exists(path) then
      try
        val json = Files.readString(path, StandardCharsets.UTF_8).parseJson
        Some(json.convertTo[A])
      catch
        case e: Exception =>
          log.warn("Failed to load checkpoint {}: {}", path, e.getMessage)
          None
    else
      log.debug("Checkpoint not found: {}", path)
      None

  // ─── Public API ───────────────────────────────────────────────────────────

  def savePreprocessed(dir: String, baseSimId: String, window: Option[TimeWindow], data: PreprocessedDataset): Unit =
    saveJson(checkpointPath(dir, baseSimId, window, "preprocessed_base.json"), data)

  def loadPreprocessed(dir: String, baseSimId: String, window: Option[TimeWindow]): Option[PreprocessedDataset] =
    loadJson[PreprocessedDataset](checkpointPath(dir, baseSimId, window, "preprocessed_base.json"))

  def saveClustering(dir: String, baseSimId: String, window: Option[TimeWindow], result: ClusteringResult): Unit =
    saveJson(checkpointPath(dir, baseSimId, window, "clustering.json"), result)

  def loadClustering(dir: String, baseSimId: String, window: Option[TimeWindow]): Option[ClusteringResult] =
    loadJson[ClusteringResult](checkpointPath(dir, baseSimId, window, "clustering.json"))

  def saveWarpingPaths(dir: String, baseSimId: String, window: Option[TimeWindow], result: WarpingPathsResult): Unit =
    saveJson(checkpointPath(dir, baseSimId, window, "warping_paths.json"), result)

  def loadWarpingPaths(dir: String, baseSimId: String, window: Option[TimeWindow]): Option[WarpingPathsResult] =
    loadJson[WarpingPathsResult](checkpointPath(dir, baseSimId, window, "warping_paths.json"))
