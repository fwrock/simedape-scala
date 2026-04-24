package org.interscity.htc
package clustering

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import org.tslearn.clustering.KShape
import org.tslearn.utils.MatrixUtils

import core.SimEDaPEConfig
import core.Protocol.*
import data.Models.*

/**
 * Wraps tslearn4j's KShape clustering algorithm.
 *
 * Input:  PreprocessedDataset   — z-normalised time series
 * Output: ClusteringResult      — labels, centroids, simulation point per cluster
 *
 * Simulation point selection
 * ──────────────────────────
 * For each cluster k, the series with minimum MAE to the centroid is chosen
 * as the "simulation point" — the only series that will be physically
 * simulated in the partial scenario.
 */
object KShapeClusterer:

  def apply(cfg: SimEDaPEConfig): Behavior[ClustererCommand] =
    Behaviors.receiveMessage {
      case RunClustering(dataset, replyTo) =>
        val result = scala.util.Try(cluster(dataset, cfg))
        result.fold(
          err => replyTo ! ClusteringFailed(err),
          r   => replyTo ! ClusteringDone(r),
        )
        Behaviors.same
    }

  // ── core algorithm ────────────────────────────────────────────────────────

  private def cluster(dataset: PreprocessedDataset, cfg: SimEDaPEConfig): ClusteringResult =
    val n      = dataset.size
    val length = dataset.normalised(0).length

    // Convert to tslearn4j format: RealMatrix[] where each is [length × 1]
    val tsData = dataset.normalised.map { series =>
      val matrix = Array.tabulate(length, 1)((i, _) => series(i))
      MatrixUtils.toTimeSeriesDataset(matrix)(0)
    }

    val kshape = new KShape(
      cfg.nClusters,
      cfg.kshapeMaxIter,
      1e-6,
      cfg.kshapeNInit,
      false,    // verbose
      42L,      // random_state
      "random",
    )
    kshape.fit(tsData)

    val labels    = kshape.getLabels
    val jCentroids = kshape.getClusterCenters

    // Extract centroids as flat double arrays
    val centroids = Array.tabulate(cfg.nClusters) { k =>
      val m = jCentroids(k)
      Array.tabulate(m.getRowDimension)(i => m.getEntry(i, 0))
    }

    // For each cluster: pick series with minimum MAE to centroid
    val simPointIndices = Array.tabulate(cfg.nClusters) { k =>
      val members  = labels.zipWithIndex.collect { case (label, idx) if label == k => idx }
      val centroid = centroids(k)
      members.minByOption(idx => mae(dataset.normalised(idx), centroid))
        .getOrElse(members.headOption.getOrElse(0))
    }

    ClusteringResult(
      nClusters       = cfg.nClusters,
      labels          = labels,
      centroids       = centroids,
      simPointIndices = simPointIndices,
    )

  // ── helpers ───────────────────────────────────────────────────────────────

  private def mae(a: Array[Double], b: Array[Double]): Double =
    require(a.length == b.length, s"Length mismatch: ${a.length} vs ${b.length}")
    a.zip(b).map { case (x, y) => math.abs(x - y) }.sum / a.length
