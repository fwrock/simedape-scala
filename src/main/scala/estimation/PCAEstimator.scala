package org.interscity.htc
package estimation

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.ops.transforms.Transforms

import data.Models.*
import preprocessing.TimeSeriesPreprocessor

/**
 * PCA-based estimator using ND4J for accelerated linear algebra.
 *
 * Algorithm (per cluster)
 * ──────────────────────
 * OFFLINE (base scenario):
 *   1. Stack the normalised time series of the cluster → matrix X [n × T]
 *   2. Centre X: X_c = X - mean(X, axis=0)
 *   3. SVD: X_c = U · S · Vᵀ  (economy / thin SVD)
 *   4. Keep top-k components: V_k = V[:, :k]  [T × k]
 *   5. Store (X_mean, V_k) per cluster
 *
 * ONLINE (partial scenario):
 *   For each non-simulated series j in cluster:
 *   1. Get normalised simulation point series s_new  [T]
 *   2. Project:  α = V_k^T · (s_new - X_mean)       [k]
 *   3. Reconstruct: ẑ_j = V_k · α + X_mean          [T]
 *      (Note: we use the sim-point projection because it's the only
 *       available series in the new scenario for this cluster.)
 *   4. De-normalise with target series' μ, σ from base
 *
 * This is equivalent to sklearn's PCA.transform() + inverse_transform()
 * but uses ND4J for performance on large matrices.
 */
object PCAEstimator:

  /**
   * Fits per-cluster PCA models and returns them as a map.
   *
   * @param clustering  cluster assignment result
   * @param dataset     normalised base dataset
   * @param nComponents number of PCA components to retain per cluster
   * @return map from cluster id → fitted PCAModel
   */
  def fitClusters(
      clustering: ClusteringResult,
      dataset: PreprocessedDataset,
      nComponents: Int,
  ): Map[Int, PCAModel] =
    (0 until clustering.nClusters).map { k =>
      val members = clustering.membersOf(k)
      k -> fitCluster(members.map(dataset.normalised), nComponents)
    }.toMap

  /**
   * Estimates all non-simulated series in a cluster using its PCA model.
   *
   * @param model       fitted PCA model for this cluster
   * @param simPointNew normalised sim-point series from the new scenario
   * @param memberCount number of members in the cluster
   * @param baseMeans   μ per member series (from base normalisation)
   * @param baseStds    σ per member series
   * @return array of estimated series (one per cluster member)
   */
  def estimateCluster(
      model: PCAModel,
      simPointNew: Array[Double],
      memberCount: Int,
      baseMeans: Array[Double],
      baseStds: Array[Double],
  ): Array[Array[Double]] =
    val t = simPointNew.length

    // Project the sim-point series through the PCA model
    val sCentered = Nd4j.create(simPointNew).subi(model.mean)         // [T]
    val alpha     = model.components.mmul(sCentered.reshape(t, 1))    // [k × 1]
    val zHat      = model.components.transpose().mmul(alpha)          // [T × 1]
    val reconstructed = zHat.reshape(t).addi(model.mean)              // [T]

    val zArr = reconstructed.toDoubleVector

    // De-normalise for every cluster member using their individual μ/σ
    Array.tabulate(memberCount) { i =>
      TimeSeriesPreprocessor.prune(
        TimeSeriesPreprocessor.denormalize(zArr, baseMeans(i), baseStds(i))
      )
    }

  // ── Internal model + fitting ───────────────────────────────────────────────

  /**
   * Fitted PCA model for one cluster.
   *
   * @param mean       column-wise mean of the training matrix    [T]
   * @param components top-k right singular vectors               [k × T]
   */
  final case class PCAModel(mean: INDArray, components: INDArray)

  private def fitCluster(series: Array[Array[Double]], nComponents: Int): PCAModel =
    val n = series.length
    val t = series(0).length

    // Build matrix X [n × T]
    val X = Nd4j.create(series.flatten.map(_.toFloat).toArray, Array(n, t))

    // Centre
    val mean   = X.mean(0)           // [1 × T]
    val Xc     = X.subRowVector(mean) // [n × T]

    // SVD via ND4J (thin)
    val svdParts = Nd4j.linalg().svd(Xc, true, true).asInstanceOf[Array[INDArray]]
    // svdParts(0) = U [n×min], svdParts(1) = S [min], svdParts(2) = Vt [min×T]
    val Vt  = svdParts(2)            // [min × T]

    val k = nComponents.min(Vt.rows().toInt)
    val Vk = Vt.get(
      org.nd4j.linalg.indexing.NDArrayIndex.interval(0, k),
      org.nd4j.linalg.indexing.NDArrayIndex.all(),
    ) // [k × T]

    PCAModel(mean = mean.reshape(t), components = Vk)
