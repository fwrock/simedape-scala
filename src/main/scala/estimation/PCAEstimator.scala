package org.interscity.htc
package estimation

import data.Models.*
import preprocessing.TimeSeriesPreprocessor
import org.tslearn.utils.NativeLinearAlgebra

/** Double-precision PCA using OpenBLAS covariance and LAPACK symmetric eigenvectors.
  * The decomposition operates on the T x T feature covariance, avoiding an N x N
  * matrix for large numbers of short series. Components are stored as [k x T].
  */
object PCAEstimator:
  final case class PCAModel(mean: Array[Double], components: Array[Array[Double]])

  def fitClusters(clustering: ClusteringResult, dataset: PreprocessedDataset,
                  nComponents: Int): Map[Int, PCAModel] =
    require(nComponents > 0, "nComponents must be positive")
    (0 until clustering.nClusters).map { k =>
      val members = clustering.membersOf(k)
      require(members.nonEmpty, s"Cannot fit PCA for empty cluster $k")
      k -> fitCluster(members.map(dataset.normalised), nComponents)
    }.toMap

  def estimateCluster(model: PCAModel, simPointNew: Array[Double], memberCount: Int,
                      baseMeans: Array[Double], baseStds: Array[Double]): Array[Array[Double]] =
    val t = model.mean.length
    require(simPointNew.length == t, "PCA input length mismatch")
    require(memberCount == baseMeans.length && memberCount == baseStds.length,
            "PCA member statistics mismatch")
    val centred = Array.tabulate(t)(i => simPointNew(i) - model.mean(i))
    val reconstructed = model.mean.clone()
    for component <- model.components do
      var weight = 0.0
      var j = 0
      while j < t do
        weight += component(j) * centred(j)
        j += 1
      j = 0
      while j < t do
        reconstructed(j) += component(j) * weight
        j += 1
    Array.tabulate(memberCount) { i =>
      TimeSeriesPreprocessor.prune(
        TimeSeriesPreprocessor.denormalize(reconstructed, baseMeans(i), baseStds(i)))
    }

  private[estimation] def fitCluster(series: Array[Array[Double]], nComponents: Int): PCAModel =
    val n = series.length
    val t = series(0).length
    require(t > 0 && series.forall(_.length == t), "Invalid PCA dataset shape")
    val mean = new Array[Double](t)
    for row <- series; j <- 0 until t do mean(j) += row(j) / n
    val centred = new Array[Double](Math.multiplyExact(n, t))
    for i <- 0 until n; j <- 0 until t do centred(i * t + j) = series(i)(j) - mean(j)
    val covariance = NativeLinearAlgebra.gram(centred, n, t)
    val components = NativeLinearAlgebra.leadingEigenvectors(covariance, t, nComponents.min(n).min(t))
    PCAModel(mean, components)
