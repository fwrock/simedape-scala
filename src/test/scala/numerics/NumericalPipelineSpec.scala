package org.interscity.htc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKit
import core.SimEDaPEConfig
import core.Protocol.*
import data.Models.*
import clustering.KShapeClusterer
import estimation.{PCAEstimator, TimeTranslation}
import preprocessing.TimeSeriesPreprocessor
import warping.DTWComputer

class NumericalPipelineSpec extends AnyFlatSpec with Matchers:
  "DTW" should "preserve the initial alignment and identical-series path" in {
    val values = Array(3.0,1.0,4.0,1.0,5.0,9.0,2.0,6.0)
    val (path,distance) = DTWComputer.warpingPath(values,values)
    distance shouldBe 0.0
    path.map(s => (s.i,s.j)) shouldBe values.indices.map(i => (i,i)).toList
  }
  it should "use relaxed endpoints and the zero-window convention of dtaidistance" in {
    val a = Array(0.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0)
    val b = Array(0.0,0.0,1.0,2.0,3.0,4.0,90.0,90.0)
    val (path,distance) = DTWComputer.warpingPath(a,b)
    distance shouldBe math.sqrt(5.0) +- 1e-12
    path.last.j should be < 6
    path.head.i shouldBe 0
  }
  "PCA" should "reconstruct a rank-one dataset using the native backend" in {
    val values = Array(Array(1.0,2.0,3.0,4.0),Array(2.0,3.0,4.0,5.0),Array(3.0,4.0,5.0,6.0))
    val ds = PreprocessedDataset("fixture",Vector("a","b","c"),values,Array.fill(3)(0.0),Array.fill(3)(1.0))
    val clustering = ClusteringResult(1,Array(0,0,0),Array(values(0)),Array(0))
    val model = PCAEstimator.fitClusters(clustering,ds,1)(0)
    val result = PCAEstimator.estimateCluster(model,values(0),3,ds.means,ds.stds)
    result.foreach(row => row.zip(values(0)).foreach((a,b) => a shouldBe b +- 1e-10))
  }
  "Clustering actor" should "retain every input point in each centroid" in {
    val kit = ActorTestKit(com.typesafe.config.ConfigFactory.load())
    try
      val cfg = SimEDaPEConfig.fromTypesafe().copy(nClusters=1,kshapeMaxIter=5,seriesLength=8)
      val values = Array(-1.0,0.0,1.0,0.0,-1.0,0.0,1.0,0.0)
      val ds = PreprocessedDataset("fixture",Vector("a","b"),Array(values,values.clone()),Array(0.0,0.0),Array(1.0,1.0))
      val actor = kit.spawn(KShapeClusterer(cfg))
      val probe = kit.createTestProbe[ClustererReply]()
      actor ! RunClustering(ds,probe.ref)
      val result = probe.expectMessageType[ClusteringDone].result
      result.centroids(0).length shouldBe 8
    finally kit.shutdownTestKit()
  }
  "Time translation" should "preserve cumulative-cost reconstruction" in {
    val path = List(WarpingStep(0,0,0.0,false),WarpingStep(1,1,3.0,true),WarpingStep(2,2,5.0,false))
    TimeTranslation.translate(path,Array(10.0,10.0,10.0)).toSeq shouldBe Seq(10.0,7.0,14.0)
  }

  "PCA estimation worker" should "run without DTW paths when PCA is selected" in {
    val kit = ActorTestKit(com.typesafe.config.ConfigFactory.load())
    try
      val values = Array(Array(1.0,2.0,3.0,4.0),Array(2.0,3.0,4.0,5.0),Array(3.0,4.0,5.0,6.0))
      val probe = kit.createTestProbe[EstimationWorkerReply]()
      val worker = kit.spawn(estimation.EstimationWorker())
      worker ! EstimateCluster(0,0,Array(0,1,2),Array.fill(3)(0.0),Array.fill(3)(1.0),
        Array.fill(3)(0.0),Array.fill(3)(1.0),values(0),Map.empty,25.0,probe.ref,
        core.EstimationMethod.PCA,values,1)
      val result = probe.expectMessageType[ClusterEstimationDone]
      result.estimates.size shouldBe 3
      result.estimates.values.foreach(row => row.zip(values(0)).foreach((a,b) => a shouldBe b +- 1e-10))
    finally kit.shutdownTestKit()
  }
  "Warping supervisor" should "complete every cluster with a single worker slot" in {
    val kit = ActorTestKit(com.typesafe.config.ConfigFactory.load())
    try
      val cfg = SimEDaPEConfig.fromTypesafe().copy(workerParallelism=1)
      val values = Array(3.0,1.0,4.0,1.0,5.0,9.0,2.0,6.0)
      val ds = PreprocessedDataset("fixture",Vector("a","b","c"),Array.fill(3)(values),Array.fill(3)(0.0),Array.fill(3)(1.0))
      val clusters = ClusteringResult(3,Array(0,1,2),Array.fill(3)(values),Array(0,1,2))
      val reply = kit.createTestProbe[WarpingSupervisorReply]()
      val progress = kit.createTestProbe[PipelineEvent]()
      kit.spawn(warping.WarpingPathSupervisor(cfg)) ! ComputeWarpingPaths(clusters,ds,reply.ref,progress.ref)
      reply.expectMessageType[WarpingPathsDone].paths.paths.size shouldBe 3
    finally kit.shutdownTestKit()
  }
