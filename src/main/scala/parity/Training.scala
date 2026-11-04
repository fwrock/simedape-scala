package org.interscity.htc.parity

import org.tslearn.clustering.KShape
import org.tslearn.utils.{MatrixUtils,TimeSeriesMatrix,NumpyRandomState,EmptyClusterException}
import org.tslearn.preprocessing.TimeSeriesResampler
import org.tslearn.decomposition.PCAModel
import org.apache.commons.math3.linear.RealMatrix
import java.nio.file.Path
import java.util.concurrent.{Executors,Future,Callable}
import scala.collection.mutable
import org.interscity.htc.warping.DTWComputer
import spray.json.*
import Json.*

object Training:
  def cluster(data: Prepared,cfg: RunConfig,progress: String=>Unit=_=>()): TrainedModel =
    require(data.normalized.length>=cfg.clusters,"More clusters than series")
    val size=data.normalized(0).length
    val reduced=if cfg.clusteringSize>0 && cfg.clusteringSize<size then
      data.normalized.map(row=>TimeSeriesResampler.resample(row,cfg.clusteringSize,"tslearn")) else data.normalized
    val matrices=reduced.map(row=>TimeSeriesMatrix.wrapColumn(row): RealMatrix)
    val random=new NumpyRandomState(cfg.seed)
    var attempt=0;var best: Option[KShape]=None;var last: Option[KShape]=None
    while attempt<10 && best.isEmpty do
      attempt+=1
      val indices=random.permutation(matrices.length).take(cfg.clusters)
      val initial=indices.map(i=>matrices(i).copy())
      val candidate=new KShape(cfg.clusters,cfg.iterations,1e-6,1,false,cfg.seed,initial)
      last=Some(candidate)
      try best=Some(candidate.fit(matrices))
      catch case _: EmptyClusterException => progress(s"Retrying K-Shape initialization $attempt/10")
    val fitted=best.orElse(last).getOrElse(throw IllegalArgumentException("K-Shape did not initialize"))
    val active=fitted.getLabels.distinct.sorted
    require(active.nonEmpty,"K-Shape returned no occupied clusters")
    if active.length<cfg.clusters then progress(s"Retaining ${active.length}/${cfg.clusters} occupied clusters, matching Python")
    val relabel=active.zipWithIndex.toMap
    val labels=fitted.getLabels.map(relabel)
    val centroids=active.map(k=>TimeSeriesResampler.resample(MatrixUtils.matrixToArray(fitted.getClusterCenters()(k)),size,"tslearn"))
    val points=centroids.indices.map { k =>
      val members=labels.indices.filter(labels(_)==k)
      require(members.nonEmpty,s"Empty cluster $k")
      members.minBy(i=>data.normalized(i).indices.iterator.map(j=>math.abs(data.normalized(i)(j)-centroids(k)(j))).sum)
    }.toArray
    val ch=cfg.json.field("clickhouse",empty).asJsObject
    val safeConfig=JsObject(cfg.json.fields+("clickhouse"->JsObject(ch.fields-"password")))
    val model=TrainedModel(safeConfig,data.dataset.series.map(_.label),Clusters(labels,centroids,points),data.means,data.stds,data.dataset.roads,data.dataset.series.map(_.vehicles))
    model.copy(report=Some(ClusterReport.build(model,data)),streetVehicles=data.dataset.streetVehicles)

  def pca(model: TrainedModel,data: Prepared,components: Int,progress: String=>Unit=_=>()): TrainedModel =
    require(components>0,"n_components must be positive")
    val fitted=(0 until model.clusters.count).map { k =>
      val members=model.clusters.members(k);val raw=members.map(data.raw)
      val pca=PCAModel.fit(raw,components)
      val weights=raw.map(pca.project)
      val sp=members.indexOf(model.clusters.simulationPoints(k))
      require(sp>=0,"Simulation point is outside its cluster")
      progress(s"PCA cluster ${k+1}/${model.clusters.count}: ${pca.components().length} components")
      k->PCACluster(pca,members,weights,weights(sp).clone())
    }.toMap
    val updated=model.copy(pca=fitted)
    updated.copy(report=updated.report.map(r=>ClusterReport.enrich(r,updated)))

  def warping(model: TrainedModel,data: Prepared,folder: Path,workers: Int=1,precision: String="float64",progress: String=>Unit=_=>(),check: ()=>Unit=()=>()): TrainedModel =
    require(workers>0,"n_jobs must be positive")
    val pool=Executors.newFixedThreadPool(workers)
    try
      for k<-0 until model.clusters.count do
        check()
        val members=model.clusters.members(k).iterator
        val pending=mutable.Queue.empty[Future[List[org.interscity.htc.data.Models.WarpingStep]]]
        def submit(): Unit =
          if members.hasNext then
            val i=members.next()
            pending.enqueue(pool.submit(new Callable[List[org.interscity.htc.data.Models.WarpingStep]]:
              def call(): List[org.interscity.htc.data.Models.WarpingStep] =
                check();DTWComputer.warpingPath(model.clusters.centroids(k),data.normalized(i))._1
            ))
        for _<-0 until workers*2 do submit()
        val results=new Iterator[List[org.interscity.htc.data.Models.WarpingStep]]:
          def hasNext: Boolean = pending.nonEmpty
          def next(): List[org.interscity.htc.data.Models.WarpingStep] =
            check()
            val result=pending.dequeue().get()
            submit();result
        ModelIO.writePaths(folder,k,model.clusters.members(k).length,results,precision)
        progress(s"DTW cluster ${k+1}/${model.clusters.count}")
      val updated=model.copy(dtw=Some(folder))
      updated.copy(report=updated.report.map(r=>ClusterReport.enrich(r,updated)))
    finally pool.shutdownNow()
