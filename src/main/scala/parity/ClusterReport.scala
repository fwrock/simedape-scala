package org.interscity.htc.parity
import spray.json.*
import Json.*
import scala.collection.mutable

final case class ReportArray(shape: Vector[Int],values: Array[Double],dtype: String="float32")
final case class ClusterReport(summary: JsObject,arrays: Map[String,ReportArray]):
  def api(cluster: Option[Int],near: Int=5,far: Int=5): JsObject =
    def matrix(name: String): JsArray =
      val a=arrays(name);val columns=a.shape.last
      JsArray(a.values.grouped(columns).map(row=>arr(row.toVector)).toVector)
    val base=obj("report"->summary,"centroid_distance"->matrix("centroid_distance"))
    cluster.fold(base) { k=>
      require(k>=0 && k<summary.field("n_clusters").int(),"Cluster does not exist")
      val info=summary.field("clusters").values(k)
      def examples(kind: String,count: Int): JsArray =
        val rows=matrix(s"cluster_$k/${kind}_series").elements
        JsArray(info.field(kind).values.zip(rows).take(math.max(0,math.min(count,summary.field("top").int()))).map((m,s)=>JsObject(m.asJsObject.fields+("series"->s))))
      JsObject(base.fields+("cluster"->obj("cluster"->JsNumber(k),"centroid"->arr(arrays(s"cluster_$k/centroid_series").values.toVector),"near"->examples("near",near),"far"->examples("far",far))))
    }

object ClusterReport:
  def stats(xs: Iterable[Double]): JsObject =
    val v=xs.toVector
    obj("mean"->num(if v.isEmpty then 0 else v.sum/v.size),"median"->num(if v.isEmpty then 0 else Metrics.quantile(v,.5)),"p90"->num(if v.isEmpty then 0 else Metrics.quantile(v,.9)),"max"->num(v.maxOption.getOrElse(0.0)))
  def build(model: TrainedModel,data: Prepared,top: Int=20): ClusterReport =
    val arrays=mutable.Map.empty[String,ReportArray]
    val n=model.size;val count=model.clusters.count;val length=model.length
    val distances=Array.tabulate(n)(i=>math.sqrt(data.normalized(i).indices.map(j=>math.pow(data.normalized(i)(j)-model.clusters.centroids(model.clusters.labels(i))(j),2)).sum))
    val rmse=Array.tabulate(n)(i=>distances(i)*data.stds(i)/math.sqrt(length))
    val relative=Array.tabulate(n)(i=>rmse(i)/math.max(math.abs(data.means(i)),1e-9)*100)
    arrays("distance")=ReportArray(Vector(n),distances);arrays("rmse")=ReportArray(Vector(n),rmse)
    val cd=Array.tabulate(count*count) { i=>
      val a=model.clusters.centroids(i/count);val b=model.clusters.centroids(i%count)
      math.sqrt(a.indices.map(j=>math.pow(a(j)-b(j),2)).sum)
    }
    arrays("centroid_distance")=ReportArray(Vector(count,count),cd,"float64")
    val clusters=Vector.tabulate(count) { k=>
      val members=model.clusters.members(k).toVector;val sorted=members.sortBy(i=>(distances(i),i))
      val near=sorted.take(top);val far=sorted.reverse.take(top)
      for (kind,indices)<-Vector("near"->near,"far"->far) do
        arrays(s"cluster_$k/${kind}_idx")=ReportArray(Vector(indices.size),indices.map(_.toDouble).toArray,"int64")
        arrays(s"cluster_$k/${kind}_series")=ReportArray(Vector(indices.size,length),indices.flatMap(data.raw).toArray)
      val mu=members.map(model.means).sum/members.size;val sigma=members.map(model.stds).sum/members.size
      arrays(s"cluster_$k/centroid_series")=ReportArray(Vector(length),model.clusters.centroids(k).map(_*sigma+mu))
      val d=members.map(distances);var lo=d.min;var hi=d.max
      if lo==hi then {lo-=.5;hi+=.5}
      val edges=Vector.tabulate(13)(i=>lo+(hi-lo)*i/12);val histogram=Array.fill(12)(0)
      d.foreach(x=>histogram(math.min(11,math.max(0,((x-lo)/(hi-lo)*12).toInt)))+=1)
      def examples(indices: Vector[Int]): JsArray = JsArray(indices.map(i=>obj("index"->JsNumber(i),"label"->JsString(model.labels(i)),"distance"->num(distances(i)),"rmse"->num(rmse(i)))))
      val nearest=if count<=1 then None else Some((0 until count).filter(_!=k).minBy(j=>(cd(k*count+j),j)))
      obj("cluster"->JsNumber(k),"size"->JsNumber(members.size),"pct"->num(math.rint(100000.0*members.size/n)/1000),
        "distance"->stats(d),"rmse"->stats(members.map(rmse)),"rel_error_pct"->stats(members.map(relative)),"mean_level"->num(mu),
        "histogram"->obj("counts"->ints(histogram.toVector),"edges"->arr(edges)),"nearest_cluster"->nearest.map(JsNumber(_)).getOrElse(JsNull),
        "nearest_cluster_distance"->nearest.map(j=>num(cd(k*count+j))).getOrElse(JsNull),"near"->examples(near),"far"->examples(far))
    }
    ClusterReport(obj("n_series"->JsNumber(n),"n_clusters"->JsNumber(count),"series_length"->JsNumber(length),"top"->JsNumber(top),"inertia"->num(distances.map(x=>x*x).sum),
      "overall"->obj("distance"->stats(distances.toVector),"rmse"->stats(rmse.toVector),"rel_error_pct"->stats(relative.toVector)),"clusters"->JsArray(clusters)),arrays.toMap)
  def enrich(report: ClusterReport,model: TrainedModel): ClusterReport =
    val rows=report.summary.field("clusters").values.map { row=>
      val k=row.field("cluster").int();val additions=mutable.Map.empty[String,JsValue]
      model.pca.get(k).foreach(c=>additions("pca_unexplained_pct")=num(math.rint((100-math.rint(c.model.varianceRatio().sum*10000)/100)*1000)/1000))
      model.dtw.foreach { source=>
        val paths=ModelIO.paths(source,k,Vector("offsets","d"))
        val finalCosts=paths.finalCosts
        if finalCosts.nonEmpty then additions("dtw_distance")=stats(finalCosts)
      }
      JsObject(row.asJsObject.fields++additions)
    }
    report.copy(summary=JsObject(report.summary.fields+("clusters"->JsArray(rows))))
