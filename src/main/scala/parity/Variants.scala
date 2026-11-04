package org.interscity.htc.parity

import spray.json.*
import Json.*
import scala.util.Using

final case class Variant(key: String,family: String,method: String,interpolation: Option[String],eigenvalueFilter: Boolean,metadata: JsObject)
object Variants:
  val catalog: Vector[Variant] = Using.resource(getClass.getResourceAsStream("/parity/variants.json")) { source =>
    require(source!=null,"Variant catalogue is missing")
    new String(source.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson.values.map { value =>
      Variant(value.field("key").text(),value.field("family").text(),value.field("method").text(),
        Option(value.field("interp")).filter(_!=JsNull).map(_.text()),value.field("eig").bool(),value.asJsObject)
    }
  }
  require(catalog.size==100 && catalog.map(_.key).distinct.size==100,"Invalid variant catalogue")
  private val index=catalog.map(v=>v.key->v).toMap
  def get(key: String): Variant = index.getOrElse(key,throw IllegalArgumentException(s"Unknown estimation variant: $key"))
  def resolve(keys: Vector[String]): Vector[Variant] = (if keys==Vector("all") then catalog else keys.distinct.map(get))

final case class Missing(simulationPoints: Int=0,series: Int=0)
object Estimator:
  def preparationKey(variant: Variant,model: TrainedModel,partial: Dataset): (Int,String) =
    if variant.family=="dtw" then (model.length,variant.interpolation.getOrElse("tslearn"))
    else if variant.family=="pca" && variant.method=="sp" && variant.interpolation.isEmpty then (model.length,"tslearn")
    else (partial.maxSize,"tslearn")
  def estimate(model: TrainedModel,partial: Prepared,variant: Variant,percentage: Double)(emit: (String,Array[Double])=>Unit): Missing =
    require(percentage>=0 && percentage<=100,"percentage_sample must be in [0,100]")
    def prune(values: Array[Double]): Array[Double] = values.map(v=>if v<0 then 0.0 else v)
    def filtered(cluster: PCACluster,weights: Array[Double]): Array[Double] = if variant.eigenvalueFilter then cluster.model.filter(weights) else weights
    var missingSP=0;var missingSeries=0
    val observed=partial.dataset.index
    if variant.family=="dtw" then
      val source=model.dtw.getOrElse(throw IllegalArgumentException("Model has no DTW paths"))
      for k<-0 until model.clusters.count do
        val members=model.clusters.members(k);val sp=model.labels(model.clusters.simulationPoints(k))
        if !observed.contains(sp) then missingSP+=members.length
        else
          val paths=ModelIO.paths(source,k)
          val spSeries=partial.normalized(observed(sp))
          members.zipWithIndex.foreach { (target,local) =>
            val label=model.labels(target)
            observed.get(label) match
              case None => missingSeries+=1
              case Some(index) =>
                val (mu,sigma)=variant.method match
                  case "base" => (model.means(target),model.stds(target))
                  case "sample" => (partial.means(index),partial.stds(index))
                  case "avg" => ((model.means(target)+partial.means(index))/2,(model.stds(target)+partial.stds(index))/2)
                  case "wavg" =>
                    val p=percentage/100
                    ((model.means(target)+p*partial.means(index))/(1+p),(model.stds(target)+p*partial.stds(index))/(1+p))
                  case _ => throw IllegalArgumentException("Invalid DTW normalization")
                val translated=paths.translate(local,if variant.method=="base" then model.clusters.centroids(k) else spSeries)
                emit(label,prune(translated.map(_*sigma+mu)))
          }
    else
      require(model.pca.nonEmpty,"Model has no PCA models")
      val clusterWeights=scala.collection.mutable.Map.empty[Int,Array[Double]]
      if variant.method=="sp" || (variant.family=="pca_delta" && variant.method!="base") then
        model.pca.foreach { (k,cluster) =>
          observed.get(model.labels(model.clusters.simulationPoints(k))).foreach { index =>
            val weights=cluster.model.weights(partial.raw(index),variant.interpolation.nonEmpty,variant.interpolation.getOrElse("linear"))
            clusterWeights(k)=weights
          }
        }
      model.labels.zipWithIndex.foreach { (label,target) =>
        val link=label.replaceFirst("-[0-9]+-[0-9]+$", "")
        if !model.roads.contains(link) then missingSeries+=1
        else
          val clusterId=model.clusters.labels(target)
          val reconstruction: Option[Array[Double]] =
            if variant.family=="pca_delta" then model.pca.get(clusterId).flatMap { cluster =>
              val local=java.util.Arrays.binarySearch(cluster.members,target)
              if local<0 then None
              else
                val train=cluster.trainWeights(local)
                val weights=if variant.method=="base" then Some(train.clone()) else clusterWeights.get(clusterId).filter(_.length==cluster.spWeights.length).map { current =>
                  val scale=variant.method match
                    case "delta" => 1.0
                    case "avg" => .5
                    case "wavg" => percentage/100
                    case _ => throw IllegalArgumentException("Invalid PCA delta method")
                  Array.tabulate(train.length)(i=>train(i)+scale*(current(i)-cluster.spWeights(i)))
                }
                weights.map(w=>cluster.model.reconstruct(filtered(cluster,w),model.length))
            }
            else if variant.method=="sp" then model.pca.get(clusterId).flatMap(c=>clusterWeights.get(clusterId).map(w=>c.model.reconstruct(filtered(c,w),model.length)))
            else observed.get(label).flatMap { index =>
              val raw=partial.raw(index)
              val assigned=model.clusters.centroids.indices.minBy { k =>
                val centroid=model.clusters.centroids(k)
                require(raw.length<=centroid.length,"Partial horizon exceeds trained horizon")
                raw.indices.iterator.map(i=>math.pow(raw(i)-centroid(i),2)).sum
              }
              model.pca.get(assigned).map { cluster =>
                val weights=cluster.model.weights(raw,variant.method=="stretched",variant.interpolation.getOrElse("linear"))
                cluster.model.reconstruct(filtered(cluster,weights),model.length)
              }
            }
          reconstruction match
            case Some(values) => emit(label,prune(values))
            case None => missingSeries+=1
      }
    Missing(missingSP,missingSeries)

  /** One path read and one translation per target for every DTW normalization in a preparation group. */
  def dtwBatch(model: TrainedModel,partial: Prepared,variants: Vector[Variant],percentage: Double)(emit: (Variant,String,Array[Double])=>Unit): Missing =
    require(variants.forall(_.family=="dtw") && percentage>=0 && percentage<=100,"Invalid DTW batch")
    val source=model.dtw.getOrElse(throw IllegalArgumentException("Model has no DTW paths"))
    val observed=partial.dataset.index;var missingSP=0;var missingSeries=0
    for k<-0 until model.clusters.count do
      val members=model.clusters.members(k);val sp=model.labels(model.clusters.simulationPoints(k))
      if !observed.contains(sp) then missingSP+=members.length
      else
        val paths=ModelIO.paths(source,k);val spSeries=partial.normalized(observed(sp))
        members.zipWithIndex.foreach { (target,local)=>
          val label=model.labels(target)
          observed.get(label) match
            case None=>missingSeries+=1
            case Some(index)=>
              val translated=if variants.exists(_.method!="base") then paths.translate(local,spSeries) else Array.empty[Double]
              val baseline=if variants.exists(_.method=="base") then paths.translate(local,model.clusters.centroids(k)) else Array.empty[Double]
              variants.foreach { v=>
                val factor=v.method match
                  case "base"=>0.0
                  case "sample"=>Double.PositiveInfinity
                  case "avg"=>1.0
                  case "wavg"=>percentage/100
                  case _=>throw IllegalArgumentException("Invalid normalization")
                val (mean,std)=if factor.isPosInfinity then (partial.means(index),partial.stds(index)) else ((model.means(target)+factor*partial.means(index))/(1+factor),(model.stds(target)+factor*partial.stds(index))/(1+factor))
                emit(v,label,(if v.method=="base" then baseline else translated).map(x=>math.max(0,x*std+mean)))
              }
        }
    Missing(missingSP,missingSeries)

  def standalonePCA(model: TrainedModel,partial: Prepared,mode: String)(emit: (String,Array[Double])=>Unit): Missing =
    if mode=="sp" then estimate(model,partial,Variants.get("pca_sp"),100)(emit)
    else
      var missing=0
      partial.dataset.series.indices.foreach { i=>
        val row=partial.dataset.series(i);val raw=partial.raw(i)
        if !model.roads.contains(row.link) then missing+=1
        else
          val assigned=model.clusters.centroids.indices.minBy(k=>raw.indices.map(j=>math.pow(raw(j)-model.clusters.centroids(k)(j),2)).sum)
          model.pca.get(assigned) match
            case None=>missing+=1
            case Some(cluster)=>
              val weights=cluster.model.weights(raw,mode=="stretched","linear")
              emit(row.label,cluster.model.reconstruct(weights,model.length).map(v=>if v<0 then 0.0 else v))
      }
      Missing(series=missing)

  def metrics(model: TrainedModel,partial: Prepared,variant: Variant,percentage: Double): JsObject =
    val rows=Vector.newBuilder[(String,Array[Double])]
    val missing=estimate(model,partial,variant,percentage)((label,values)=>rows+=((label,values)))
    val result=Metrics.result(rows.result(),model.roads,missing.simulationPoints,missing.series)
    if variant.family=="dtw" then result else JsObject(result.fields-"missing_simulation_points")
