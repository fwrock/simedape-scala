package org.interscity.htc.parity.experiments

import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import org.tslearn.utils.NumpyGenerator
import scala.collection.mutable

final case class PatternCluster(members: Vector[Int],medoid: Int,center: Array[Double],basis: MaskedPCA)
final case class PatternBlock(context: String,columns: Vector[Int],ids: Vector[Int],assignment: Array[Int],clusters: Vector[PatternCluster],distances: Array[Array[Double]])

final class TrainingPrior(training: SeriesDataset):
  private val byContext=training.groups.distinct.map { group=>group->MaskedPCA.mean(training.groups.indices.filter(training.groups(_)==group).map(training.values).toArray) }.toMap
  private val byLabel=training.groups.zip(training.labels).distinct.map { key=>key->MaskedPCA.mean(training.labels.indices.filter(i=>(training.groups(i),training.labels(i))==key).map(training.values).toArray) }.toMap
  def template(label: String,group: String): (Array[Double],Array[Boolean]) =
    val local=byLabel.getOrElse((group,label),Array.fill(training.t*training.p)(Double.NaN)).clone()
    val context=byContext.getOrElse(group,Array.fill(local.length)(Double.NaN));val fallback=Array.fill(local.length)(false)
    for j<-local.indices do if !local(j).isFinite && context(j).isFinite then {local(j)=context(j);fallback(j)=true}
    (local,fallback)

/** Training-only joint/separate/grouped PCA, DTW, mean and baseline estimators. */
final class PatternEstimator(val settings: JsObject):
  val method=settings.field("method",JsString("pca")).text()
  val clustering=settings.field("clustering",JsString("joint")).text()
  val warping=settings.field("warping",JsString("dependent")).text()
  val scaling=settings.field("scaling",JsString("global")).text()
  val layout=settings.field("pca_layout",JsString("series")).text()
  val fallback=settings.field("coverage_fallback",JsString("training")).text()
  val groupByContext=settings.field("group_by_context").bool()
  val components=settings.field("n_components").int(3)
  val clusters=settings.field("n_clusters").int(2)
  val iterations=settings.field("max_iter").int(20)
  val maximum=settings.field("max_training_series").int(2000)
  val ridge=settings.field("ridge").number(1e-6)
  val seed=settings.field("seed").number().toLong
  val window=Option(settings.field("window")).filter(_!=JsNull).map(_.int())
  require(Set("pca","dtw","mean","baseline")(method) && Set("joint","separate","grouped")(clustering),"Invalid method/clustering")
  require(Set("dependent","independent","euclidean")(warping) && Set("global","none")(scaling) && Set("series","observations")(layout),"Invalid warping/scaling/layout")
  require(Set("none","training")(fallback) && clusters>0 && components>0 && iterations>0 && maximum>0 && ridge>=0 && ridge.isFinite && window.forall(_>=0),"Invalid estimator settings")
  var training: SeriesDataset=null
  var prior: TrainingPrior=null
  var offset=Array.empty[Double]
  var scale=Array.empty[Double]
  var weights=Array.empty[Double]
  var normalized=Array.empty[Array[Double]]
  var blocks=Vector.empty[PatternBlock]
  var diagnostics=Vector.empty[JsValue]
  private def slice(row: Array[Double],cols: Vector[Int]): Array[Double] = Array.tabulate(training.t*cols.size)(j=>row((j/cols.size)*training.p+cols(j%cols.size)))
  private def distance(a: Array[Double],b: Array[Double],cols: Vector[Int]): Double = Distances.distance(a,b,cols.size,warping,window,cols.map(weights).toArray)
  private def medoids(values: Array[Array[Double]],k: Int,cols: Vector[Int]): (Array[Int],Vector[Int],Array[Array[Double]]) =
    val n=values.length;val distances=Array.fill(n,n)(0.0)
    for i<-0 until n;j<-0 until i do
      val d=distance(values(i),values(j),cols);require(d.isFinite,"Training series have no comparable support; partition the experiment")
      distances(i)(j)=d;distances(j)(i)=d
    var representatives=Vector(new NumpyGenerator(seed).integer(n))
    while representatives.size<k do
      val next=(0 until n).maxBy(i=>if representatives.contains(i) then -1.0 else representatives.map(distances(i)(_)).min)
      representatives:+=next
    def assign(): Array[Int] =
      val labels=Array.tabulate(n)(i=>representatives.indices.minBy(c=>distances(i)(representatives(c))))
      representatives.zipWithIndex.foreach((i,c)=>labels(i)=c);labels
    var labels=assign();var iter=0;var done=false
    while iter<iterations && !done do
      labels=assign()
      val updated=representatives.indices.map { c=>
        val members=labels.indices.filter(labels(_)==c)
        members.minBy(i=>members.map(distances(i)(_)).sum)
      }.toVector
      done=updated==representatives;representatives=updated;iter+=1
    (assign(),representatives,distances)
  def fit(data: SeriesDataset): PatternEstimator =
    require(data.n<=maximum,"Training set exceeds explicit pairwise-clustering limit; partition the experiment")
    training=data;prior=new TrainingPrior(data)
    offset=Array.tabulate(data.p) { p=>
      val observed=data.values.flatMap(row=>row.indices.by(data.p).map(j=>row(j+p))).filter(_.isFinite)
      require(observed.nonEmpty,"Each variable needs a training observation");observed.sum/observed.length
    }
    scale=Array.tabulate(data.p) { p=>
      val observed=data.values.flatMap(row=>row.indices.by(data.p).map(j=>row(j+p))).filter(_.isFinite)
      val variance=observed.map(x=>math.pow(x-offset(p),2)).sum/observed.length
      if scaling=="none" || variance<=1e-12 then 1.0 else math.sqrt(variance)
    }
    if scaling=="none" then offset=Array.fill(data.p)(0.0)
    val mapping=settings.field("variable_weights",JsObject(data.variables.map(v=>v->JsNumber(1)).toMap)).asJsObject.fields
    require(mapping.keySet==data.variables.toSet && mapping.values.forall(v=>v.number()>0 && v.number().isFinite),"variable_weights must cover every variable with a positive finite weight")
    weights=data.variables.map(v=>mapping(v).number()).toArray
    val columns=clustering match
      case "joint"=>Vector(data.variables.indices.toVector)
      case "separate"=>data.variables.indices.map(i=>Vector(i)).toVector
      case _=>
        val names=settings.field("variable_groups").values.map(_.names)
        require(names.nonEmpty && names.forall(_.nonEmpty) && names.flatten.sorted==data.variables.sorted,"variable_groups must partition all variables")
        names.map(_.map(data.variables.indexOf))
    normalized=data.values.map(row=>row.indices.map(j=>(row(j)-offset(j%data.p))/scale(j%data.p)).toArray)
    val contexts=if groupByContext then data.groups.distinct.sorted else Vector("all")
    blocks=contexts.flatMap { context=>columns.flatMap { cols=>
      val ids=data.values.indices.filter(i=>(!groupByContext || data.groups(i)==context) && slice(normalized(i),cols).exists(_.isFinite)).toVector
      if ids.isEmpty then None else
        val values=ids.map(i=>slice(normalized(i),cols)).toArray
        val (assignment,representatives,distances)=medoids(values,math.min(clusters,ids.size),cols)
        val fitted=representatives.indices.map { c=>
          val members=ids.indices.filter(assignment(_)==c).toVector
          val rows=members.map(values).toArray
          val matrix=if layout=="series" then rows else rows.flatMap(_.grouped(cols.size))
          PatternCluster(members.map(ids),ids(representatives(c)),MaskedPCA.mean(rows),MaskedPCA.fit(matrix,components,ridge))
        }.toVector
        Some(PatternBlock(context,cols,ids,assignment,fitted,distances))
    } }
    this
  private def warped(center: Array[Double],partial: Array[Double],cols: Int,weights: Array[Double]): Array[Double] =
    if !partial.exists(_.isFinite) || warping=="euclidean" then center.clone()
    else
      val guide=partial.indices.map(i=>if partial(i).isFinite then partial(i) else center(i)).toArray
      val path=Distances.dtw(center,guide,cols,window,weights)._2
      val out=center.clone()
      path.groupMap(_._2)(_._1).foreach { (j,aligned)=>
        val mean=MaskedPCA.mean(aligned.map(i=>center.slice(i*cols,(i+1)*cols)).toArray)
        Array.copy(mean,0,out,j*cols,cols)
      };out
  def predict(data: SeriesDataset,allowTrainingRuns: Boolean=false,allowReducedProxy: Boolean=false): Array[Array[Double]] =
    require(training!=null,"Fit the model first")
    require(data.variables==training.variables && data.times.toVector==training.times.toVector,"Prediction variable/time axes differ from training")
    require(allowTrainingRuns || !data.runs.exists(training.runs.toSet),"Training and evaluation runs must be disjoint")
    require(allowReducedProxy || data.metadata.field("observation_semantics").text()!="reduced_proxy","Reduced proxy values require calibration or an explicit ablation")
    val result=Array.fill(data.n,data.t*data.p)(Double.NaN);val diag=Vector.newBuilder[JsValue]
    data.values.indices.foreach { i=>
      val query=data.values(i).indices.map(j=>(data.values(i)(j)-offset(j%data.p))/scale(j%data.p)).toArray
      val applicable=blocks.filter(b=> !groupByContext || b.context==data.groups(i))
      if applicable.isEmpty then diag+=obj("series"->JsNumber(i),"reason"->JsString("unseen_context"))
      applicable.foreach { block=>
        val part=slice(query,block.columns);val distances=block.clusters.map(c=>distance(part,c.center,block.columns))
        val assigned=if distances.exists(_.isFinite) then distances.indices.minBy(distances) else block.clusters.indices.maxBy(c=>block.clusters(c).members.size)
        val cluster=block.clusters(assigned);var estimate=cluster.center.clone()
        var info=obj("series"->JsNumber(i),"variables"->strings(block.columns.map(training.variables)),"cluster"->JsNumber(assigned),"observations"->JsNumber(part.count(_.isFinite)))
        method match
          case "baseline"=>
            val matching=block.ids.filter(j=>training.labels(j)==data.labels(i))
            if matching.nonEmpty then estimate=MaskedPCA.mean(matching.map(j=>slice(normalized(j),block.columns)).toArray)
          case "pca"=>
            if layout=="series" then
              val (row,details)=cluster.basis.reconstruct(part);estimate=row;info=JsObject(info.fields++details.fields)
            else
              val reconstructed=part.grouped(block.columns.size).map(cluster.basis.reconstruct).toVector
              estimate=reconstructed.flatMap(_._1).toArray;info=JsObject(info.fields+("per_time"->JsArray(reconstructed.map(_._2))))
          case "dtw"=>
            if warping=="independent" then
              for p<-block.columns.indices do
                val values=warped(estimate.indices.by(block.columns.size).map(j=>estimate(j+p)).toArray,part.indices.by(block.columns.size).map(j=>part(j+p)).toArray,1,Array(1.0))
                values.indices.foreach(j=>estimate(j*block.columns.size+p)=values(j))
            else estimate=warped(estimate,part,block.columns.size,block.columns.map(weights).toArray)
          case _=>()
        part.indices.filter(part(_).isFinite).foreach(j=>estimate(j)=part(j))
        for j<-estimate.indices do result(i)((j/block.columns.size)*data.p+block.columns(j%block.columns.size))=estimate(j)
        diag+=info
      }
      for j<-result(i).indices do result(i)(j)=result(i)(j)*scale(j%data.p)+offset(j%data.p)
      if fallback=="training" then
        val (expected,context)=prior.template(data.labels(i),data.groups(i));var labels=0;var contexts=0
        result(i).indices.foreach { j=>
          if !result(i)(j).isFinite && expected(j).isFinite then {result(i)(j)=expected(j);if context(j) then contexts+=1 else labels+=1}
        }
        diag+=obj("series"->JsNumber(i),"coverage_fallback"->JsString("training"),"label_values"->JsNumber(labels),"context_values"->JsNumber(contexts),"unsupported"->JsNumber(result(i).count(v=> !v.isFinite)))
    }
    diagnostics=diag.result();result
  def select(budget: Int,strategy: String="medoid",selectionSeed: Long=seed): Vector[Int] =
    require(training!=null && budget>=1 && budget<=training.n,"Representative budget exceeds training series")
    require(Set("random","medoid","centroid_nearest","pca","coverage")(strategy),"Unknown representative strategy")
    if strategy=="random" then new NumpyGenerator(selectionSeed).sample(training.n,budget).toVector.sorted
    else if strategy=="coverage" then
      val remaining=mutable.Set.from(training.values.indices);val covered=mutable.Set.empty[String];val selected=Vector.newBuilder[Int]
      for _<-0 until budget do
        val next=remaining.toVector.minBy(i=>(-(training.units(i).toSet--covered).size,i))
        selected+=next;covered++=training.units(next);remaining-=next
      selected.result()
    else if strategy=="pca" then
      val mu=MaskedPCA.mean(normalized)
      val filled=normalized.map(row=>row.indices.map(j=>if row(j).isFinite then row(j) else if mu(j).isFinite then mu(j) else 0.0).toArray)
      val mean=MaskedPCA.mean(filled);val centered=filled.map(row=>row.indices.map(j=>row(j)-mean(j)).toArray)
      val count=math.min(components,math.min(training.n,mean.length))
      val eig=org.tslearn.utils.NativeLinearAlgebra.rightSingularSystem(centered.flatten,training.n,mean.length,count)
      val rank=eig.values().count(_>1e-20)
      if rank==0 then (0 until budget).toVector
      else
        val u=Array.tabulate(rank)(k=>Array.tabulate(training.n)(i=>centered(i).indices.map(j=>centered(i)(j)*eig.vectors()(k)(j)).sum/math.sqrt(eig.values()(k))))
        org.tslearn.utils.NativeLinearAlgebra.columnPivotOrder(u).take(budget).toVector
    else
      val ranked=blocks.flatMap { block=>block.clusters.map { cluster=>
        val local=block.ids.zipWithIndex.toMap
        val ordered=cluster.members.sortBy { i=>
          val d=if strategy=="medoid" then cluster.members.map(j=>block.distances(local(i))(local(j))).sum else distance(slice(normalized(i),block.columns),cluster.center,block.columns)
          (d,i)
        };mutable.ArrayBuffer.from(ordered)
      } }
      val chosen=mutable.ArrayBuffer.empty[Int];val used=mutable.Set.empty[Int]
      while chosen.size<budget do
        val before=chosen.size
        ranked.foreach { candidates=>
          while candidates.nonEmpty && used(candidates.head) do candidates.remove(0)
          if candidates.nonEmpty && chosen.size<budget then {val next=candidates.remove(0);chosen+=next;used+=next}
        }
        if before==chosen.size then training.values.indices.filterNot(used).take(budget-chosen.size).foreach(i=>{chosen+=i;used+=i})
      chosen.toVector
  def selectDistinctLabels(budget: Int,strategy: String="medoid",selectionSeed: Long=seed): Vector[Int] =
    require(budget>=1 && budget<=training.labels.distinct.size,"Budget exceeds distinct labels")
    val ranked=if strategy=="random" then new NumpyGenerator(selectionSeed).permutation(training.n).toVector else select(training.n,strategy,selectionSeed)
    val seen=mutable.Set.empty[String]
    ranked.filter { i=>if seen(training.labels(i)) then false else {seen+=training.labels(i);true} }.take(budget)

  def summary: JsObject = obj("method"->JsString(method),"clustering"->JsString(clustering),"warping"->JsString(warping),"coverage_fallback"->JsString(fallback),"window"->window.map(JsNumber(_)).getOrElse(JsNull),
    "scaling"->JsString(scaling),"pca_layout"->JsString(layout),"training_runs"->strings(training.runs.distinct.sorted),
    "blocks"->JsArray(blocks.map(b=>obj("context"->JsString(b.context),"variables"->strings(b.columns.map(training.variables)),"cluster_sizes"->ints(b.clusters.map(_.members.size)),
      "centroids"->JsArray(b.clusters.map(c=>JsArray(c.center.grouped(b.columns.size).map(a=>arr(a.toVector)).toVector))),"medoid_labels"->strings(b.clusters.map(c=>training.labels(c.medoid)))))))
