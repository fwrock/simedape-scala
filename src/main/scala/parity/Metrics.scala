package org.interscity.htc.parity

import spray.json.*
import Json.*

object Metrics:
  val keys=Vector("average_speed","percentage_of_track","average_vehicles","km_traveled","density_veh_km","speed_index","delay_index","peak_occupancy","congested_time_pct")
  def mean(values: Iterable[Double]): Double = if values.isEmpty then Double.NaN else values.sum/values.size
  def geometric(values: Iterable[Double],positiveOnly: Boolean=false): Double =
    val xs=if positiveOnly then values.filter(_>0).toVector else values.toVector
    if xs.isEmpty || xs.exists(x=>x.isNaN || x<0) then Double.NaN
    else if xs.contains(0.0) then 0.0
    else math.exp(xs.iterator.map(math.log).sum/xs.size)
  def quantile(values: Seq[Double],q: Double): Double =
    if values.isEmpty then Double.NaN
    else
      val sorted=values.sorted;val pos=q*(sorted.length-1);val left=pos.toInt
      sorted(left)+(sorted(math.min(left+1,sorted.length-1))-sorted(left))*(pos-left)
  def calculate(values: Array[Double],road: Road): Map[String,Double] =
    require(values.nonEmpty,"Cannot calculate metrics for an empty series")
    val capacity=road.length/7.5
    val speeds=values.map(n=>if n>=capacity then 1.0 else road.freeSpeed*(1-n/capacity))
    val occupancy=values.map(_/capacity*100)
    val indices=speeds.map(v=>math.max(.02,if road.freeSpeed==0 then 1.0 else v/road.freeSpeed))
    val km=(road.length+values.sliding(2).filter(p=>p.length==2 && p(1)>p(0)).map(p=>(p(1)-p(0))*road.length).sum)/1000
    Map("average_speed"->geometric(speeds.toVector),"percentage_of_track"->mean(occupancy.toVector),
      "average_vehicles"->mean(values.toVector),"km_traveled"->km,"density_veh_km"->(mean(values.toVector)/(road.length/1000)),
      "speed_index"->mean(indices.map(math.min(_,1.0)).toVector),"delay_index"->(mean(indices.map(1.0/_).toVector)-1),
      "peak_occupancy"->occupancy.max,"congested_time_pct"->(100.0*occupancy.count(_>=80)/occupancy.length))
  def result(rows: Vector[(String,Array[Double])],roads: Map[String,Road],missingSP: Int=0,missingSeries: Int=0): JsObject =
    val calculated=rows.map { (label,values) =>
      val link=label.replaceFirst("-[0-9]+-[0-9]+$", "")
      calculate(values,roads.getOrElse(link,throw IllegalArgumentException(s"No road metadata for $link")))
    }
    JsObject(keys.map(k=>k->arr(calculated.map(_(k)))).toMap++Map("labels"->strings(rows.map(_._1)),"missing_simulation_points"->JsNumber(missingSP),"missing_time_series"->JsNumber(missingSeries)))
  def distribution(values: Vector[Double]): JsObject =
    val average=mean(values)
    val base=obj("min"->num(values.minOption.getOrElse(Double.NaN)),"max"->num(values.maxOption.getOrElse(Double.NaN)),
      "mean"->num(average),"median"->num(quantile(values,.5)),"std"->num(math.sqrt(mean(values.map(v=>math.pow(v-average,2))))),"mape"->num(mean(values.map(math.abs))))
    if values.isEmpty then base else JsObject(base.fields++Map("p25"->num(quantile(values,.25)),"p75"->num(quantile(values,.75))))
  private def correlation(a: Vector[Double],b: Vector[Double]): Double =
    val ma=mean(a);val mb=mean(b)
    val numerator=a.zip(b).map((x,y)=>(x-ma)*(y-mb)).sum
    val denominator=math.sqrt(a.map(x=>math.pow(x-ma,2)).sum*b.map(y=>math.pow(y-mb,2)).sum)
    if denominator==0 then Double.NaN else numerator/denominator
  private def ordinal(values: Vector[Double]): Vector[Double] =
    val result=new Array[Double](values.size)
    values.indices.sortBy(i=>(values(i),i)).zipWithIndex.foreach((i,rank)=>result(i)=rank.toDouble)
    result.toVector
  def robust(real: Vector[Double],estimated: Vector[Double]): JsObject =
    val pairs=real.zip(estimated).filter((r,e)=>r.isFinite && e.isFinite)
    if pairs.isEmpty then JsObject(Vector("n","wape","smape","median_ape","bias_pct","rmse","mae","pearson","spearman").map(_->JsNull).toMap)
    else
      val (r,e)=pairs.unzip;val diffs=pairs.map((r,e)=>e-r);val denominator=r.map(math.abs).sum
      obj("n"->JsNumber(r.size),"wape"->num(if denominator>0 then 100*diffs.map(math.abs).sum/denominator else Double.NaN),
        "smape"->num(100*mean(pairs.map((r,e)=>if math.abs(r)+math.abs(e)==0 then 0.0 else 2*math.abs(e-r)/(math.abs(r)+math.abs(e))))),
        "median_ape"->num(quantile(pairs.filter(_._1>0).map((r,e)=>100*math.abs(e-r)/r),.5)),
        "bias_pct"->num(if r.sum!=0 then 100*diffs.sum/r.sum else Double.NaN),"rmse"->num(math.sqrt(mean(diffs.map(x=>x*x)))),
        "mae"->num(mean(diffs.map(math.abs))),"pearson"->num(if r.size>=3 then correlation(r,e) else Double.NaN),
        "spearman"->num(if r.size>=3 && r.distinct.size>1 && e.distinct.size>1 then correlation(ordinal(r),ordinal(e)) else Double.NaN))

  def compare(real: JsValue,estimated: JsValue,label: String,context: JsValue=empty): JsObject =
    val active=keys.filter(k=>real.field(k).isInstanceOf[JsArray] && estimated.field(k).isInstanceOf[JsArray])
    val rl=real.field("labels").names;val el=estimated.field("labels").names
    val byLabel=active.forall(k=>rl.nonEmpty && el.nonEmpty && rl.size==real.field(k).values.size && el.size==estimated.field(k).values.size)
    val pairs=active.map { key =>
      val rv=real.field(key).values.map(_.number(Double.NaN));val ev=estimated.field(key).values.map(_.number(Double.NaN))
      val rows=if byLabel then
        val index=rl.zipWithIndex.toMap
        el.zip(ev).flatMap((l,e)=>index.get(l).map(i=>(rv(i),e)))
      else rv.zip(ev)
      key->rows
    }.toMap
    val realAgg=active.map(k=>k->num(geometric((if byLabel then pairs(k).map(_._1) else real.field(k).values.map(_.number(Double.NaN))),positiveOnly=true))).toMap
    val estAgg=active.map(k=>k->num(geometric((if byLabel then pairs(k).map(_._2) else estimated.field(k).values.map(_.number(Double.NaN))),positiveOnly=true))).toMap
    val errors=active.map(k=>k->num(if realAgg(k)==JsNull || realAgg(k).number()==0 then Double.NaN else 100*(estAgg(k).number(Double.NaN)-realAgg(k).number())/realAgg(k).number())).toMap
    val distributions=active.map(k=>k->distribution(pairs(k).filter(_._1>0).map((r,e)=>100*(e-r)/r))).toMap
    val robustResults=active.map(k=>k->robust(pairs(k).map(_._1),pairs(k).map(_._2))).toMap
    val nReal=real.field("average_speed").values.size;val nEst=estimated.field("average_speed").values.size
    val base=obj("label"->JsString(label),"n_real"->JsNumber(nReal),"n_estimated"->JsNumber(nEst),
      "coverage_pct"->num(if nReal==0 then 0 else math.rint(10000.0*nEst/nReal)/100),"pairing"->JsString(if byLabel then "label" else "position"),
      "n_paired"->JsNumber(pairs.getOrElse("average_speed",Vector.empty).size),
      "aggregate"->obj("real"->JsObject(realAgg),"estimated"->JsObject(estAgg),"error_pct"->JsObject(errors)),
      "per_series_error"->JsObject(distributions),"robust"->JsObject(robustResults))
    if !byLabel then base
    else
      val labels=el.filter(rl.toSet)
      val worst=active.map { k =>
        val rows=labels.zip(pairs(k)).filter(_._2._1!=0).map((l,p)=>(100*math.abs(p._2-p._1)/math.abs(p._1),l,p._1,p._2)).sortBy(-_._1).take(15)
        k->JsArray(rows.map((error,l,r,e)=>obj("label"->JsString(l),"real"->num(r),"estimated"->num(e),"error_pct"->num(error))))
      }.toMap
      val scatter=active.map(k=>k->JsArray(pairs(k).zipWithIndex.filter((_,i)=>i%math.max(1,pairs(k).size/400)==0).map((p,_)=>arr(Seq(p._1,p._2))))).toMap
      val breakdowns=active.map(k=>k->breakdown(labels,pairs(k),context)).toMap
      JsObject(base.fields+ ("specific"->obj("worst"->JsObject(worst),"scatter"->JsObject(scatter),"breakdowns"->JsObject(breakdowns))))

  private def breakdown(labels: Vector[String],pairs: Vector[(Double,Double)],context: JsValue): JsObject =
    if context==empty then empty
    else
      val rows=labels.zip(pairs);val cluster=context.field("cluster_of",empty);val lengths=context.field("length_of",empty);val cuts=context.field("length_cuts").values.map(_.number())
      def lengthKey(label: String): Option[String] =
        val link=label.replaceFirst("-[0-9]+-[0-9]+$", "")
        val value=lengths.field(link)
        def fmt(d: Double): String = BigDecimal(d).bigDecimal.stripTrailingZeros.toPlainString
        if value==JsNull || cuts.isEmpty then None
        else
          val index=cuts.count(value.number()>_)
          val low=if index==0 then "0" else fmt(cuts(index-1))
          Some(if index<cuts.size then s"$low–${fmt(cuts(index))} m" else s">${fmt(cuts.last)} m")
      val categories=Map[String,((String,(Double,Double)))=>Option[String]](
        "by_cluster"->(row=>Option(cluster.field(row._1)).filter(_!=JsNull).map(_.text())),
        "by_interval"->(row=>Some(row._1.split("-").drop(1).mkString("-")).filter(_.nonEmpty)),
        "by_length"->(row=>lengthKey(row._1)))
      JsObject(categories.flatMap { (name,key) =>
        val groups=rows.flatMap(row=>key(row).map(_->row._2)).groupMap(_._1)(_._2)
        if groups.isEmpty then None else Some(name->JsObject(groups.map { (group,values) =>
          val stats=robust(values.map(_._1),values.map(_._2));group->JsObject(stats.fields.view.filterKeys(Set("n","wape","bias_pct","median_ape")).toMap)
        }))
      })
