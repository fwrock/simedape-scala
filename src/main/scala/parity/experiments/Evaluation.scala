package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import org.tslearn.utils.NumpyGenerator

object Evaluation:
  def mask(data: SeriesDataset,fraction: Double,seed: Long=0,mode: String="random",selectedLabels: Option[Vector[String]]=None): Array[Array[Boolean]] =
    require(fraction.isFinite && fraction>=0 && fraction<=1,"Observation fraction must be in [0,1]")
    require(Set("random","prefix","block","representatives")(mode),"Unknown observation mask")
    if mode=="representatives" then require(selectedLabels.nonEmpty,"Representative masks need training-selected labels")
    val rng=new NumpyGenerator(seed)
    val missing=math.rint(data.t*(1-fraction)).toInt
    Array.tabulate(data.n) { i=>
      val start=if mode=="block" then rng.integer(data.t-missing+1) else 0
      Array.tabulate(data.t*data.p) { j=>
        val keep=mode match
          case "random"=>rng.random()<fraction
          case "prefix"=>j/data.p<(data.t*fraction).toInt
          case "block"=>j/data.p<start || j/data.p>=start+missing
          case _=>selectedLabels.get.contains(data.labels(i))
        keep && data.values(i)(j).isFinite
      }
    }
  def evaluate(truth: SeriesDataset,prediction: Array[Array[Double]],score: Array[Array[Boolean]]): JsObject =
    require(prediction.length==truth.n && score.length==truth.n && prediction.forall(_.length==truth.t*truth.p) && score.forall(_.length==truth.t*truth.p),"Prediction/score axes do not match truth")
    JsObject(truth.variables.indices.map { p=>
      var eligible=0
      val pairs=Vector.newBuilder[(Double,Double)]
      for i<-truth.values.indices;j<-p until truth.t*truth.p by truth.p do
        if score(i)(j) && truth.values(i)(j).isFinite then
          eligible+=1
          if prediction(i)(j).isFinite then pairs+=((truth.values(i)(j),prediction(i)(j)))
      val rows=pairs.result();val count=rows.size
      val base=obj("eligible"->JsNumber(eligible),"estimated"->JsNumber(count),"coverage"->(if eligible==0 then JsNull else num(count.toDouble/eligible)),
        "mae"->JsNull,"rmse"->JsNull,"bias"->JsNull,"quantile_errors"->Json.empty)
      val result=if count==0 then base else
        val (real,estimated)=rows.unzip;val errors=rows.map((r,e)=>e-r)
        JsObject(base.fields++Map("mae"->num(errors.map(math.abs).sum/count),"rmse"->num(math.sqrt(errors.map(v=>v*v).sum/count)),"bias"->num(errors.sum/count),
          "quantile_errors"->JsObject(Vector(.1,.5,.9,.99).map(q=>q.toString->num(math.abs(Metrics.quantile(estimated,q)-Metrics.quantile(real,q)))).toMap))++
          (if truth.variables(p)=="acceleration" then Map.empty else Map("negative_predictions"->JsNumber(estimated.count(_<0)))))
      truth.variables(p)->result
    }.toMap)
  def commonSupport(truth: SeriesDataset,predictions: Map[String,Array[Array[Double]]]): (JsObject,JsObject) =
    require(predictions.nonEmpty,"Common-support evaluation needs predictions")
    predictions.values.foreach(v=>require(v.length==truth.n && v.forall(_.length==truth.t*truth.p),"Prediction axes differ from truth"))
    val mask=Array.tabulate(truth.n)(i=>Array.tabulate(truth.t*truth.p)(j=>truth.values(i)(j).isFinite && predictions.values.forall(_(i)(j).isFinite)))
    val coverage=JsObject(truth.variables.indices.map { p=>
      val eligible=truth.values.indices.map(i=>(p until truth.t*truth.p by truth.p).count(j=>truth.values(i)(j).isFinite)).sum
      val shared=truth.values.indices.map(i=>(p until truth.t*truth.p by truth.p).count(j=>mask(i)(j))).sum
      truth.variables(p)->obj("eligible"->JsNumber(eligible),"shared"->JsNumber(shared),"fraction"->(if eligible==0 then JsNull else num(shared.toDouble/eligible)))
    }.toMap)
    (JsObject(predictions.map((name,values)=>name->evaluate(truth,values,mask))),coverage)
  def tripOutcomes(rows: Vector[JsValue]): JsObject =
    val required=Set("run_id","trip_id","departure_seconds","arrival_seconds","distance_m","censored")
    rows.foreach(row=>require(required.subsetOf(row.asJsObject.fields.keySet),"Missing trip columns"))
    require(rows.map(r=>(r.field("run_id").text(),r.field("trip_id").text())).distinct.size==rows.size,"Duplicate trip outcome")
    var censored=0;var distance=0.0;val travel=Vector.newBuilder[Double]
    rows.foreach { row=>
      val departure=row.field("departure_seconds").number(Double.NaN);val meters=row.field("distance_m").number(Double.NaN)
      require(departure.isFinite && meters.isFinite && meters>=0,"Trips need finite departures and nonnegative distance")
      distance+=meters
      if row.field("censored").bool() then {require(row.field("arrival_seconds")==JsNull,"Censored trips cannot carry invented arrivals");censored+=1}
      else
        val duration=row.field("arrival_seconds").number(Double.NaN)-departure
        require(duration.isFinite && duration>=0,"Completed trips require valid arrival times");travel+=duration
    }
    val values=travel.result()
    obj("trips"->JsNumber(rows.size),"completed"->JsNumber(values.size),"censored"->JsNumber(censored),"completion_rate"->(if rows.isEmpty then JsNull else num(values.size.toDouble/rows.size)),"distance_km"->num(distance/1000),
      "travel_time_quantiles_seconds"->(if values.isEmpty then Json.empty else JsObject(Vector(.1,.5,.9,.99).map(q=>q.toString->num(Metrics.quantile(values,q))).toMap)))
