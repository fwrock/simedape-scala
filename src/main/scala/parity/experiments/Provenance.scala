package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*

object Provenance:
  val physical=Vector("representation","alignment","bin_seconds","start_seconds","end_seconds","complete_snapshots","complete_events","count_source","sampling","variable_units")
  def compatible(a: SeriesDataset,b: SeriesDataset): Unit =
    require(a.variables==b.variables && a.times.toVector==b.times.toVector,"Observation variable/time axes differ")
    physical.foreach(k=>require(a.metadata.field(k)==b.metadata.field(k),s"Observation semantics differ: $k"))
  private def pythonFloat(value: Double): String =
    require(value.isFinite,"Nonfinite value in provenance")
    val raw=java.lang.Double.toString(value).toLowerCase
    val absolute=math.abs(value)
    if absolute!=0 && (absolute>=1e16 || absolute<1e-4) then
      val parts=(if raw.contains('e') then raw else java.math.BigDecimal.valueOf(value).stripTrailingZeros().toString.toLowerCase).split('e')
      val exponent=parts(1).toInt;parts(0).stripSuffix(".0")+"e"+(if exponent<0 then "-" else "+")+f"${math.abs(exponent)}%02d"
    else
      val decimal=java.math.BigDecimal.valueOf(value).toPlainString
      if decimal.contains('.') then decimal else decimal+".0"
  /** Python json.dumps(sort_keys=True,separators=(',',':'),ensure_ascii=True) for provenance. */
  def canonical(value: JsValue): String = value match
    case o: JsObject=>o.fields.toVector.sortBy(_._1).map((k,v)=>canonical(JsString(k))+":"+canonical(v)).mkString("{",",","}")
    case JsArray(values)=>values.map(canonical).mkString("[",",","]")
    case s: JsString=>s.compactPrint.flatMap(c=>if c>=128 then f"\\u${c.toInt}%04x" else c.toString)
    case JsNumber(n)=>if n.scale==0 then n.toBigInt.toString else pythonFloat(n.toDouble)
    case other=>other.compactPrint
  def digest(value: JsValue): String = java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(canonical(value).getBytes(java.nio.charset.StandardCharsets.UTF_8)))
  def validateRunInputs(value: JsValue): JsObject =
    val record=value.asJsObject
    require(record.field("schema").text()=="simedape.run_inputs.v1" && record.field("sha256").text()==digest(JsObject(record.fields-"sha256")),"Missing or invalid run input fingerprint")
    require(record.field("clock").isInstanceOf[JsObject] && record.field("engine_jar_sha256").text().nonEmpty && record.field("source_inputs_sha256").text().nonEmpty,"Missing run input provenance")
    val departures=record.field("departure_seconds").asJsObject.fields
    require(departures.nonEmpty && departures.values.forall(v=>v.isInstanceOf[JsNumber] && v.number().isFinite),"Invalid departure inputs")
    record

final case class PairedMapping(template: Array[Double],gain: Array[Double],values: Array[Int],runs: Array[Int])
final class PairedResidualEstimator(val ridge: Double=1.0,val nonnegative: Boolean=true,val departureTolerance: Double=1.0):
  require(ridge>0 && ridge.isFinite && departureTolerance>=0 && departureTolerance.isFinite,"Invalid paired calibration settings")
  var full: SeriesDataset=null
  var prior: TrainingPrior=null
  var context: JsObject=Json.empty
  var engineHash=""
  var clock: JsValue=JsNull
  var envelope=Map.empty[String,(Double,Double)]
  var mappings=Map.empty[(String,String),PairedMapping]
  var sourceHash: Option[String]=None
  var diagnostics=Vector.empty[JsValue]
  def fit(training: SeriesDataset,proxy: SeriesDataset): PairedResidualEstimator =
    Provenance.compatible(training,proxy)
    val ctx=proxy.metadata.field("reduction_context",Json.empty).asJsObject
    require(proxy.metadata.field("observation_semantics").text()=="reduced_proxy" && ctx.field("schema").text()=="simedape.reduction_context.v1" && ctx.field("observation_semantics").text()=="reduced_proxy" && ctx.field("sha256").text()==Provenance.digest(JsObject(ctx.fields-"sha256")),"Paired fitting requires verified reduced-proxy context")
    require(proxy.runs.distinct.size>=2,"Two distinct paired calibration runs are required")
    require(Set("vehicle_sample","meso_background")(ctx.field("policy").text()),"Unsupported paired reduction policy")
    require(training.metadata.field("observation_semantics").text()!="reduced_proxy","Full training data cannot be reduced proxies")
    val lookup=training.runs.zip(training.labels).zipWithIndex.toMap
    require(proxy.runs.zip(proxy.labels).forall(lookup.contains),"Proxy calibration has no matching full training identity")
    val records=training.runs.distinct.sorted.map { run=>
      val record=Provenance.validateRunInputs(training.metadata.field("run_provenance").field(run));require(record.field("run_id").text()==run,"Incorrect training run identity");run->record
    }.toMap
    val first=records(training.runs.distinct.sorted.head);engineHash=first.field("engine_jar_sha256").text();clock=first.field("clock")
    require(records.values.forall(r=>r.field("engine_jar_sha256").text()==engineHash && r.field("clock")==clock),"Training engines or clocks differ")
    val inputs=proxy.runs.distinct.sorted.map { run=>
      val record=Provenance.validateRunInputs(proxy.metadata.field("run_provenance").field(run))
      require(record.field("run_id").text()==run && Vector("engine_jar_sha256","clock","departure_seconds","source_inputs_sha256").forall(k=>record.field(k)==records(run).field(k)),"Full/proxy calibration provenance disagrees")
      record.field("departure_seconds").asJsObject.fields
    }
    val population=inputs.head.keySet;require(inputs.forall(_.keySet==population),"Calibration departure populations differ")
    envelope=population.map(id=>id->(inputs.map(_(id).number()).min,inputs.map(_(id).number()).max)).toMap
    full=training;prior=new TrainingPrior(training);context=ctx
    val variance=Array.tabulate(full.p) { p=>
      val values=full.values.flatMap(row=>row.indices.by(full.p).map(j=>row(j+p))).filter(_.isFinite)
      if values.isEmpty then 1.0 else {val mean=values.sum/values.length;math.max(1e-12,values.map(v=>math.pow(v-mean,2)).sum/values.length)}
    }
    mappings=proxy.groups.zip(proxy.labels).distinct.sorted.map { key=>
      val ids=proxy.labels.indices.filter(i=>(proxy.groups(i),proxy.labels(i))==key).toVector
      val targets=ids.map(i=>lookup((proxy.runs(i),proxy.labels(i))))
      require(targets.forall(i=>full.groups(i)==key._1),"Paired context groups disagree")
      val rows=ids.map(proxy.values).toArray;val template=MaskedPCA.mean(rows);val expected=prior.template(key._2,key._1)._1
      val numerator=Array.fill(full.p)(0.0);val denominator=variance.map(_*ridge);val supported=Array.fill(full.p)(0);val runs=Array.fill(full.p)(0)
      ids.indices.foreach { local=>
        val perRun=Array.fill(full.p)(false)
        for j<-template.indices do
          val x=rows(local)(j)-template(j);val y=full.values(targets(local))(j)-expected(j);val p=j%full.p
          if x.isFinite && y.isFinite then {numerator(p)+=x*y;denominator(p)+=x*x;supported(p)+=1;perRun(p)=true}
        perRun.indices.filter(perRun).foreach(p=>runs(p)+=1)
      }
      val gain=Array.tabulate(full.p)(p=>if runs(p)<2 then 0.0 else numerator(p)/denominator(p))
      key->PairedMapping(template,gain,supported,runs)
    }.toMap
    this
  def assessInputs(ctx: JsValue,inputs: JsValue): JsObject =
    val reasons=scala.collection.mutable.ArrayBuffer.empty[String]
    if ctx!=context then reasons+="reduction_context_changed"
    val record=try Provenance.validateRunInputs(inputs) catch case scala.util.control.NonFatal(_)=>
      return obj("decision"->JsString("requires_full_run"),"reasons"->strings(reasons.toVector:+"invalid_run_inputs"))
    if record.field("engine_jar_sha256").text()!=engineHash then reasons+="engine_changed"
    if record.field("clock")!=clock then reasons+="clock_changed"
    val departures=record.field("departure_seconds").asJsObject.fields
    if departures.keySet!=envelope.keySet then reasons+="departure_population_changed"
    val excess=(departures.keySet intersect envelope.keySet).toVector.map { id=>
      val (low,high)=envelope(id);id->math.max(0,math.max(low-departureTolerance-departures(id).number(),departures(id).number()-high-departureTolerance))
    }
    val outside=excess.filter(_._2>1e-9).map(_._1).sorted
    if outside.nonEmpty then reasons+="departure_outside_envelope"
    obj("decision"->JsString(if reasons.nonEmpty then "requires_full_run" else "within_calibration_envelope"),"reasons"->strings(reasons.toVector),"outside_vehicle_count"->JsNumber(outside.size),"outside_vehicle_examples"->strings(outside.take(10)),
      "max_departure_excess_seconds"->num(excess.map(_._2).maxOption.getOrElse(0.0)),"scope"->JsString("Input compatibility only; empirical accuracy and joint departure distributions need validation"))
  def predict(proxy: SeriesDataset,allowDepartureExtrapolation: Boolean=false): Array[Array[Double]] =
    require(full!=null,"Fit paired model first");Provenance.compatible(full,proxy)
    require(!proxy.runs.exists(full.runs.toSet),"Paired training and evaluation runs must be disjoint")
    require(proxy.metadata.field("observation_semantics").text()=="reduced_proxy" && proxy.metadata.field("reduction_context")==context,"Prediction reduction context differs from calibration")
    val assessments=proxy.runs.distinct.sorted.map { run=>
      val inputs=proxy.metadata.field("run_provenance").field(run)
      require(inputs.field("run_id",JsString(run)).text()==run,"Incorrect prediction run identity")
      val assessment=assessInputs(context,inputs)
      val denied=assessment.field("reasons").names.filterNot(r=>allowDepartureExtrapolation && r=="departure_outside_envelope")
      require(denied.isEmpty,s"Prediction requires full run: ${denied.mkString(", ")}");run->assessment
    }.toMap
    val diag=Vector.newBuilder[JsValue]
    val result=proxy.values.indices.map { i=>
      val key=(proxy.groups(i),proxy.labels(i));val mapping=mappings.getOrElse(key,throw IllegalArgumentException("Prediction label/context has no paired calibration"))
      val (expected,fallback)=prior.template(key._2,key._1);var updates=0;var clipped=0
      expected.indices.foreach { j=>
        val value=proxy.values(i)(j);val p=j%proxy.p
        if value.isFinite && mapping.template(j).isFinite && expected(j).isFinite then {expected(j)+=(value-mapping.template(j))*mapping.gain(p);updates+=1}
        if nonnegative && Set("speed","speed_std","count","queue","flow","gap","gap_p10","acceleration_positive","deceleration","distance","position","lane_changes")(proxy.variables(p)) && expected(j)<0 then {expected(j)=0;clipped+=1}
      }
      val assessment=assessments(proxy.runs(i))
      diag+=obj("label"->JsString(key._2),"proxy_updates"->JsNumber(updates),"context_fallback"->JsNumber(fallback.count(identity)),"unsupported"->JsNumber(expected.count(v=> !v.isFinite)),"clipped"->JsNumber(clipped),
        "gain"->arr(mapping.gain.toVector),"input_semantics"->JsString("reduced_proxy"),"input_assessment"->assessment,"departure_extrapolation_ablation"->JsBoolean(allowDepartureExtrapolation && assessment.field("decision").text()=="requires_full_run"))
      expected
    }.toArray
    diagnostics=diag.result();result
  def selectDistinctLabels(budget: Int): Vector[Int] =
    val selected=context.field("selected_series").names;require(selected.nonEmpty && selected.size==budget,"Paired calibration requires its exact selected-series budget")
    selected.map(l=>{val i=full.labels.indexOf(l);require(i>=0,"Calibration series absent from training");i})
  def summary: JsObject = obj("method"->JsString("paired_residual"),"ridge"->num(ridge),"nonnegative"->JsBoolean(nonnegative),"engine_jar_sha256"->JsString(engineHash),"input_clock"->clock,
    "departure_tolerance_seconds"->num(departureTolerance),"departure_envelope"->JsObject(envelope.map((id,v)=>id->arr(Vector(v._1,v._2)))),"calibration_source_sha256"->sourceHash.map(JsString(_)).getOrElse(JsNull),
    "training_runs"->strings(full.runs.distinct.sorted),"reduction_context"->context,"mappings"->JsArray(mappings.toVector.sortBy(_._1).map((key,m)=>obj("group"->JsString(key._1),"label"->JsString(key._2),"gain"->arr(m.gain.toVector),"paired_values"->ints(m.values.toVector),"paired_runs"->ints(m.runs.toVector)))))
