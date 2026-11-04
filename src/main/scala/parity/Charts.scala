package org.interscity.htc.parity
import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.io.FileNotFoundException

object Charts:
  final case class Spec(id: String,title: String,kind: String,source: String,params: Vector[String]=Vector.empty)
  val specs=Vector(
    Spec("error_aggregate","Erro percentual agregado (gmean)","bar","compare"),Spec("mape_by_method","MAPE por método","bar","compare"),Spec("mape_radar","MAPE por métrica","radar","compare"),
    Spec("coverage","Cobertura: séries estimadas vs. reais","bar","compare"),Spec("error_distribution","Distribuição de erros por série","boxplot","compare",Vector("metric")),
    Spec("real_vs_estimated","Real vs. estimado por série","scatter","estimation",Vector("method","metric","limit")),Spec("sweep_progression","Progressão do MAPE por cenário parcial","line","sweep",Vector("metric","methods")),
    Spec("sweep_heatmap","Heatmap de MAPE (método × cenário)","heatmap","sweep",Vector("metric","methods")),Spec("sweep_coverage","Cobertura por cenário parcial","line","sweep",Vector("methods")),
    Spec("cluster_sizes","Séries por cluster","bar","clustering"),Spec("centroids","Centróides dos clusters","line","clustering"))
  def ordered(keys: Set[String]): Vector[String] = Metrics.keys.filter(keys)++(keys--Metrics.keys).toVector.sorted
  def methods(raw: Option[String]): Option[Vector[String]] = raw.filter(_.nonEmpty).map(_.split(',').map(_.trim).filter(_.nonEmpty).map(Exports.valid).toVector)
  def sweep(root: Path,prefix: String="sweep",filter: Option[Vector[String]]=None): JsObject =
    val scenarios=Exports.read(root,s"${prefix}_summary").field("scenarios").values.sortBy(_.field("percentage").number())
    if scenarios.isEmpty then throw new FileNotFoundException("No sweep scenarios")
    val found=scenarios.flatMap(_.field("methods",empty).asJsObject.fields.keys.toVector.sorted).distinct
    if filter.exists(_.exists(m=> !found.contains(m))) then throw new FileNotFoundException("Method absent from sweep")
    val ms=found.filter(m=>filter.forall(_.contains(m)))
    val metrics=ordered(scenarios.flatMap(_.field("methods",empty).asJsObject.fields.values.flatMap(_.field("mape",empty).asJsObject.fields.keys)).toSet)
    def column(field: String,key: Option[String]=None): JsObject = JsObject(ms.map { m=>m->JsArray(scenarios.map { s=>
      val value=s.field("methods").field(m).field(field);key.fold(value)(value.field(_))
    }) }.toMap)
    obj("prefix"->JsString(prefix),"scenarios"->JsArray(scenarios.map(s=>obj("label"->s.field("label"),"percentage"->s.field("percentage")))),"methods"->strings(ms),
      "metrics"->JsArray(metrics.map(k=>obj("key"->JsString(k),"label"->JsString(Exports.labels.getOrElse(k,k))))),
      "mape"->JsObject(metrics.map(k=>k->column("mape",Some(k))).toMap),"error_pct"->JsObject(metrics.map(k=>k->column("error_pct",Some(k))).toMap),"coverage_pct"->column("coverage_pct"))
  def catalog(root: Path,compare: String="compare",sweep: String="sweep",estimation: String="result",clustering: String="clustering"): JsArray =
    Vector(compare,sweep,estimation,clustering).foreach(Exports.valid)
    val ready=Map("compare"->Exports.methods(root,compare,Set("summary")).nonEmpty,"estimation"->(Exports.methods(root,estimation).nonEmpty && Files.isRegularFile(root.resolve("new_simulation.json"))),
      "sweep"->Files.isRegularFile(root.resolve(s"${sweep}_summary.json")),"clustering"->Files.isRegularFile(root.resolve(s"$clustering.json")))
    JsArray(specs.map(s=>obj("id"->JsString(s.id),"title"->JsString(s.title),"type"->JsString(s.kind),"source"->JsString(s.source),"params"->strings(s.params),"available"->JsBoolean(ready(s.source)),
      "metrics"->(if s.params.contains("metric") then strings(Metrics.keys) else JsNull),"url"->JsString(s"charts/${s.id}"))))
  def build(root: Path,id: String,params: Map[String,String]=Map.empty,compare: String="compare",sweepPrefix: String="sweep",estimation: String="result",clustering: String="clustering"): JsObject =
    Vector(compare,sweepPrefix,estimation,clustering).foreach(Exports.valid)
    val spec=specs.find(_.id==id).getOrElse(throw new FileNotFoundException(id))
    val metric=params.getOrElse("metric","average_speed");require(Metrics.keys.contains(metric),"Invalid metric")
    val label=Exports.labels(metric)
    def axis(label: String,categories: JsValue=JsNull,unit: String=""): JsObject =
      JsObject(Map("label"->JsString(label))++(if categories==JsNull then Map.empty else Map("categories"->categories))++(if unit.isEmpty then Map.empty else Map("unit"->JsString(unit))))
    def chart(title: String=spec.title,x: JsObject=empty,y: JsObject=empty,series: Vector[JsValue]=Vector.empty,meta: JsObject=empty,extra: Map[String,JsValue]=Map.empty): JsObject =
      JsObject(Map("id"->JsString(id),"title"->JsString(title),"type"->JsString(spec.kind),"source"->JsString(spec.source),"x"->x,"y"->y,"series"->JsArray(series),"meta"->meta)++extra)
    if spec.source=="compare" then
      val ms=Exports.methods(root,compare,Set("summary"));if ms.isEmpty then throw new FileNotFoundException("No comparisons")
      val results=ms.map(m=>m->Exports.read(root,s"${compare}_$m")).toMap
      val metrics=ordered(results.values.flatMap(_.field("per_series_error",empty).asJsObject.fields.keys).toSet)
      val categories=strings(metrics.map(k=>Exports.labels.getOrElse(k,k)))
      id match
        case "error_aggregate" | "mape_by_method" | "mape_radar"=>
          val series=ms.map { m=>obj("name"->JsString(m),"data"->JsArray(metrics.map(k=>if id=="error_aggregate" then results(m).field("aggregate").field("error_pct").field(k) else results(m).field("per_series_error").field(k).field("mape")))) }
          chart(x=axis("Métrica",categories),y=axis(if id=="error_aggregate" then "Erro (%)" else "MAPE (%)",unit="%"),series=series)
        case "coverage"=>
          chart(x=axis("Método",strings(ms)),y=axis("Cobertura (%)",unit="%"),series=Vector(obj("name"->JsString("Cobertura"),"data"->JsArray(ms.map(m=>results(m).field("coverage_pct"))))),
            meta=obj("n_real"->JsObject(ms.map(m=>m->results(m).field("n_real")).toMap),"n_estimated"->JsObject(ms.map(m=>m->results(m).field("n_estimated")).toMap)))
        case _=>
          val boxes=ms.map { m=>val stats=results(m).field("per_series_error").field(metric)
            obj("min"->stats.field("min"),"q1"->stats.field("p25"),"median"->stats.field("median"),"q3"->stats.field("p75"),"max"->stats.field("max"),"mean"->stats.field("mean"))
          }
          chart(s"Distribuição de erros — $label",axis("Método",strings(ms)),axis("Erro por série (%)",unit="%"),Vector(obj("name"->JsString(label),"data"->JsArray(boxes))),obj("metric"->JsString(metric)))
    else if spec.source=="sweep" then
      val table=sweep(root,sweepPrefix,methods(params.get("methods")));val ms=table.field("methods").names
      val scenarios=table.field("scenarios").values;val categories=JsArray(scenarios.map(_.field("label")))
      val meta=obj("percentages"->JsArray(scenarios.map(_.field("percentage"))))
      val x=axis("Cenário parcial",categories)
      if id=="sweep_coverage" then chart(x=x,y=axis("Cobertura (%)",unit="%"),series=ms.map(m=>obj("name"->JsString(m),"data"->table.field("coverage_pct").field(m))),meta=meta)
      else
        val columns=table.field("mape").field(metric);if columns==JsNull then throw new FileNotFoundException("Metric absent from sweep")
        val context=JsObject(meta.fields+("metric"->JsString(metric)))
        if id=="sweep_progression" then chart(s"Progressão do MAPE — $label",x,axis("MAPE (%)",unit="%"),ms.map(m=>obj("name"->JsString(m),"data"->columns.field(m))),context)
        else chart(s"Heatmap de MAPE — $label",x,axis("Método",strings(ms),"%"),meta=context,extra=Map("values"->JsArray(ms.map(columns.field(_)))))
    else if spec.source=="clustering" then
      val data=Exports.read(root,clustering)
      if id=="cluster_sizes" then
        val sizes=data.field("time_series_by_cluster",empty).asJsObject.fields;val ks=sizes.keys.toVector.sortBy(_.toInt)
        chart(x=axis("Cluster",strings(ks)),y=axis("Séries"),series=Vector(obj("name"->JsString("Séries"),"data"->JsArray(ks.map(sizes)))))
      else
        val centroids=data.field("centroids",empty).asJsObject.fields;val ks=centroids.keys.toVector.sortBy(_.toInt)
        val width=centroids.values.map(_.values.size).maxOption.getOrElse(0)
        chart(x=axis("Ponto da série (normalizada)",ints(0 until width)),y=axis("Valor (z-score)"),series=ks.map(k=>obj("name"->JsString(s"cluster $k"),"data"->JsArray(centroids(k).values.map {case JsArray(v)=>v.headOption.getOrElse(JsNull);case v=>v}))))
    else
      val ms=Exports.methods(root,estimation);if ms.isEmpty then throw new FileNotFoundException("No estimates")
      val method=Exports.valid(params.getOrElse("method",ms.head));val limit=params.getOrElse("limit","500").toInt;require(limit>=1,"limit must be positive")
      val real=Exports.read(root,"new_simulation").field(metric).values;val est=Exports.read(root,s"${estimation}_$method").field(metric).values
      val labels=try Exports.read(root,"time_series_labels").names catch case _: FileNotFoundException=>Vector.empty
      val n=math.min(real.size,est.size);val step=math.max(1,math.ceil(n.toDouble/limit).toInt)
      val points=(0 until n by step).filter(i=>real(i)!=JsNull && est(i)!=JsNull).map(i=>obj("x"->real(i),"y"->est(i),"label"->JsString(labels.lift(i).getOrElse(i.toString)))).toVector
      chart(s"Real vs. estimado — $label ($method)",axis(s"Real — $label"),axis(s"Estimado — $label"),Vector(obj("name"->JsString(method),"data"->JsArray(points))),
        obj("reference"->JsString("identity"),"metric"->JsString(metric),"method"->JsString(method),"total_series"->JsNumber(n),"returned"->JsNumber(points.size),"step"->JsNumber(step)))
