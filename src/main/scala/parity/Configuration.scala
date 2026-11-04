package org.interscity.htc.parity
import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object Configuration:
  def normalize(value: JsObject): JsObject =
    val name=value.field("scenario_name",JsString("default")).text()
    def merge(defaults: JsObject,key: String): JsObject = JsObject(defaults.fields++value.field(key,empty).asJsObject.fields)
    val ch=obj("host"->JsString(sys.env.getOrElse("HTC_CLICKHOUSE_HOST","localhost")),"port"->JsNumber(sys.env.getOrElse("HTC_CLICKHOUSE_PORT","8123").toInt),
      "database"->JsString(sys.env.getOrElse("HTC_CLICKHOUSE_DATABASE","htc")),"user"->JsString(sys.env.getOrElse("HTC_CLICKHOUSE_USER","default")),"password"->JsString(sys.env.getOrElse("HTC_CLICKHOUSE_PASSWORD","")),
      "table"->JsString("vehicle_link_events"),"simulation_id"->JsString(""),"secure"->JsBoolean(false))
    val defaults=obj("scenario_name"->JsString(name),"scenario_dir"->JsString(""),"street_info_file"->JsString(""),"csv_sep"->JsString(";"),"compact_model"->JsBoolean(true),"model_precision"->JsString("float64"),
      "output_dir"->JsString(s"output/$name/"),"input_dir"->JsString(""),"transport_modes"->strings(Vector("car")),"data_source"->JsString("csv"))
    val result=JsObject(defaults.fields++value.fields++Map(
      "intervals"->merge(obj("start_time"->JsNumber(0),"end_time"->JsNumber(86400),"interval_size"->JsNumber(3600)),"intervals"),
      "clustering"->merge(obj("n_clusters"->JsNumber(36),"verbose"->JsBoolean(true),"random_state"->JsNumber(10),"max_iter"->JsNumber(8),"output_file"->JsString("clustering"),"clustering_size"->JsNumber(0)),"clustering"),"clickhouse"->merge(ch,"clickhouse")))
    RunConfig(result);result
  def load(file: Path): JsValue =
    if file.toString.endsWith(".properties") then properties(file)
    else if file.toString.endsWith(".json") then Json.read(file)
    else
      def convert(value: Any): JsValue = value match
        case null=>JsNull
        case m: java.util.Map[?,?]=>JsObject(m.asScala.toVector.map((k,v)=>k.toString->convert(v)).toMap)
        case l: java.util.List[?]=>JsArray(l.asScala.toVector.map(convert))
        case b: java.lang.Boolean=>JsBoolean(b.booleanValue())
        case n: java.lang.Number=>JsNumber(BigDecimal(n.toString))
        case s=>JsString(s.toString)
      val options=new org.yaml.snakeyaml.LoaderOptions();options.setAllowDuplicateKeys(false);options.setMaxAliasesForCollections(50)
      Using.resource(Files.newInputStream(file))(s=>convert(new org.yaml.snakeyaml.Yaml(new org.yaml.snakeyaml.constructor.SafeConstructor(options)).load[Any](s)))
  private def properties(file: Path): JsObject =
    val p=new java.util.Properties();Using.resource(Files.newInputStream(file))(p.load)
    def get(key: String,default: String=""): JsValue = JsString(p.getProperty(key,default))
    def number(key: String,default: Int): JsValue = JsNumber(p.getProperty(key,default.toString).toLong)
    obj("config"->obj("scenario_name"->get("scenario.name","default"),"scenario_dir"->get("scenario.single.dir.files"),"street_info_file"->get("scenario.single.dir.street.info"),"input_dir"->get("scneario.input.dir"),"csv_sep"->get("csv.sep",";"),
      "data_source"->get("data.source","csv"),"transport_modes"->strings(p.getProperty("transport.modes","car").split(',').map(_.trim).filter(_.nonEmpty).toVector),
      "intervals"->obj("start_time"->number("intervals.start_time",0),"end_time"->number("intervals.end_time",86400),"interval_size"->number("intervals.interval_size",3600),"sample_period"->number("intervals.sample_period",0)),
      "clustering"->obj("n_clusters"->number("clustering.n_clusters",36),"random_state"->number("clustering.random_state",10),"max_iter"->number("clustering.max_iter",8),"clustering_size"->number("clustering.clustering_size",0),"output_file"->get("clustering.output.save.file","clustering"),"verbose"->JsBoolean(p.getProperty("clustering.verbose","True")=="True")),
      "clickhouse"->JsObject(p.stringPropertyNames().asScala.filter(_.startsWith("clickhouse.")).map(k=>k.stripPrefix("clickhouse.")->(if k=="clickhouse.secure" then JsBoolean(p.getProperty(k)=="True") else if k=="clickhouse.port" then number(k,8123) else get(k))).toMap)))
  def run(file: Path,stageNames: Vector[String]=Vector.empty): JsObject =
    val document=load(file);val config=RunConfig(normalize(document.field("config",document).asJsObject))
    val stages=if stageNames.nonEmpty then stageNames.map(StageRequest(_)) else document.field("stages").values.map {
      case JsString(s)=>StageRequest(s)
      case s=>StageRequest(s.field("name").text(),s.field("params",empty).asJsObject)
    }
    val input=config.json.field("input_dir").text()
    new Pipeline(config,Path.of(config.json.field("output_dir").text()),if input.isEmpty then None else Some(Path.of(input)),println).run(stages,e=>println(e.compactPrint))
