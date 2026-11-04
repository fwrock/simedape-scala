package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.io.FileNotFoundException
import java.util.concurrent.Executors
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.http.scaladsl.model.*
import org.apache.pekko.http.scaladsl.server.{Route,ExceptionHandler}
import org.apache.pekko.http.scaladsl.server.Directives.*
import scala.concurrent.{Future,ExecutionContext}
import scala.util.Using
import scala.jdk.CollectionConverters.*

/** Python-compatible /api routes alongside the existing /api/v1 actor interface. */
final class Api(val root: Path,platformURL: Option[String]=sys.env.get("SIMEDAPE_PLATFORM_API_URL"))(using system: ActorSystem[?]) extends AutoCloseable:
  Files.createDirectories(root)
  private val configs=root.resolve("configs");Files.createDirectories(configs)
  val jobs=new Jobs(root.resolve("jobs"),sys.env.get("SIMEDAPE_JOB_WORKERS").fold(1)(_.toInt))
  private val executor=Executors.newFixedThreadPool(4)
  private given ExecutionContext=ExecutionContext.fromExecutor(executor)
  val platform=new PlatformApi(root,platformURL)
  private def json(value: JsValue,status: StatusCode=StatusCodes.OK): HttpResponse = HttpResponse(status,entity=HttpEntity(ContentTypes.`application/json`,value.compactPrint))
  private def configFile(name: String): Path = Json.safe(configs,Exports.valid(name)+".json")
  private def config(name: String): JsObject =
    val path=configFile(name);if !Files.isRegularFile(path) then throw new FileNotFoundException(s"Configuration not found: $name")
    Selection.read(path).asJsObject
  private def names: Vector[String] = Using.resource(Files.list(configs))(_.iterator().asScala.map(_.getFileName.toString).filter(_.endsWith(".json")).map(_.stripSuffix(".json")).toVector.sorted)
  private def async(action: =>HttpResponse): Route = complete(Future(action))
  private def prefix(job: JsValue,params: Map[String,String],stage: String,key: String,default: String): String =
    Exports.valid(params.getOrElse("prefix",job.field("meta").field("stage_params").field(stage).field(key,JsString(default)).text()))
  private def routeRun(id: String,resource: String,params: Map[String,String]): HttpResponse =
    val job=jobs.get(id);val meta=job.field("meta");val out=Path.of(meta.field("output_dir").text())
    def response(value: JsObject): HttpResponse = json(JsObject(value.fields+("run_id"->JsString(id))))
    resource match
      case ""=>json(job)
      case "progress"=>response(JsObject(job.fields.filter((k,_)=>Set("name","status","created_at","started_at","finished_at","error")(k))++job.field("progress").asJsObject.fields+("data_source"->meta.field("data_source"))))
      case "artifacts"=>response(obj("output_dir"->meta.field("output_dir"),"data_source"->meta.field("data_source"),"artifacts"->JsArray(Exports.artifacts(out,meta.field("clustering_file").text("clustering")))))
      case "model"=>response(Exports.model(out))
      case "clustering-report"=>
        val model=ModelIO.read(Exports.modelFile(out))
        response(model.report.getOrElse(throw new FileNotFoundException("Model has no clustering report")).api(params.get("cluster").map(_.toInt),params.getOrElse("near","5").toInt,params.getOrElse("far","5").toInt))
      case "model/download"=>
        val file=Exports.modelFile(out)
        HttpResponse(entity=HttpEntity.Default(ContentTypes.`application/octet-stream`,Files.size(file),org.apache.pekko.stream.scaladsl.FileIO.fromPath(file))).withHeaders(headers.`Content-Disposition`(headers.ContentDispositionTypes.attachment,Map("filename"->s"$id.simedape")))
      case "estimation"=>
        val pre=prefix(job,params,"estimation","output_prefix","result")
        params.get("method") match
          case None=>response(Exports.estimation(out,pre))
          case Some(method)=>response(Exports.detail(out,pre,method,params.get("metric"),math.max(0,params.getOrElse("offset","0").toInt),params.get("limit").map(v=>math.max(0,v.toInt))))
      case "comparison"=>
        val pre=prefix(job,params,"compare","output_prefix","compare")
        params.get("method") match
          case None=>response(Exports.comparison(out,pre))
          case Some(method)=>response(JsObject(Exports.read(out,s"${pre}_${Exports.valid(method)}").asJsObject.fields+("method"->JsString(method))))
      case "charts"=>response(obj("data_source"->meta.field("data_source"),"charts"->Charts.catalog(out,
        prefix(job,Map.empty,"compare","output_prefix","compare"),prefix(job,Map.empty,"sweep","output_prefix","sweep"),prefix(job,Map.empty,"estimation","output_prefix","result"),meta.field("clustering_file").text("clustering"))))
      case resource if resource.startsWith("charts/")=>response(Charts.build(out,resource.stripPrefix("charts/"),params,
        prefix(job,Map.empty,"compare","output_prefix","compare"),prefix(job,Map.empty,"sweep","output_prefix","sweep"),prefix(job,Map.empty,"estimation","output_prefix","result"),meta.field("clustering_file").text("clustering")))
      case "sweep"=>response(Charts.sweep(out,prefix(job,params,"sweep","output_prefix","sweep"),Charts.methods(params.get("methods"))))
      case "report.pdf" | "latex" | "export.zip"=>
        val report=Reports.build(out,params,prefix(job,Map.empty,"compare","output_prefix","compare"),prefix(job,Map.empty,"sweep","output_prefix","sweep"),job.field("name").text("SimEDaPE"))
        val (bytes,mime,filename)=resource match
          case "report.pdf"=>(report.pdf,ContentType(MediaTypes.`application/pdf`),s"$id.pdf")
          case "export.zip"=>(report.zip(job.field("name").text("SimEDaPE")),ContentType(MediaTypes.`application/zip`),s"$id-article.zip")
          case _=>
            val text=params.get("table").map(n=>report.tables.find(_._1==n).getOrElse(throw new FileNotFoundException(s"Unknown table: $n"))._2).getOrElse(report.tables.map(_._2).mkString("\n"))
            (text.getBytes(java.nio.charset.StandardCharsets.UTF_8),ContentTypes.`text/plain(UTF-8)`,s"$id-tables.tex")
        HttpResponse(entity=HttpEntity(mime,bytes)).withHeaders(headers.`Content-Disposition`(headers.ContentDispositionTypes.attachment,Map("filename"->filename)))
      case "reports"=>
        val reports=Exports.artifacts(out).filter(a=>a.field("kind").text()=="report").map(a=>JsObject((a.asJsObject.fields-"kind")+("name"->JsString(a.field("name").text().stripPrefix("plots/")))))
        response(obj("reports"->JsArray(reports)))
      case other if other.startsWith("reports/")=>
        val name=Exports.valid(other.stripPrefix("reports/"));require(name.endsWith(".pdf"),"Expected PDF")
        val file=Json.safe(out,s"plots/$name");if !Files.isRegularFile(file) then throw new FileNotFoundException(name)
        HttpResponse(entity=HttpEntity.Default(ContentType(MediaTypes.`application/pdf`),Files.size(file),org.apache.pekko.stream.scaladsl.FileIO.fromPath(file)))
      case _=>throw new FileNotFoundException(s"Unknown run resource: $resource")
  val route: Route =
    val errors=ExceptionHandler {
      case e: FileNotFoundException=>complete(json(obj("error"->JsString(e.getMessage)),StatusCodes.NotFound))
      case e: IllegalArgumentException=>complete(json(obj("error"->JsString(e.getMessage)),StatusCodes.BadRequest))
      case e: spray.json.DeserializationException=>complete(json(obj("error"->JsString(e.getMessage)),StatusCodes.BadRequest))
      case e: spray.json.JsonParser.ParsingException=>complete(json(obj("error"->JsString(e.getMessage)),StatusCodes.BadRequest))
    }
    respondWithHeaders(
      headers.`Access-Control-Allow-Origin`.*,
      headers.`Access-Control-Allow-Methods`(HttpMethods.GET,HttpMethods.POST,HttpMethods.PUT,HttpMethods.DELETE,HttpMethods.PATCH,HttpMethods.OPTIONS),
      headers.`Access-Control-Allow-Headers`("Content-Type","Authorization","Accept")
    ) { handleExceptions(errors) {
      pathPrefix("api") {
        concat(
          path("platform" / "status")(get(complete(json(platform.status)))),
          options(complete(StatusCodes.NoContent)),
          platform.route,
          path("health")(get(complete(json(obj("status"->JsString("ok")))))),
          path("simulations") { get { parameterMap { params=>async {
            val cfg=RunConfig(params.get("config").map(config).getOrElse(Configuration.normalize(Json.empty)))
            try json(obj("source"->JsString("clickhouse"),"simulations"->Sources.simulations(cfg)))
            catch case scala.util.control.NonFatal(e)=>json(obj("error"->JsString(s"ClickHouse unavailable: ${e.getMessage}")),StatusCodes.ServiceUnavailable)
          } } } },
          path("stages") { get {
            complete(json(JsArray(Pipeline.stages.filter(s=>Pipeline.implemented(s.field("name").text())).map(s=>JsObject(s.asJsObject.fields-"parameters")))))
          } },
          path("estimation-variants") { get {
            complete(json(obj("interpolations"->strings(Vector("linear","poly","lagrange","newton","vandermonde","spline")),"default"->strings(Vector("avg","base","sample","wavg")),"variants"->JsArray(Variants.catalog.map(_.metadata)))))
          } },
          path("configs") {
            concat(get(async(json(JsArray(names.map(n=>obj("name"->JsString(n),"config"->config(n))))))),
              post(entity(as[String]) { body=>async {
                val value=body.parseJson;val name=Exports.valid(value.field("name").text());val cfg=Configuration.normalize(value.field("config").asJsObject)
                Json.write(configFile(name),cfg);json(obj("name"->JsString(name),"config"->cfg),StatusCodes.Created)
              } }))
          },
          path("configs" / Segment) { name=>
            concat(get(async(json(obj("name"->JsString(name),"config"->config(name))))),
              put(entity(as[String]) { body=>async {
                config(name);val value=body.parseJson;val cfg=Configuration.normalize(value.field("config",value).asJsObject)
                Json.write(configFile(name),cfg);json(obj("name"->JsString(name),"config"->cfg))
              } }),delete(async {config(name);Files.delete(configFile(name));json(obj("deleted"->JsString(name))) }))
          },
          path("runs") {
            concat(get(complete(platform.mergeRuns(jobs.list).map(json(_)))),post(entity(as[String]) { body=>async {
              val request=body.parseJson;val ref=request.field("config",empty)
              val cfg=RunConfig(Configuration.normalize(ref match {case JsString(n)=>config(n);case v=>v.asJsObject}))
              val stages=request.field("stages").names.map(n=>StageRequest(n,request.field("stage_params").field(n,empty).asJsObject))
              val out=Path.of(cfg.json.field("output_dir").text())
              val in=cfg.json.field("input_dir").text();val id=jobs.submit(cfg,stages,out,if in.isEmpty then None else Some(Path.of(in)),request.field("name").text(),if ref.isInstanceOf[JsString] then ref else JsNull)
              json(obj("run_id"->JsString(id)),StatusCodes.Accepted)
            } }))
          },
          pathPrefix("runs" / Segment) { id=>
            extractRequest { request=>
              if !jobs.contains(id) && platform.configured then complete(platform.foreignRun(request))
              else concat(pathEndOrSingleSlash {
              concat(get(async(routeRun(id,"",Map.empty))),delete(async {
                if jobs.cancel(id) then json(obj("run_id"->JsString(id),"status"->JsString("cancelled"))) else json(obj("error"->JsString("Run already finished")),StatusCodes.Conflict)
              }))
            },get(path(Remaining) { resource=>parameterMap(params=>async(routeRun(id,resource,params))) }))
            }
          }
        )
      }
    }
    }
  def close(): Unit = {jobs.close();executor.shutdownNow()}
