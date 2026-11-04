package org.interscity.htc.parity
import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.Directives.*
import scala.concurrent.{Future,ExecutionContext}
import scala.concurrent.duration.*
import scala.util.{Using,control}

/** Explicit integration with existing HTC control services. SimEDaPE computation is never forwarded. */
final class PlatformApi(state: Path,backendURL: Option[String])(using system: ActorSystem[?],ec: ExecutionContext):
  val backend: Option[Uri]=backendURL.filter(_.nonEmpty).map { text=>
    val uri=Uri(text)
    require(Set("http","https")(uri.scheme) && uri.authority.host.address().nonEmpty && uri.authority.userinfo.isEmpty,"Platform URL requires an HTTP(S) origin without embedded credentials")
    require(uri.rawQueryString.isEmpty && uri.fragment.isEmpty,"Platform URL cannot contain a query or fragment")
    val prefix=uri.path.toString.stripSuffix("/").stripSuffix("/api").stripSuffix("/")
    uri.withPath(Uri.Path(prefix))
  }
  private val lock=new Object
  private val cache=state.resolve("platform-jobs.json")
  private var known: Map[String,JsValue]=if Files.isRegularFile(cache) then Json.read(cache).asJsObject.fields else Map.empty
  val specifications: Vector[JsValue]=Using.resource(getClass.getResourceAsStream("/parity/platform-routes.json"))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson.values)
  private def pattern(path: String): scala.util.matching.Regex =
    val variable="<([^>]+)>".r
    val result=new StringBuilder("^");var previous=0
    variable.findAllMatchIn(path).foreach { m=>
      result.append(java.util.regex.Pattern.quote(path.substring(previous,m.start)))
      result.append(if m.group(1).startsWith("path:") then ".+" else "[^/]+")
      previous=m.end
    }
    result.append(java.util.regex.Pattern.quote(path.substring(previous)));result.append("$");result.toString.r
  private val routes=specifications.map(s=>(pattern(s.field("path").text()),s.field("methods").names.toSet))
  def configured: Boolean = backend.nonEmpty
  def matches(request: HttpRequest): Boolean = routes.exists((p,methods)=>methods(request.method.value) && p.matches(request.uri.path.toString))
  private def failure(message: String,status: StatusCode=StatusCodes.ServiceUnavailable): HttpResponse =
    HttpResponse(status,entity=HttpEntity(ContentTypes.`application/json`,obj("error"->JsString(message),"implementation"->JsString("external_htc_service")).compactPrint))
  private val excluded=Set("host","connection","keep-alive","proxy-authenticate","proxy-authorization","te","trailer","transfer-encoding","upgrade","content-length","timeout-access","remote-address","raw-request-uri")
  private def send(request: HttpRequest): Future[HttpResponse] = backend match
    case None=>
      request.discardEntityBytes();Future.successful(failure("HTC platform integration is not configured; set SIMEDAPE_PLATFORM_API_URL"))
    case Some(base)=>
      if request.headers.exists(_.lowercaseName()=="x-simedape-platform-hop") then
        request.discardEntityBytes();Future.successful(failure("Platform backend points back to this service",StatusCodes.BadGateway))
      else
        val target=base.withPath(base.path++request.uri.path)
        val destination=request.uri.rawQueryString.fold(target)(target.withRawQueryString)
        val forwarded=request.withUri(destination).withHeaders(request.headers.filterNot(h=>excluded(h.lowercaseName())):+headers.RawHeader("X-SimEDaPE-Platform-Hop","1"))
        Http()(system).singleRequest(forwarded).map { response=>
          response.withHeaders(response.headers.filterNot(h=>excluded(h.lowercaseName()) || h.lowercaseName().startsWith("access-control-allow-")))
        }.recover {case control.NonFatal(e)=>failure(s"HTC platform service unavailable: ${e.getMessage}")}
  private def track(response: HttpResponse): Future[HttpResponse] =
    if response.status!=StatusCodes.Accepted then Future.successful(response)
    else response.entity.withSizeLimit(1048576).toStrict(10.seconds).map { entity=>
      val body=entity.data.utf8String.parseJson
      val id=body.field("run_id").text()
      if id.nonEmpty then lock.synchronized {
        known=known+(Exports.valid(id)->num(System.currentTimeMillis()/1000.0));Json.write(cache,JsObject(known))
      }
      response.withEntity(entity)
    }
  def forward(request: HttpRequest): Future[HttpResponse] =
    require(matches(request),"Route is not an HTC platform integration")
    send(request).flatMap(r=>if request.method==HttpMethods.POST && request.uri.path.toString=="/api/htc/runs" then track(r) else Future.successful(r))
  /** Reads/cancellation of externally created HTC jobs retain their original service ownership. */
  def foreignRun(request: HttpRequest): Future[HttpResponse] =
    require((request.method==HttpMethods.GET || request.method==HttpMethods.DELETE) && request.uri.path.toString.matches("/api/runs/[^/]+(?:/.*)?"),"Invalid external run resource")
    send(request)
  def mergeRuns(local: Vector[JsObject]): Future[JsArray] =
    val ids=lock.synchronized(known.keys.toVector.sorted).filterNot(id=>local.exists(_.field("run_id").text()==id))
    if !configured || ids.isEmpty then Future.successful(JsArray(local))
    else
      val loaded=ids.grouped(8).foldLeft(Future.successful(Vector.empty[JsValue])) { (before,batch)=>
        before.flatMap { previous=>
          Future.sequence(batch.map { id=>
            send(HttpRequest(uri=Uri(s"/api/runs/${Exports.valid(id)}"))).flatMap { response=>
              if response.status==StatusCodes.OK then response.entity.withSizeLimit(16777216).toStrict(10.seconds).map(e=>Some(e.data.utf8String.parseJson))
              else {response.discardEntityBytes();Future.successful(None)}
            }.recover {case control.NonFatal(_)=>None}
          }).map(rows=>previous++rows.flatten)
        }
      }
      loaded.map(remote=>JsArray((local++remote).sortBy(-_.field("created_at").number())))
  def status: JsObject = obj("mode"->JsString("service_integration"),"configured"->JsBoolean(configured),"backend_origin"->backend.map(u=>JsString(s"${u.scheme}://${u.authority}")).getOrElse(JsNull),
    "integrated_routes"->JsNumber(specifications.size),"native_pipeline_stages"->JsNumber(Pipeline.implemented.size),"native_estimation_variants"->JsNumber(Variants.catalog.size))
  val route: Route = extractRequest { request=>if matches(request) then complete(forward(request)) else reject }
