package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKit
import org.apache.pekko.http.scaladsl.Http
import spray.json.*
import Json.*
import java.nio.file.Files
import java.net.{URI,InetSocketAddress}
import java.net.http.{HttpClient,HttpRequest,HttpResponse}
import com.sun.net.httpserver.HttpServer
import scala.concurrent.Await
import scala.concurrent.duration.*

class PlatformApiSpec extends AnyFlatSpec with Matchers:
  "Platform integration" should "forward all allowlisted control routes and preserve native computation ownership" in {
    val upstream=HttpServer.create(InetSocketAddress("127.0.0.1",0),0)
    upstream.createContext("/prefix/api",exchange=>{
      val path=exchange.getRequestURI.getPath.stripPrefix("/prefix")
      val method=exchange.getRequestMethod
      val body=new String(exchange.getRequestBody.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8)
      val created=method=="POST" && path=="/api/htc/runs"
      val result=if created then obj("run_id"->JsString("htc-remote"))
        else if path=="/api/runs/htc-remote" then obj("run_id"->JsString("htc-remote"),"created_at"->JsNumber(100),"status"->JsString("running"))
        else obj("path"->JsString(path),"method"->JsString(method),"body"->JsString(body),"query"->JsString(Option(exchange.getRequestURI.getRawQuery).getOrElse("")))
      val bytes=result.compactPrint.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      exchange.getResponseHeaders.add("Content-Type","application/json")
      exchange.sendResponseHeaders(if created then 202 else 200,bytes.length)
      exchange.getResponseBody.write(bytes);exchange.close()
    })
    upstream.start()
    val kit=ActorTestKit("PlatformApiSpec");given org.apache.pekko.actor.typed.ActorSystem[?]=kit.system
    val service=new Api(Files.createTempDirectory("platform-api-"),Some(s"http://127.0.0.1:${upstream.getAddress.getPort}/prefix/api/"))
    val binding=Await.result(Http()(kit.system).newServerAt("127.0.0.1",0).bind(service.route),15.seconds)
    val client=HttpClient.newHttpClient();val base=s"http://127.0.0.1:${binding.localAddress.getPort}"
    def call(method: String,path: String): HttpResponse[String] =
      val publisher=if Set("GET","DELETE","OPTIONS")(method) then HttpRequest.BodyPublishers.noBody() else HttpRequest.BodyPublishers.ofString("{\"test\":true}")
      client.send(HttpRequest.newBuilder(URI.create(base+path)).header("Content-Type","application/json").method(method,publisher).build(),HttpResponse.BodyHandlers.ofString())
    try
      service.platform.specifications.size shouldBe 66
      service.platform.specifications.foreach { spec=>
        val path="<[^>]+>".r.replaceAllIn(spec.field("path").text(),"fixture")
        spec.field("methods").names.foreach { method=>
          val response=call(method,path+"?name=a%20b")
          withClue(s"$method $path: ${response.body()}") {
            response.statusCode() shouldBe (if method=="POST" && path=="/api/htc/runs" then 202 else 200)
            response.headers().firstValue("Access-Control-Allow-Origin").orElse("") shouldBe "*"
            if path!="/api/htc/runs" then
              val value=response.body().parseJson
              value.field("path").text() shouldBe path
              value.field("method").text() shouldBe method
              value.field("query").text() shouldBe "name=a%20b"
              if !Set("GET","DELETE")(method) then value.field("body").text() shouldBe "{\"test\":true}"
          }
        }
      }
      val runs=call("GET","/api/runs").body().parseJson.values
      runs.map(_.field("run_id").text()) should contain("htc-remote")
      call("GET","/api/runs/htc-remote").body().parseJson.field("status").text() shouldBe "running"
      call("GET","/api/platform/status").body().parseJson.field("configured").bool() shouldBe true
      call("GET","/api/estimation-variants").body().parseJson.field("variants").values.size shouldBe 100
      call("POST","/api/runs").statusCode() shouldBe 400
      call("OPTIONS","/api/locations").statusCode() shouldBe 204
    finally
      Await.result(binding.unbind(),10.seconds);service.close();kit.shutdownTestKit();upstream.stop(0)
  }
  it should "return an explicit unavailable response when no platform service is configured" in {
    val kit=ActorTestKit("UnconfiguredPlatformApiSpec");given org.apache.pekko.actor.typed.ActorSystem[?]=kit.system
    val service=new Api(Files.createTempDirectory("no-platform-api-"),None)
    val binding=Await.result(Http()(kit.system).newServerAt("127.0.0.1",0).bind(service.route),15.seconds)
    try
      val response=HttpClient.newHttpClient().send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:${binding.localAddress.getPort}/api/locations")).build(),HttpResponse.BodyHandlers.ofString())
      response.statusCode() shouldBe 503
      response.body().parseJson.field("implementation").text() shouldBe "external_htc_service"
    finally
      Await.result(binding.unbind(),10.seconds);service.close();kit.shutdownTestKit()
  }
