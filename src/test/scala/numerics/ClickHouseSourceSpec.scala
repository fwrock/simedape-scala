package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.*
import Json.*
import com.sun.net.httpserver.HttpServer
import java.net.{InetSocketAddress,URLDecoder}
import java.nio.file.Files

class ClickHouseSourceSpec extends AnyFlatSpec with Matchers:
  "ClickHouse ingestion" should "stream authenticated JSONEachRow with typed simulation and horizon parameters" in {
    val fixture=scala.io.Source.fromResource("parity/occupancy.json").mkString.parseJson
    val rows=fixture.field("events").values.sortBy(e=>(e.field("link_id").text(),e.field("tick").number()))
    val server=HttpServer.create(new InetSocketAddress("127.0.0.1",0),0)
    var query="";var user="";var password=""
    server.createContext("/",exchange=>{
      query=URLDecoder.decode(exchange.getRequestURI.getRawQuery,"UTF-8");user=exchange.getRequestHeaders.getFirst("X-ClickHouse-User");password=exchange.getRequestHeaders.getFirst("X-ClickHouse-Key")
      val data=rows.map(_.compactPrint).mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
      exchange.sendResponseHeaders(200,data.length);exchange.getResponseBody.write(data);exchange.close()
    });server.start()
    val roadFile=Files.createTempFile("roads-",".csv")
    Files.writeString(roadFile,"link;length;capacity;freespeed\n"+fixture.field("roads").asJsObject.fields.map((k,v)=>s"$k;${v.values.map(_.number()).mkString(";")}").mkString("\n"))
    try
      val config=RunConfig(obj("data_source"->JsString("clickhouse"),"street_info_file"->JsString(roadFile.toString),"intervals"->fixture.field("intervals"),
        "clickhouse"->obj("host"->JsString("127.0.0.1"),"port"->JsNumber(server.getAddress.getPort),"user"->JsString("fixture"),"password"->JsString("secret-fixture"),"simulation_id"->JsString("test'quoted"))))
      val data=Sources.load(config)
      data.series.map(_.label) shouldBe fixture.field("labels").names
      data.series.zip(fixture.field("series").values).foreach((s,v)=>s.values.toVector shouldBe v.values.map(_.number()))
      query should include("simulation_id = {simulation_id:String}")
      query should include("param_simulation_id=test'quoted")
      query should include("ORDER BY link_id,tick FORMAT JSONEachRow")
      user shouldBe "fixture";password shouldBe "secret-fixture"
    finally {server.stop(0);Files.delete(roadFile)}
  }
  it should "reject conflicting simultaneous occupancy and unknown catalogue links" in {
    val roads=Map("1"->Road("1",100,10,10))
    val window=Window(0,60,60,10)
    intercept[IllegalArgumentException] {Sources.fromEvents(Iterator(FlowEvent("1",10,"a",1),FlowEvent("1",10,"b",2)),window,roads)}
    intercept[IllegalArgumentException] {Sources.fromEvents(Iterator(FlowEvent("2",10,"a",1)),window,roads)}
  }
