package org.interscity.htc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parity.*
import parity.Json.*
import spray.json.*
import scala.util.Using

class PythonSourcesMetricsSpec extends AnyFlatSpec with Matchers:
  def fixture(name: String): JsValue = Using.resource(getClass.getResourceAsStream(s"/parity/$name.json"))(stream=>new String(stream.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson)
  def same(got: JsValue,expected: JsValue): Unit = (got,expected) match
    case (JsNumber(a),JsNumber(b)) => a.toDouble shouldBe b.toDouble +- (1e-10*math.max(1,math.abs(b.toDouble)))
    case (a: JsObject,b: JsObject) =>
      a.fields.keySet shouldBe b.fields.keySet
      b.fields.foreach((k,v)=>withClue(k+": "){same(a.fields(k),v)})
    case (JsArray(a),JsArray(b)) => a.length shouldBe b.length;a.zip(b).foreach((x,y)=>same(x,y))
    case _ => got shouldBe expected
  "Regular occupancy" should "match Python values, catalogue coverage, boundaries and real vehicle membership" in {
    val f=fixture("occupancy")
    val roads=f.field("roads").asJsObject.fields.map { (id,value) => val v=value.values;id->Road(id,v(0).number(),v(1).number(),v(2).number()) }
    val events=f.field("events").values.map(e=>FlowEvent(e.field("link_id").text(),e.field("tick").number(),e.field("vehicle_id").text(),e.field("vehicle_count_on_link").number()))
    val data=Sources.fromEvents(events.iterator,Window(0,120,60,10),roads)
    same(strings(data.series.map(_.label)),f.field("labels"))
    same(JsArray(data.series.map(s=>arr(s.values.toVector))),f.field("series"))
    same(JsArray(data.series.map(s=>strings(s.vehicles))),f.field("members"))
    data.series.zip(f.field("metrics").values).foreach { (s,expected) => same(JsObject(Metrics.calculate(s.values,roads(s.link)).view.mapValues(num).toMap),expected) }
  }
  "Metric comparison" should "match Python for missing labels, zeros and all available metrics" in {
    val f=fixture("comparison")
    same(Metrics.compare(f.field("real"),f.field("estimated"),"fixture"),f.field("expected"))
  }
