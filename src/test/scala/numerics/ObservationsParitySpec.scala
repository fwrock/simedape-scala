package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.interscity.htc.parity.experiments.*
import spray.json.*
import Json.*
import java.nio.file.Path

class ObservationsParitySpec extends AnyFlatSpec with Matchers:
  def resource(name: String): Path = Path.of(getClass.getResource(s"/parity/$name").toURI)
  "Physical observation aggregation" should "match Python across CSV, Parquet, Parquet directories and JSONL without rounding actor identities" in {
    val fixture=Json.read(resource("observations.json"))
    for source<-Vector("observations.csv","observations.parquet","observations-parts","observations.jsonl") do
      val rows=Observations.read(resource(source))
      rows.exists(_.vehicle=="1152921504606846977") shouldBe true
      fixture.field("cases").values.foreach { c=>
        val got=Observations.aggregate(rows,c.field("config"));val expected=SeriesDataset.load(resource(c.field("file").text()))
        withClue(source+" "+c.field("config").compactPrint+": ") {
          got.labels shouldBe expected.labels;got.variables shouldBe expected.variables;got.runs shouldBe expected.runs;got.groups shouldBe expected.groups;got.units shouldBe expected.units
          got.metadata shouldBe expected.metadata
          for i<-got.values.indices;j<-got.values(i).indices do
            if expected.values(i)(j).isNaN then got.values(i)(j).isNaN shouldBe true
            else got.values(i)(j) shouldBe expected.values(i)(j) +- 1e-9
        }
      }
    fixture.field("fingerprints").asJsObject.fields.foreach((name,hash)=>SeriesDataset.fingerprint(resource(name)) shouldBe hash.text())
  }
  it should "reject duplicate/overlapping samples and incomplete measurement declarations" in {
    val rows=Observations.read(resource("observations.jsonl"))
    val duplicate=rows.find(_.event=="sample").get
    val raw=obj("run_id"->JsString("r"),"vehicle_id"->JsString("v"),"link_id"->JsString("a"),"time_seconds"->JsNumber(2),"event_type"->JsString("sample"),"sample_duration_seconds"->JsNumber(2))
    intercept[IllegalArgumentException] {Observations.validate(Vector(raw,raw))}
    val overlapping=JsObject(raw.fields+("time_seconds"->JsNumber(3)))
    intercept[IllegalArgumentException] {Observations.validate(Vector(raw,overlapping))}
    intercept[IllegalArgumentException] {Observations.aggregate(rows,obj("variables"->strings(Vector("count")),"start_seconds"->JsNumber(0),"end_seconds"->JsNumber(8),"bin_seconds"->JsNumber(2),"window_bins"->JsNumber(2)))}
  }
