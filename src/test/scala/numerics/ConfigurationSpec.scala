package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.*
import Json.*
import java.nio.file.Files

class ConfigurationSpec extends AnyFlatSpec with Matchers:
  "Configuration" should "load safe YAML stage parameters and retain legacy properties keys" in {
    val yaml=Files.createTempFile("simedape-",".yml")
    Files.writeString(yaml,"""config:
  scenario_name: fixture
  clustering:
    n_clusters: 2
stages:
  - data_load
  - name: estimation_variants
    params:
      variants: [avg, pca_sp]
      workers: 2
""")
    val document=Configuration.load(yaml)
    val cfg=Configuration.normalize(document.field("config").asJsObject)
    cfg.field("clustering").field("n_clusters").int() shouldBe 2
    cfg.field("clustering").field("max_iter").int() shouldBe 8
    cfg.field("output_dir").text() shouldBe "output/fixture/"
    document.field("stages").values(1).field("params").field("variants").names shouldBe Vector("avg","pca_sp")
    val properties=Files.createTempFile("simedape-",".properties")
    Files.writeString(properties,"scenario.name=legacy\nscneario.input.dir=portable\nclustering.n_clusters=3\nclickhouse.port=8124\nclickhouse.secure=True\n")
    val legacy=Configuration.normalize(Configuration.load(properties).field("config").asJsObject)
    legacy.field("input_dir").text() shouldBe "portable"
    legacy.field("clickhouse").field("port").int() shouldBe 8124
    legacy.field("clickhouse").field("secure").bool() shouldBe true
    Files.delete(yaml);Files.delete(properties)
  }
  it should "reject unsupported stages and misspelled parameters before starting work" in {
    intercept[IllegalArgumentException] {Pipeline.validate(Vector(StageRequest("clustering",obj("cluster_count"->JsNumber(5)))))}
    intercept[IllegalArgumentException] {Pipeline.validate(Vector(StageRequest("unknown")))}
    intercept[IllegalArgumentException] {RunConfig(obj("csv_sep"->JsString("")))}
  }
