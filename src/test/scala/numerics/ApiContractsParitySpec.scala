package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.*
import Json.*
import java.nio.file.Files

class ApiContractsParitySpec extends AnyFlatSpec with Matchers:
  def close(got: JsValue,expected: JsValue,path: String=""): Unit = (got,expected) match
    case (JsNumber(a),JsNumber(b))=>withClue(path+": "){a.toDouble shouldBe b.toDouble +- 1e-7*math.max(1,math.abs(b.toDouble))}
    case (a: JsObject,b: JsObject)=>
      withClue(path){a.fields.keySet shouldBe b.fields.keySet};b.fields.foreach((k,v)=>close(a.fields(k),v,s"$path/$k"))
    case (JsArray(a),JsArray(b))=>withClue(path){a.size shouldBe b.size};a.zip(b).zipWithIndex.foreach((v,i)=>close(v._1,v._2,s"$path/$i"))
    case _=>withClue(path+": "){got shouldBe expected}
  "Artifact and chart APIs" should "match Python response contracts, including all eleven chart types and pagination" in {
    val fixture=scala.io.Source.fromResource("parity/api-contracts.json").mkString.parseJson
    val root=Files.createTempDirectory("simedape-contracts-")
    fixture.field("files").asJsObject.fields.foreach((key,value)=>Json.write(root.resolve(key+".json"),value))
    val expected=fixture.field("expected")
    close(Exports.estimation(root,"result"),expected.field("estimation"),"estimation")
    close(Exports.detail(root,"result","avg",Some("average_speed"),1,Some(2)),expected.field("detail"),"detail")
    close(Exports.comparison(root,"compare"),expected.field("comparison"),"comparison")
    close(Charts.catalog(root),expected.field("catalog"),"catalog")
    close(Charts.sweep(root),expected.field("sweep"),"sweep")
    Charts.specs.foreach(s=>close(Charts.build(root,s.id),expected.field("charts").field(s.id),s.id))
  }
