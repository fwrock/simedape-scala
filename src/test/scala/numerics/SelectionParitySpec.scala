package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.*
import Json.*

class SelectionParitySpec extends AnyFlatSpec with Matchers:
  "Selection" should "match Python's ten strategies, seeded sampling, forced links and person mappings" in {
    val input=scala.io.Source.fromResource("parity/selection.json").mkString.parseJson
    val vehicles=input.field("vehicles").values.map(_.names)
    val labels=input.field("labels").names
    val clusters=input.field("clusters").values.map(_.int())
    val topology=input.field("topology").asJsObject.fields.map((k,v)=>k->v.names.toSet)
    input.field("cases").values.foreach { c=>
      val mapping=if c.field("mapping")==JsNull then None else Some(c.field("mapping").asJsObject.fields.map((k,v)=>k->v.text()))
      val u=Selection.universe(vehicles,labels,clusters,mapping,if mapping.nonEmpty then "person" else "vehicle")
      val result=Selection.manifest(u,"fixture",c.field("params"),c.field("sp").values.map(_.int()),input.field("distances").values.map(_.number()),topology)
      withClue(c.field("params").compactPrint+" "+mapping+": ") { result shouldBe c.field("expected") }
    }
  }
