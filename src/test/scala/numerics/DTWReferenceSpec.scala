package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.interscity.htc.warping.DTWComputer
import spray.json.*
import Json.*

class DTWReferenceSpec extends AnyFlatSpec with Matchers:
  "Band-stored exact DTW" should "match Python cumulative costs, relaxed ends and paths on unequal lengths and ties" in {
    val cases=scala.io.Source.fromResource("parity/dtw.json").mkString.parseJson.values
    cases.foreach { c=>
      val a=c.field("a").values.map(_.number()).toArray;val b=c.field("b").values.map(_.number()).toArray
      val (path,distance)=DTWComputer.warpingPath(a,b)
      withClue(s"${a.length}x${b.length}: ") {
        distance shouldBe c.field("distance").number() +- 1e-10
        path.size shouldBe c.field("path").values.size
        path.zip(c.field("path").values).foreach { (step,row)=>
          val v=row.values;step.i shouldBe v(0).int();step.j shouldBe v(1).int()
          step.cost shouldBe v(2).number() +- 1e-10;step.descending shouldBe (v(3).int()==1)
        }
      }
    }
  }
