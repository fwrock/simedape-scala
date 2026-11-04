package org.interscity.htc.parity

import java.nio.file.{Files,Path,StandardCopyOption}
import java.io.BufferedWriter
import spray.json.*
import Json.*
import scala.util.Using
import scala.jdk.CollectionConverters.*

/** Column spools permit JSON-compatible results without retaining millions of boxed numbers. */
final class MetricSink(destination: Path,roads: Map[String,Road]) extends AutoCloseable:
  Files.createDirectories(destination.toAbsolutePath.getParent)
  private val scratch=Files.createTempDirectory(destination.toAbsolutePath.getParent,".metrics-")
  private val columns=Metrics.keys:+"labels"
  private val outputs=columns.map(k=>k->Files.newBufferedWriter(scratch.resolve(k))).toMap
  private var count=0
  def append(label: String,values: Array[Double]): Unit =
    val link=label.replaceFirst("-[0-9]+-[0-9]+$", "")
    val calculated=Metrics.calculate(values,roads.getOrElse(link,throw IllegalArgumentException(s"Missing road metadata: $link")))
    Metrics.keys.foreach { key =>
      if count>0 then outputs(key).write(',')
      outputs(key).write(num(calculated(key)).compactPrint)
    }
    if count>0 then outputs("labels").write(',')
    outputs("labels").write(JsString(label).compactPrint);count+=1
  def finish(missing: Missing,includeSP: Boolean,includeSeries: Boolean=true,scalars: JsObject=Json.empty): Int =
    outputs.values.foreach(_.close())
    val temporary=scratch.resolve("result.json")
    Using.resource(Files.newBufferedWriter(temporary)) { writer =>
      writer.write('{')
      columns.zipWithIndex.foreach { (key,index) =>
        if index>0 then writer.write(',')
        writer.write(JsString(key).compactPrint);writer.write(":[")
        Using.resource(Files.newBufferedReader(scratch.resolve(key))) { reader =>
          val chars=new Array[Char](65536);var n=reader.read(chars)
          while n>=0 do
            if n>0 then writer.write(chars,0,n)
            n=reader.read(chars)
        }
        writer.write(']')
      }
      if includeSeries then writer.write(s",\"missing_time_series\":${missing.series}")
      if includeSP then writer.write(s",\"missing_simulation_points\":${missing.simulationPoints}")
      scalars.fields.toVector.sortBy(_._1).foreach((key,value)=>writer.write(","+JsString(key).compactPrint+":"+value.compactPrint))
      writer.write('}')
    }
    Files.move(temporary,destination.toAbsolutePath,StandardCopyOption.REPLACE_EXISTING)
    count
  def close(): Unit =
    outputs.values.foreach(_.close())
    if Files.exists(scratch) then
      Using.resource(Files.list(scratch))(_.iterator().asScala.foreach(Files.deleteIfExists(_)))
      Files.deleteIfExists(scratch)
