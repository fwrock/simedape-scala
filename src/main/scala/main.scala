package org.interscity.htc

@main
def main(args: String*): Unit = args.toList match
  case Nil => core.SimEDaPE.start()
  case "serve" :: Nil => core.SimEDaPE.start()
  case "run" :: file :: stages =>
    val result=parity.Configuration.run(java.nio.file.Path.of(file),stages.toVector)
    println(result.prettyPrint)
  case _ => throw IllegalArgumentException("Usage: simedape [serve | run pipeline.yml [stage ...]]")
