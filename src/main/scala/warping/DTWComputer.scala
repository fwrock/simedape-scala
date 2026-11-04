package org.interscity.htc
package warping

import data.Models.*
import org.tslearn.metrics.ConstrainedDTW

/** Exact constrained DTW from the shared Java library; band storage and psi=2 match Python. */
object DTWComputer:
  def warpingPath(s1: Array[Double],s2: Array[Double],windowFraction: Double=0.10): (WarpingPath,Double) =
    val path=ConstrainedDTW.compute(s1,s2,windowFraction)
    val steps=path.i().indices.iterator.map(i=>WarpingStep(path.i()(i),path.j()(i),path.cost()(i),path.descending()(i))).toList
    (steps,path.distance())
