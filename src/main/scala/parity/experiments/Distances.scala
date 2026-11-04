package org.interscity.htc.parity.experiments

/** Physical-time multivariable distances; missing pairs never count as measurements. */
object Distances:
  def dtw(a: Array[Double],b: Array[Double],columns: Int,window: Option[Int],weights: Array[Double]): (Double,Vector[(Int,Int)]) =
    require(columns>0 && a.length%columns==0 && b.length%columns==0 && weights.length==columns && weights.forall(w=>w.isFinite && w>0),"Invalid DTW axes/weights")
    val n=a.length/columns;val m=b.length/columns
    require(n>0 && m>0,"DTW requires nonempty series")
    val radius=window.getOrElse(math.max(n,m));require(radius>=math.abs(n-m) && radius>=0,"DTW window cannot align these lengths")
    // Only traceback directions are retained; cumulative distances use two rolling rows.
    val backs=Array.tabulate(n)(i=>new Array[Byte](math.max(0,math.min(m,i+radius+1)-math.max(0,i-radius))))
    var previous=Array.fill(m+1)(Double.PositiveInfinity);previous(0)=0
    for i<-0 until n do
      val current=Array.fill(m+1)(Double.PositiveInfinity)
      val first=math.max(0,i-radius)
      for j<-first until math.min(m,i+radius+1) do
        var cost=0.0;var support=0.0
        for p<-0 until columns do
          val av=a(i*columns+p);val bv=b(j*columns+p)
          if av.isFinite && bv.isFinite then {cost+=weights(p)*math.pow(av-bv,2);support+=weights(p)}
        val local=if support>0 then cost/support else 1.0
        val choices=Array(previous(j),previous(j+1),current(j))
        val step=choices.indices.minBy(choices(_));backs(i)(j-first)=step.toByte
        current(j+1)=local+choices(step)
      previous=current
    if !previous(m).isFinite then (Double.PositiveInfinity,Vector.empty)
    else
      var i=n-1;var j=m-1;var support=0;val path=Vector.newBuilder[(Int,Int)]
      while i>=0 && j>=0 do
        path+=((i,j))
        if (0 until columns).exists(p=>a(i*columns+p).isFinite && b(j*columns+p).isFinite) then support+=1
        val step=backs(i)(j-math.max(0,i-radius))
        if step==0 then {i-=1;j-=1} else if step==1 then i-=1 else j-=1
      if support==0 then (Double.PositiveInfinity,Vector.empty) else (math.sqrt(previous(m)/support),path.result().reverse)
  def distance(a: Array[Double],b: Array[Double],columns: Int,warping: String,window: Option[Int],weights: Array[Double]): Double = warping match
    case "euclidean"=>
      require(a.length==b.length,"Euclidean distance axes differ")
      var cost=0.0;var support=0.0
      for j<-a.indices do if a(j).isFinite && b(j).isFinite then {cost+=weights(j%columns)*math.pow(a(j)-b(j),2);support+=weights(j%columns)}
      if support>0 then math.sqrt(cost/support) else Double.PositiveInfinity
    case "dependent"=>dtw(a,b,columns,window,weights)._1
    case "independent"=>
      val distances=Array.tabulate(columns)(p=>dtw(a.indices.by(columns).map(j=>a(j+p)).toArray,b.indices.by(columns).map(j=>b(j+p)).toArray,1,window,Array(1.0))._1)
      val valid=distances.indices.filter(distances(_).isFinite)
      if valid.isEmpty then Double.PositiveInfinity else math.sqrt(valid.map(p=>weights(p)*math.pow(distances(p),2)).sum/valid.map(weights).sum)
    case _=>throw IllegalArgumentException("Unknown warping")
