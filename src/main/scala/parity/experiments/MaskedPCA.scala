package org.interscity.htc.parity.experiments
import org.tslearn.utils.NativeLinearAlgebra
import org.interscity.htc.parity.*
import Json.*
import spray.json.*

/** Training-only imputation and native SVD, followed by masked ridge least squares. */
final case class MaskedPCA(mean: Array[Double],components: Array[Array[Double]],varianceRatio: Array[Double],ridge: Double):
  def reconstruct(row: Array[Double]): (Array[Double],JsObject) =
    require(row.length==mean.length,"PCA row differs from trained features")
    val observed=row.indices.filter(i=>row(i).isFinite && mean(i).isFinite).toVector
    val result=mean.clone();var rank=0;var condition: JsValue=JsNull
    if components.nonEmpty && observed.nonEmpty then
      val original=observed.map(j=>components.map(_(j))).toArray
      val flat=original.flatten;val count=math.min(original.length,components.length)
      val singular=NativeLinearAlgebra.rightSingularSystem(flat,original.length,components.length,count).values().map(v=>math.sqrt(math.max(0,v)))
      rank=singular.count(_>singular(0)*math.ulp(1.0)*math.max(original.length,components.length))
      condition=if singular.last>1e-12 then num(singular.head/singular.last) else JsNull
      val design=if ridge>0 then original++Array.tabulate(components.length)(i=>Array.tabulate(components.length)(j=>if i==j then math.sqrt(ridge) else 0.0)) else original
      val target=observed.map(j=>row(j)-mean(j)).toArray++(if ridge>0 then new Array[Double](components.length) else Array.empty[Double])
      val weights=NativeLinearAlgebra.leastSquares(design,target)
      for j<-result.indices do
        if mean(j).isFinite then result(j)=mean(j)+components.indices.map(k=>weights(k)*components(k)(j)).sum
    observed.foreach(j=>result(j)=row(j))
    (result,obj("observations"->JsNumber(observed.size),"rank"->JsNumber(rank),"condition"->condition,"components"->JsNumber(components.length)))
object MaskedPCA:
  def mean(rows: Array[Array[Double]]): Array[Double] =
    require(rows.nonEmpty,"Empty data")
    Array.tabulate(rows(0).length) { j=>
      var sum=0.0;var count=0
      rows.foreach { row=>if row(j).isFinite then {sum+=row(j);count+=1} }
      if count==0 then Double.NaN else sum/count
    }
  def fit(rows: Array[Array[Double]],count: Int,ridge: Double): MaskedPCA =
    require(rows.nonEmpty && rows(0).nonEmpty && count>0 && ridge>=0 && ridge.isFinite,"Invalid masked PCA settings")
    val mu=mean(rows);val width=mu.length
    val centered=rows.flatMap(row=>row.indices.map(j=>if row(j).isFinite && mu(j).isFinite then row(j)-mu(j) else 0.0))
    val eig=NativeLinearAlgebra.rightSingularSystem(centered,rows.length,width,math.min(rows.length,width))
    val rank=eig.values().count(v=>math.sqrt(math.max(0,v))>math.sqrt(math.max(0,eig.values()(0)))*1e-10)
    val keep=math.min(count,rank);val total=centered.map(v=>v*v).sum
    MaskedPCA(mu,eig.vectors().take(keep),eig.values().take(keep).map(v=>v/total),ridge)
