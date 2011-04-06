/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package tensor;

import domain._;
import generic.{TensorBuilder,TensorMonadic};

import scalala.scalar.Scalar;
import scalala.generic.collection._
import tensor.Counter2.Curried
;

/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dlwh
 */
trait Counter2Like
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V,
 +M1[VV] <: Curried[scala.collection.Map,K1]#Result[VV],
 +M2 <: scala.collection.Map[K2,V],
 +This<:Counter2[K1,K2,V]]
extends Tensor2Like[K1,K2,V,SetDomain[K1],SetDomain[K2],Domain2[K1,K2],Domain2[K2,K1],This]
//with TensorNomadic[(K1,K2),V,This]
{ self =>

  override def newBuilder[NK,NV:Scalar](domain : IterableDomain[NK])
  : TensorBuilder[NK,NV,Tensor[NK,NV]] = domain match {
    case that : IndexDomain =>
      super.newBuilder(that)
    case that : Domain1[_] =>
      mutable.Counter(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : Domain2[_,_] =>
      mutable.Counter2(that)(implicitly[Scalar[NV]]).asBuilder;
    case _ =>
      super.newBuilder(domain);
  }

  def data : M1[_<:M2];
  
  override def domain : Domain2[K1,K2] = {
    new Domain2[K1,K2] {
      def _1 = new SetDomain(data.keySet)
      def _2 = new SetDomain(data.values.flatMap(_.keys).toSet);
    }
  }
  
  override def apply(k : K1, k2: K2) = data.get(k).flatMap(_.get(k2)) getOrElse scalar.zero;

  override def checkKey(k : K1, k2: K2) = ();
  
  override def checkDomain(d : scalala.tensor.domain.Domain[(K1,K2)]) = ();


  //
  // non-tupled monadic
  //

//  override def iterator =
//    repr.pairsIterator;

  override def foreach[U](fn : (K1,K2,V)=>U) : Unit =
    foreachPair( (k:(K1,K2),v:V) => fn(k._1,k._2,v));

  override def foreachNonZero[U](fn : (K1,K2,V)=>U) : Unit =
    foreachNonZeroPair((kk:(K1,K2),v:V)=>fn(kk._1,kk._2,v));
  
  override def map[TT>:This,RV,That](fn : (K1,K2,V)=>RV)
  (implicit bf : CanMapKeyValuePairs[TT, (K1,K2), V, RV, That]) : That =
    mapPairs[TT,RV,That]( (kk:(K1,K2),v:V)=>fn(kk._1,kk._2,v))(bf);
    
  //
  // faster implementations
  //

  override def foreachKey[U](fn : ((K1,K2)) => U) : Unit =
    for((k1,m) <- data; k2 <- m.keys) fn(k1->k2);

  override def foreachValue[U](fn : V => U) : Unit =
    valuesIterator.foreach(fn);

  override def keysIterator = for( (k1,m) <- data.iterator; k2 <- m.keysIterator) yield (k1,k2);

  override def valuesIterator = for(m <- data.valuesIterator; v <- m.valuesIterator) yield v

  override def pairsIterator = for( (k1,m) <- data.iterator; (k2,v) <- m.iterator) yield (k1,k2)->v;
}

trait Counter2
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V]
extends Tensor2[K1,K2,V] with Counter2Like[K1,K2,V,Curried[scala.collection.Map,K1]#Result,scala.collection.Map[K2,V],Counter2[K1,K2,V]];

object Counter2 {
  def apply[K1,K2,V:Scalar](values : (K1,K2,V)*) : mutable.Counter2[K1,K2,V] =
    mutable.Counter2(values :_ *);
    
  def apply[K1,K2,V:Scalar](domain : Domain2[K1,K2]) =
    mutable.Counter2(domain);


  /**
   * This is just a curried version of scala.collection.Map.
   * Used to get around Scala's lack of partially applied types.
   *
   * @author dlwh
   */
  trait Curried[M[_,_],K] {
    type Result[V] = M[K,V];
  }


  implicit def canSliceRow[K1,K2,V:Scalar] : CanSliceRow[Counter2[K1,K2,V],K1,Counter[K2,V]]
  = new CanSliceRow[Counter2[K1,K2,V],K1,Counter[K2,V]] {
    val vS = implicitly[Scalar[V]];
    override def apply(from : Counter2[K1,K2,V], row : K1) = new Counter[K2,V] {
      def data = from.data(row);
      implicit val scalar = vS;
    }
  }


}

