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
import generic.{TensorBuilder,TensorPairsMonadic};

import scalala.scalar.Scalar;
import scalala.generic.collection._
import tensor.Counter2.Curried

/**
 * A Tensor2 with an open row key domain that maps to an arbitrary Tensor1 as its rows.
 *
 * CRS = Compress Row Storage
 *
 * @author dlwh
 */
trait CRSTensor2Like
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V,
 +M1[VV] <: Curried[scala.collection.Map,K1]#Result[VV],
 +T <: Tensor1[K2,V],
 +This<:CRSTensor2[K1,K2,V,T]]
extends Tensor2Like[K1,K2,V,SetDomain[K1],SetDomain[K2],Domain2[K1,K2],Domain2[K2,K1],This]
{ self =>

  protected def k2domain:Domain1[K2];

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

  def data : M1[_<:T];


  def apply(i: K1, j: K2) = data(i)(j);

  override def size = {
    var s = 0;
    for (m <- data.valuesIterator) {
      s += m.size
    }
    s;
  }

  override def domain : Domain2[K1,K2] = {
    new Domain2[K1,K2] {
      def _1 = new SetDomain(data.keySet)
      def _2 = k2domain;
    }
  }


  override def checkKey(k : K1, k2: K2) = data(k).checkKey(k2);

  // TODO: how to implement this nicely?
  override def checkDomain(d : scalala.tensor.domain.Domain[(K1,K2)]) = ();

    
  //
  // faster implementations
  //

  override def foreachKey[U](fn : ((K1,K2)) => U) : Unit =
    for((k1,m) <- data; k2 <- m.keys) fn(k1->k2);

  override def foreachValue[U](fn : V => U) : Unit =
    valuesIterator.foreach(fn);
    
  override def foreachTriple[U](fn : (K1,K2,V) => U) : Unit =
    triplesIterator.foreach(triple => fn(triple._1,triple._2,triple._3));

  override def keysIterator = for ((k1,m) <- data.iterator; k2 <- m.keysIterator) yield (k1,k2);

  override def valuesIterator = for (m <- data.valuesIterator; v <- m.valuesIterator) yield v

  override def pairsIterator = for ((k1,m) <- data.iterator; (k2,v) <- m.pairsIterator) yield (k1,k2)->v;
  
  override def triplesIterator = for ((k1,m) <- data.iterator; (k2,v) <- m.pairsIterator) yield (k1,k2,v);
}
/**
 * A Tensor2 with an open row key domain that maps to an arbitrary Tensor1 as its rows.
 *
 * CRS = Compress Row Storage
 *
 * @author dlwh
 */
trait CRSTensor2
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V, +T<:Tensor1[K2,V]]
extends Tensor2[K1,K2,V] with CRSTensor2Like[K1,K2,V,Curried[scala.collection.Map,K1]#Result,T,CRSTensor2[K1,K2,V,T]];

object CRSTensor2 {
  /**
   * Returns a new empty CRSTensor2 using the "template" to create rows. The actual rows are zero'd out versions of
   * the template.
   */
  def apply[K1,K2,V,T<:mutable.Tensor1[K2,V]](template: T)(implicit view: CanViewAsTensor1[T,K2,V],
                                                     zeros: CanCreateZerosLike[T,T],
                                                     scalar: Scalar[V]) : mutable.CRSTensor2[K1,K2,V,T] =
    mutable.CRSTensor2[K1,K2,V,T](template);
    
  implicit def canSliceRow[K1,K2,V,T<:Tensor1[K2,V]] : CanSliceRow[CRSTensor2[K1,K2,V,T],K1,T]
  = new CanSliceRow[CRSTensor2[K1,K2,V,T],K1,T] {
    override def apply(from : CRSTensor2[K1,K2,V,T], row : K1) = from.data(row);
  }


}

