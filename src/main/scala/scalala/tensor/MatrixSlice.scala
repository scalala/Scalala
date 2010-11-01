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

import generic.{CanAdd,CanSub,CanMul,CanDiv,CanPow,CanMod};
import generic.collection._;
import scalar.Scalar;

/**
 * A table sliced from a Tensor2 by providing a set product set of
 * keys from A1 x A2.
 *
 * @author dramage
 */
trait MatrixSliceLike
[@specialized(Int) A1, @specialized(Int) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +Coll<:Tensor2[A1,A2,B],
 +This<:MatrixSlice[A1,A2,B,Coll]]
extends TensorSliceLike[(A1,A2),D,(Int,Int),TableDomain,B,Coll,This]
with MatrixLike[B,This] {

  def lookup1(i : Int) : A1;
  def lookup2(j : Int) : A2;

  /* final */ override def lookup(tup : (Int,Int)) =
    (lookup1(tup._1), lookup2(tup._2));

  override def apply(i : Int, j : Int) : B =
    underlying.apply(lookup1(i), lookup2(j));
}

/**
 * A table sliced from a Tensor2 by providing a set product set of
 * keys from A1 x A2.
 *
 * @author dramage
 */
trait MatrixSlice
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor2[A1,A2,B]]
extends TensorSlice[(A1,A2),(Int,Int),B,Coll]
with Matrix[B]
with MatrixSliceLike[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Coll,MatrixSlice[A1,A2,B,Coll]];


object MatrixSlice {
  class FromKeySeqs[A1, A2, B, +Coll<:Tensor2[A1,A2,B]]
  (override val underlying : Coll, val keys1 : Seq[A1], val keys2 : Seq[A2])
  (implicit override val scalar : Scalar[B])
  extends MatrixSlice[A1, A2, B, Coll] {
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }

  implicit def canMapValues[K1,K2,V,RV](implicit c : CanMapValues[Matrix[V],V,RV,Matrix[RV]]) =
    c.asInstanceOf[CanMapValues[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V,RV,Matrix[RV]]];

  implicit def canKeyValuePairs[K1,K2,V,RV](implicit c : CanMapKeyValuePairs[Matrix[V],(Int,Int),V,RV,Matrix[RV]]) =
    c.asInstanceOf[CanMapKeyValuePairs[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],(Int,Int),V,RV,Matrix[RV]]];

  implicit def canJoinValues[K1,K2,V1,V2,RV](implicit c : CanJoinValues[Matrix[V1],Tensor[(Int,Int),V2],V1,V2,RV,Matrix[RV]]) =
    c.asInstanceOf[CanJoinValues[MatrixSlice[K1,K2,V1,Tensor2[K1,K2,V1]],Tensor[(Int,Int),V2],V1,V2,RV,Matrix[RV]]];

  implicit def canAdd[K1,K2,V,B,That](implicit c : CanAdd[Matrix[V],B,That]) =
    c.asInstanceOf[CanAdd[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],B,That]];

  implicit def canSub[K1,K2,V,B,That](implicit c : CanSub[Matrix[V],B,That]) =
    c.asInstanceOf[CanSub[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],B,That]];

  implicit def canMul[K1,K2,V,B,That](implicit c : CanMul[Matrix[V],B,That]) =
    c.asInstanceOf[CanMul[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],B,That]];

  implicit def canDiv[K1,K2,V,B,That](implicit c : CanDiv[Matrix[V],B,That]) =
    c.asInstanceOf[CanDiv[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],B,That]];

  implicit def canMod[K1,K2,V,B,That](implicit c : CanMod[Matrix[V],B,That]) =
    c.asInstanceOf[CanMod[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],B,That]];

  implicit def canPow[K1,K2,V,B,That](implicit c : CanPow[Matrix[V],B,That]) =
    c.asInstanceOf[CanPow[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],B,That]];
}
