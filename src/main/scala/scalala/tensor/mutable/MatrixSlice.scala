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
package mutable;

import domain._;

import scalala.generic.{CanAdd,CanSub,CanMul,CanDiv,CanPow,CanMod};
import scalala.generic.{CanAssignInto, CanAddInto, CanSubInto, CanMulInto, CanDivInto, CanPowInto, CanModInto};
import scalala.generic.collection._;

import scalar.Scalar;

/**
 * Implementation trait for a pass-through view of a (key-mapped)
 * MutableDomainTable from an underlying Tensor2.
 *
 * @author dramage
 */
trait MatrixSliceLike
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +Coll<:Tensor2[A1,A2,B],
 +This<:MatrixSlice[A1,A2,B,Coll]]
extends tensor.MatrixSliceLike[A1,A2,B,D1,D2,D,T,Coll,This]
with MatrixLike[B,This] {

  override def update(i : Int, j : Int, value : B) : Unit =
    underlying.update(lookup1(i),lookup2(j),value);
}

/**
 * A pass-through view of a (key-mapped)
 * MutableDomainTable from an underlying Tensor2.
 *
 * @author dramage
 */
trait MatrixSlice
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor2[A1,A2,B]]
extends tensor.MatrixSlice[A1,A2,B,Coll]
with Matrix[B]
with MatrixSliceLike[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Coll,MatrixSlice[A1,A2,B,Coll]];

object MatrixSlice {
  class FromKeySeqs[A1, A2, B, +Coll<:Tensor2[A1,A2,B]]
  (override val underlying : Coll, val keys1 : Seq[A1], val keys2 : Seq[A2])
  (override implicit val scalar : Scalar[B])
  extends MatrixSlice[A1,A2,B,Coll] {
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

  implicit def canAssignInto[K1,K2,V](implicit c : CanAssignInto[Matrix[V],V]) =
    c.asInstanceOf[CanAssignInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];

  implicit def canAddInto[K1,K2,V](implicit c : CanAddInto[Matrix[V],V]) =
    c.asInstanceOf[CanAddInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];

  implicit def canSubInto[K1,K2,V](implicit c : CanSubInto[Matrix[V],V]) =
    c.asInstanceOf[CanSubInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];

  implicit def canMulInto[K1,K2,V](implicit c : CanMulInto[Matrix[V],V]) =
    c.asInstanceOf[CanMulInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];

  implicit def canDivInto[K1,K2,V](implicit c : CanDivInto[Matrix[V],V]) =
    c.asInstanceOf[CanDivInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];

  implicit def canModInto[K1,K2,V](implicit c : CanModInto[Matrix[V],V]) =
    c.asInstanceOf[CanModInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];

  implicit def canPowInto[K1,K2,V](implicit c : CanPowInto[Matrix[V],V]) =
    c.asInstanceOf[CanPowInto[MatrixSlice[K1,K2,V,Tensor2[K1,K2,V]],V]];
}
