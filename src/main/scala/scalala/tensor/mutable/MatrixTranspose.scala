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

import domain.{IndexDomain,TableDomain};

import scalala.generic.{CanAdd,CanSub,CanMul,CanDiv,CanPow,CanMod};
import scalala.generic.{CanAssignInto, CanAddInto, CanSubInto, CanMulInto, CanDivInto, CanPowInto, CanModInto};
import scalala.generic.collection._;

import scalar.Scalar;

/**
 * A Transpose of any Matrix type is a Matrix.
 *
 * @author dramage
 */
trait MatrixTransposeLike
[@specialized(Int,Long,Float,Double) B, +Coll <: Matrix[B], +This <: MatrixTranspose[B,Coll]]
extends tensor.MatrixTransposeLike[B,Coll,This]
with Tensor2TransposeLike[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,Coll,This]
with MatrixLike[B,This] {
  override def domain = underlying.domain.transpose.asInstanceOf[TableDomain];
}

/**
 * A Transpose of any Matrix type is a Matrix.
 *
 * @author dramage
 */
trait MatrixTranspose[@specialized(Int,Long,Float,Double) B, +Coll <: Matrix[B]]
extends tensor.MatrixTranspose[B,Coll]
with Tensor2Transpose[Int,Int,B,Coll]
with Matrix[B] with MatrixTransposeLike[B, Coll, MatrixTranspose[B, Coll]];

object MatrixTranspose {
  class Impl[B, +Coll <: Matrix[B]]
  (override val underlying : Coll)
  (implicit override val scalar : Scalar[B])
  extends MatrixTranspose[B,Coll];

  implicit def canMapValues[V,RV](implicit c : CanMapValues[Matrix[V],V,RV,Matrix[RV]]) =
    c.asInstanceOf[CanMapValues[MatrixTranspose[V,Matrix[V]],V,RV,Matrix[RV]]];

  implicit def canKeyValuePairs[V,RV](implicit c : CanMapKeyValuePairs[Matrix[V],(Int,Int),V,RV,Matrix[RV]]) =
    c.asInstanceOf[CanMapKeyValuePairs[MatrixTranspose[V,Matrix[V]],(Int,Int),V,RV,Matrix[RV]]];

  implicit def canJoinValues[V1,V2,RV](implicit c : CanJoinValues[Matrix[V1],Tensor[(Int,Int),V2],V1,V2,RV,Matrix[RV]]) =
    c.asInstanceOf[CanJoinValues[MatrixTranspose[V1,Matrix[V1]],Tensor[(Int,Int),V2],V1,V2,RV,Matrix[RV]]];

  implicit def canAdd[K1,K2,V,B,That](implicit c : CanAdd[Matrix[V],B,That]) =
    c.asInstanceOf[CanAdd[MatrixTranspose[V,Matrix[V]],B,That]];

  implicit def canSub[K1,K2,V,B,That](implicit c : CanSub[Matrix[V],B,That]) =
    c.asInstanceOf[CanSub[MatrixTranspose[V,Matrix[V]],B,That]];

  implicit def canMul[K1,K2,V,B,That](implicit c : CanMul[Matrix[V],B,That]) =
    c.asInstanceOf[CanMul[MatrixTranspose[V,Matrix[V]],B,That]];

  implicit def canDiv[K1,K2,V,B,That](implicit c : CanDiv[Matrix[V],B,That]) =
    c.asInstanceOf[CanDiv[MatrixTranspose[V,Matrix[V]],B,That]];

  implicit def canMod[K1,K2,V,B,That](implicit c : CanMod[Matrix[V],B,That]) =
    c.asInstanceOf[CanMod[MatrixTranspose[V,Matrix[V]],B,That]];

  implicit def canPow[K1,K2,V,B,That](implicit c : CanPow[Matrix[V],B,That]) =
    c.asInstanceOf[CanPow[MatrixTranspose[V,Matrix[V]],B,That]];

  implicit def canAssignInto[K1,K2,V](implicit c : CanAssignInto[Matrix[V],V]) =
    c.asInstanceOf[CanAssignInto[MatrixTranspose[V,Matrix[V]],V]];

  implicit def canAddInto[K1,K2,V](implicit c : CanAddInto[Matrix[V],V]) =
    c.asInstanceOf[CanAddInto[MatrixTranspose[V,Matrix[V]],V]];

  implicit def canSubInto[K1,K2,V](implicit c : CanSubInto[Matrix[V],V]) =
    c.asInstanceOf[CanSubInto[MatrixTranspose[V,Matrix[V]],V]];

  implicit def canMulInto[K1,K2,V](implicit c : CanMulInto[Matrix[V],V]) =
    c.asInstanceOf[CanMulInto[MatrixTranspose[V,Matrix[V]],V]];

  implicit def canDivInto[K1,K2,V](implicit c : CanDivInto[Matrix[V],V]) =
    c.asInstanceOf[CanDivInto[MatrixTranspose[V,Matrix[V]],V]];

  implicit def canModInto[K1,K2,V](implicit c : CanModInto[Matrix[V],V]) =
    c.asInstanceOf[CanModInto[MatrixTranspose[V,Matrix[V]],V]];

  implicit def canPowInto[K1,K2,V](implicit c : CanPowInto[Matrix[V],V]) =
    c.asInstanceOf[CanPowInto[MatrixTranspose[V,Matrix[V]],V]];
}
