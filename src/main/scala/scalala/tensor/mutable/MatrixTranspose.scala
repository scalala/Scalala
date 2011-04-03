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

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * A Transpose of any Matrix type is a Matrix.
 *
 * @author dramage
 */
trait MatrixTransposeLike
[@specialized(Int,Long,Float,Double) V, +Coll <: Matrix[V], +This <: MatrixTranspose[V,Coll]]
extends tensor.MatrixTransposeLike[V,Coll,This]
with Tensor2TransposeLike[Int,Int,V,IndexDomain,IndexDomain,TableDomain,TableDomain,Coll,This]
with MatrixLike[V,This] {
  override def domain = underlying.domain.transpose.asInstanceOf[TableDomain];
  
  override def t : Coll =
    underlying;
}

/**
 * A Transpose of any Matrix type is a Matrix.
 *
 * @author dramage
 */
trait MatrixTranspose[@specialized(Int,Long,Float,Double) V, +Coll <: Matrix[V]]
extends tensor.MatrixTranspose[V,Coll]
with Tensor2Transpose[Int,Int,V,Coll]
with Matrix[V] with MatrixTransposeLike[V, Coll, MatrixTranspose[V, Coll]];

object MatrixTranspose {
  class Impl[V, +Coll <: Matrix[V]]
  (override val underlying : Coll)
  (implicit override val scalar : Scalar[V])
  extends MatrixTranspose[V,Coll];

  implicit def canMapValues[V,RV](implicit c : CanMapValues[Matrix[V],V,RV,Matrix[RV]]) =
    c.asInstanceOf[CanMapValues[MatrixTranspose[V,Matrix[V]],V,RV,Matrix[RV]]];

  implicit def canKeyValuePairs[V,RV](implicit c : CanMapKeyValuePairs[Matrix[V],(Int,Int),V,RV,Matrix[RV]]) =
    c.asInstanceOf[CanMapKeyValuePairs[MatrixTranspose[V,Matrix[V]],(Int,Int),V,RV,Matrix[RV]]];

  implicit def canJoinValues[V1,V2,RV](implicit c : CanJoinValues[Matrix[V1],Tensor[(Int,Int),V2],V1,V2,RV,Matrix[RV]]) =
    c.asInstanceOf[CanJoinValues[MatrixTranspose[V1,Matrix[V1]],Tensor[(Int,Int),V2],V1,V2,RV,Matrix[RV]]];

  implicit def binaryOp[V,RV,Op<:OpType,That](implicit op : BinaryOp[Matrix[V],RV,Op,That]) =
    op.asInstanceOf[BinaryOp[MatrixTranspose[V,Matrix[V]],RV,Op,That]];

  implicit def binaryUpdateOp[V,RV,Op<:OpType](implicit op : BinaryUpdateOp[Matrix[V],RV,Op]) =
    op.asInstanceOf[BinaryUpdateOp[MatrixTranspose[V,Matrix[V]],RV,Op]];
}

