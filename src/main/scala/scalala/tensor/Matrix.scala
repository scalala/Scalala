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

import generic.Scalar;
import collection._;
import collection.domain._;
import collection.generic._;

/**
 * Implementation trait for a matrix.
 *
 * @author dramage
 */
trait MatrixLike[@specialized(Int,Long,Float,Double) B, +This<:Matrix[B]]
extends MutableDomainTableLike[B,This]
with Tensor2Like[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,This];

trait Matrix[@specialized(Int,Long,Float,Double) B]
extends MutableDomainTable[B]
with Tensor2[Int,Int,B]
with MatrixLike[B,Matrix[B]];

object Matrix {
  /** A Transpose of any Matrix type is a Matrix. */
  trait TransposeLike
  [@specialized(Int,Long,Float,Double) B,
   +Coll <: Matrix[B], +This <: Transpose[B,Coll]]
  extends Tensor2.TransposeLike[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,Coll,This]
  with MatrixLike[B,This] {
    override def domain = underlying.domain.transpose.asInstanceOf[TableDomain];
  }

  trait Transpose
  [@specialized(Int,Long,Float,Double) B,
   +Coll <: Matrix[B]]
  extends Tensor2.Transpose[Int,Int,B,Coll]
  with Matrix[B] with TransposeLike[B, Coll, Transpose[B, Coll]];

  /** Default implementation. */
  class TransposeImpl[B, +Coll <: Matrix[B]]
  (override val underlying : Coll)
  (implicit override val scalar : Scalar[B])
  extends Transpose[B,Coll];

  /** A SliceTable of any Double-valued MutableDomainMap is a Matrix. */
  trait SliceTableLike
  [@specialized(Int,Long) A1, @specialized(Int,Long) A2,
   @specialized(Int,Long,Float,Double) B,
   +D1<:IterableDomain[A1] with DomainLike[A1,D1],
   +D2<:IterableDomain[A2] with DomainLike[A2,D2],
   +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
   +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
   +Coll<:MutableDomainMap2[A1,A2,B],
   +This<:SliceTable[A1,A2,B,Coll]]
  extends MutableDomainMap2SliceTableLike[A1,A2,B,D1,D2,D,T,Coll,This]
  with MatrixLike[B,This];

  /** A SliceTable of any Double-valued MutableDomainMap is a Matrix. */
  trait SliceTable
  [@specialized(Int,Long) A1, @specialized(Int,Long) A2,
   @specialized(Int,Long,Float,Double) B,
   +Coll<:MutableDomainMap2[A1,A2,B]]
  extends MutableDomainMap2SliceTable[A1,A2,B,Coll]
  with Matrix[B]
  with SliceTableLike[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Coll,SliceTable[A1,A2,B,Coll]];

  /** Slice of a Double valued MutableDomainMap2 */
  class SliceFromKeySeqs
  [@specialized(Int,Long) A1, @specialized(Int,Long) A2,
   @specialized(Int,Long,Float,Double) B,
   +Coll<:MutableDomainMap2[A1,A2,B]]
  (underlying : Coll, keys1 : Seq[A1], keys2 : Seq[A2])
  (implicit override val scalar : Scalar[B])
  extends MutableDomainMap2SliceTable.FromKeySeqs[A1,A2,B,Coll](underlying, keys1, keys2)
  with SliceTable[A1,A2,B,Coll];

  implicit def canTranspose[B:Scalar,M<:Matrix[B]] = new DomainMap2CanTransposeFrom[M, Int, Int, Double, Transpose[B,M]] {
    override def apply(from : M) = new TransposeImpl[B,M](from);
  }
}
