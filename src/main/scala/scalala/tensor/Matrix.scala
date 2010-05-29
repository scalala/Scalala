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

import collection._;
import collection.domain._;
import collection.generic._;

/**
 * Implementation trait for a matrix.
 *
 * @author dramage
 */
trait MatrixLike[+This<:Matrix]
extends MutableDomainTableLike[Double,This]
with Tensor2Like[Int,Int,IndexDomain,IndexDomain,TableDomain,This];

trait Matrix
extends MutableDomainTable[Double]
with Tensor2[Int,Int,IndexDomain,IndexDomain,TableDomain]
with MatrixLike[Matrix];

object Matrix {
  /** A Transpose of any Matrix type is a Matrix. */
  trait TransposeLike[+Coll <: Matrix, +This <: Transpose[Coll]]
  extends Tensor2.TransposeLike[Int,Int,IndexDomain,IndexDomain,TableDomain,TableDomain,Coll,This]
  with MatrixLike[This] {
    override def domain = underlying.domain.transpose;
  }

  trait Transpose[+Coll <: Matrix]
  extends Tensor2.Transpose[Int,Int,IndexDomain,IndexDomain,TableDomain,TableDomain,Coll]
  with Matrix with TransposeLike[Coll, Transpose[Coll]];

  /** Default implementation. */
  class TransposeImpl[+Coll <: Matrix]
  (override val underlying : Coll)
  extends Transpose[Coll];

  /** A SliceTable of any Double-valued MutableDomainMap is a Matrix. */
  trait SliceTableLike
  [@specialized A1, @specialized A2,
   D1<:IterableDomain[A1], D2<:IterableDomain[A2],
   D<:Product2Domain[A1,A2,D1,D2], +Coll<:MutableDomainMap2[A1,A2,Double,D1,D2,D],
   +This<:SliceTable[A1,A2,D1,D2,D,Coll]]
  extends MutableDomainMap2SliceTableLike[A1,A2,Double,D1,D2,D,Coll,This]
  with MatrixLike[This];

  /** A SliceTable of any Double-valued MutableDomainMap is a Matrix. */
  trait SliceTable
  [@specialized A1, @specialized A2,
   D1<:IterableDomain[A1], D2<:IterableDomain[A2],
   D<:Product2Domain[A1,A2,D1,D2], +Coll<:MutableDomainMap2[A1,A2,Double,D1,D2,D]]
  extends MutableDomainMap2SliceTable[A1,A2,Double,D1,D2,D,Coll]
  with Matrix
  with SliceTableLike[A1,A2,D1,D2,D,Coll,SliceTable[A1,A2,D1,D2,D,Coll]];

  /** Slice of a Double valued MutableDomainMap2 */
  class SliceFromKeySeqs
  [@specialized A1, @specialized A2,
   D1<:IterableDomain[A1], D2<:IterableDomain[A2], D<:Product2Domain[A1,A2,D1,D2],
   +Coll <: MutableDomainMap2[A1,A2,Double,D1,D2,D]](underlying : Coll, keys1 : Seq[A1], keys2 : Seq[A2])
  extends MutableDomainMap2SliceTable.FromKeySeqs[A1,A2,Double,D1,D2,D,Coll](underlying, keys1, keys2)
  with SliceTable[A1,A2,D1,D2,D,Coll];


  implicit def canTranspose[M<:Matrix] = new DomainMap2CanTransposeFrom[M, Int, Int, Double, IndexDomain, IndexDomain, TableDomain, Transpose[M]] {
    override def apply(from : M) = new TransposeImpl[M](from);
  }
}
