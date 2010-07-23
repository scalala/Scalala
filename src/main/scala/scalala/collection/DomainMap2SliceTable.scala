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
package collection;

import domain._;
import generic._;

/**
 * A table sliced from a DomainMap2 by providing a set product set of
 * keys from A1 x A2.
 *
 * @author dramage
 */
trait DomainMap2SliceTableLike
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 D1<:IterableDomain[A1] with DomainLike[A1,D1],
 D2<:IterableDomain[A2] with DomainLike[A2,D2],
 D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +Coll<:DomainMap2[A1,A2,B,D1,D2,D,T],
 +This<:DomainMap2SliceTable[A1,A2,B,D1,D2,D,T,Coll]]
extends DomainMapSliceLike[(A1,A2),D,(Int,Int),TableDomain,B,Coll,This]
with DomainTableLike[B,This]
{

  def lookup1(i : Int) : A1;
  def lookup2(j : Int) : A2;

  /* final */ override def lookup(tup : (Int,Int)) =
    (lookup1(tup._1), lookup2(tup._2));

  override def apply(i : Int, j : Int) : B =
    underlying.apply(lookup1(i), lookup2(j));
}

/**
 * A table sliced from a DomainMap2 by providing a set product set of
 * keys from A1 x A2.
 *
 * @author dramage
 */
trait DomainMap2SliceTable
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 D1<:IterableDomain[A1] with DomainLike[A1,D1],
 D2<:IterableDomain[A2] with DomainLike[A2,D2],
 D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +Coll<:DomainMap2[A1,A2,B,D1,D2,D,T]]
extends DomainMapSlice[(A1,A2),D,(Int,Int),TableDomain,B,Coll]
with DomainTable[B]
with DomainMap2SliceTableLike[A1,A2,B,D1,D2,D,T,Coll,DomainMap2SliceTable[A1,A2,B,D1,D2,D,T,Coll]];


object DomainMap2SliceTable {
  class FromKeySeqs
  [@specialized(Int,Long) A1, @specialized(Int,Long) A2,
   @specialized(Int,Long,Float,Double,Boolean) B,
   D1<:IterableDomain[A1] with DomainLike[A1,D1],
   D2<:IterableDomain[A2] with DomainLike[A2,D2],
   D<:Product2DomainLike[A1,A2,D1,D2,T,D],
   T<:Product2DomainLike[A2,A1,D2,D1,D,T],
   +Coll<:DomainMap2[A1,A2,B,D1,D2,D,T]]
  (override val underlying : Coll, val keys1 : Seq[A1], val keys2 : Seq[A2])
  extends DomainMap2SliceTable[A1, A2, B, D1, D2, D, T, Coll] {
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }
}
