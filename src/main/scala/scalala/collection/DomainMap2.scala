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
 * Implementation trait for domain maps indexed by two keys.
 *
 * @author dramage
 */
trait DomainMap2Like
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 D1<:IterableDomain[A1] with DomainLike[A1,D1],
 D2<:IterableDomain[A2] with DomainLike[A2,D2],
 D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +This<:DomainMap2[A1,A2,B,D1,D2,D,T]]
extends DomainMapLike[(A1,A2),B,D,This] {
  def checkKey(k1 : A1, k2 : A2) : Unit = {
    if (!domain._1.contains(k1) || !domain._2.contains(k2)) {
      throw new DomainException((k1,k2)+" not in domain");
    }
  }

  /* final */ override def checkKey(pos : (A1,A2)) : Unit =
    checkKey(pos._1, pos._2);

  /** Gets the value indexed by (i,j). */
  def apply(i : A1, j : A2) : B;

  /** Fixed alias for apply(i,j). */
  /* final */ override def apply(pos : (A1,A2)) : B =
    apply(pos._1, pos._2);

  /** Slice a sub-DomainMap2 */
  def apply[That](i : Seq[A1], j : Seq[A2])
  (implicit bf : DomainMap2CanSliceTableFrom[This,A1,A2,B,D1,D2,D,T,That]) : That =
    bf.apply(repr, i, j);

  /** Transpose this DomainMap2. */
  def transpose[That]
  (implicit bf : DomainMap2CanTransposeFrom[This,A1,A2,B,D1,D2,D,T,That]) : That =
    bf.apply(repr);
}

trait DomainMap2
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 D1<:IterableDomain[A1] with DomainLike[A1,D1],
 D2<:IterableDomain[A2] with DomainLike[A2,D2],
 D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 T<:Product2DomainLike[A2,A1,D2,D1,D,T]]
extends DomainMap[(A1,A2),B,D]
with DomainMap2Like[A1,A2,B,D1,D2,D,T,DomainMap2[A1,A2,B,D1,D2,D,T]]

object DomainMap2 {
  implicit def canSliceTableFrom
  [A1, A2, B,
   D1<:IterableDomain[A1] with DomainLike[A1,D1],
   D2<:IterableDomain[A2] with DomainLike[A2,D2],
   D<:Product2DomainLike[A1,A2,D1,D2,T,D],
   T<:Product2DomainLike[A2,A1,D2,D1,D,T]] =
  new DomainMap2CanSliceTableFrom
  [DomainMap2[A1,A2,B,D1,D2,D,T],A1,A2,B,D1,D2,D,T,
   DomainMap2SliceTable[A1,A2,B,D1,D2,D,T,DomainMap2[A1,A2,B,D1,D2,D,T]]] {
    override def apply(from : DomainMap2[A1,A2,B,D1,D2,D,T], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new DomainMap2SliceTable.FromKeySeqs[A1,A2,B,D1,D2,D,T,DomainMap2[A1,A2,B,D1,D2,D,T]](from, keys1, keys2);
  }

  implicit def canTransposeFrom
  [A2, A1, B,
   D2<:IterableDomain[A2] with DomainLike[A2,D2],
   D1<:IterableDomain[A1] with DomainLike[A1,D1],
   T<:Product2DomainLike[A2,A1,D2,D1,D,T],
   D<:Product2DomainLike[A1,A2,D1,D2,T,D]] =
  new DomainMap2CanTransposeFrom
  [DomainMap2[A1,A2,B,D1,D2,D,T],A1,A2,B,D1,D2,D,T,
   DomainMap2Transpose[A2,A1,B,D2,D1,T,D,DomainMap2[A1,A2,B,D1,D2,D,T]]] {
    override def apply(input : DomainMap2[A1,A2,B,D1,D2,D,T]) =
      new DomainMap2Transpose.Impl[A2,A1,B,D2,D1,T,D,DomainMap2[A1,A2,B,D1,D2,D,T]](input);
  }
}
