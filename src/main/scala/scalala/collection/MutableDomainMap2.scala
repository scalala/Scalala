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
 * Implementation trait for a MutableDomainMap that is also a DomainMap2.
 *
 * @author dramage
 */
trait MutableDomainMap2Like
[@specialized A1, @specialized A2, @specialized B,
 D1<:IterableDomain[A1],D2<:IterableDomain[A2],D<:Product2Domain[A1,A2,D1,D2],
 +This <: MutableDomainMap2[A1,A2,B,D1,D2,D]]
extends MutableDomainMapLike[(A1,A2),B,D,This]
with DomainMap2Like[A1,A2,B,D1,D2,D,This] {

  /** Updates the value indexed by (i,j). */
  def update(i : A1, j : A2, value : B) : Unit;

  /** Fixed alias for update(i,j,value). */
  /* final */ override def update(pos : (A1,A2), value : B) : Unit =
    update(pos._1, pos._2, value);
}

/**
 * MutableDomainMap that is also a DomainMap2.
 *
 * @author dramage
 */
trait MutableDomainMap2
[@specialized A1, @specialized A2, @specialized B,
 D1<:IterableDomain[A1],D2<:IterableDomain[A2],D<:Product2Domain[A1,A2,D1,D2]]
extends MutableDomainMap[(A1,A2),B,D]
with DomainMap2[A1,A2,B,D1,D2,D]
with MutableDomainMap2Like[A1,A2,B,D1,D2,D,MutableDomainMap2[A1,A2,B,D1,D2,D]];


object MutableDomainMap2 {
  def apply
  [@specialized A1, @specialized A2, @specialized B,
   D1<:IterableDomain[A1],D2<:IterableDomain[A2], D<:Product2Domain[A1,A2,D1,D2]]
  (domain : D, default : B, map : scala.collection.Map[(A1,A2),B] = new scala.collection.mutable.HashMap[(A1,A2),B]) =
    new Impl[A1,A2,B,D1,D2,D](map, default, domain);

//  def apply[@specialized A1, @specialized A2, @specialized B]
//  (values : ((A1,A2),B)*)(implicit default : DefaultValue[B])
//  : MutableDomainMap2[A1,A2,B,SetDomain[A1],SetDomain[A2],Product2Domain[A1,A2,SetDomain[A1],SetDomain[A2]]] = {
//    var map = scala.collection.mutable.Map[(A1,A2),B](values :_*);
//    val set1 = new scala.collection.Set[A1] {
//      def contains(key: A) =
//      def iterator: Iterator[A]
//    };
//    val set2 = new scala.collection.Set[A2] {
//
//    };
//
//    val domain = new Product2Domain[A1,A2,SetDomain[A1],SetDomain[A2]](
//      new SetDomain[A1](set1), new SetDomain[A2](set2));
//    new Impl[A1,A2,B,SetDomain[A1],SetDomain[A2],Product2Domain[A1,A2,SetDomain[A1],SetDomain[A2]]](map, default.value, domain);
//  }

  class Impl
  [@specialized A1, @specialized A2, @specialized B,
   D1<:IterableDomain[A1],D2<:IterableDomain[A2], D<:Product2Domain[A1,A2,D1,D2]]
  (protected var map : scala.collection.Map[(A1,A2),B], val default : B, override val domain : D)
  extends MutableDomainMap2[A1,A2,B,D1,D2,D] {
    override def apply(k1 : A1, k2 : A2) : B = {
      checkKey(k1,k2);
      map.getOrElse((k1,k2),default);
    }

    override def update(k1 : A1, k2 : A2, value : B) = {
      checkKey(k1,k2);
      map = map.updated((k1,k2), value);
    }
  }

  implicit def canSliceTableFrom
  [@specialized A1, @specialized A2, @specialized B,
   D1<:IterableDomain[A1], D2<:IterableDomain[A2],
   D<:Product2Domain[A1,A2,D1,D2]] =
  new DomainMap2CanSliceTableFrom
  [MutableDomainMap2[A1,A2,B,D1,D2,D],A1,A2,B,D1,D2,D,
   MutableDomainMap2SliceTable[A1,A2,B,D1,D2,D,MutableDomainMap2[A1,A2,B,D1,D2,D]]] {
    override def apply(from : MutableDomainMap2[A1,A2,B,D1,D2,D], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new MutableDomainMap2SliceTable.FromKeySeqs[A1,A2,B,D1,D2,D,MutableDomainMap2[A1,A2,B,D1,D2,D]](from, keys1, keys2);
  }

  /** Slicing on double-valued maps gives a Matrix. */
  implicit def canSliceMatrixFrom
  [@specialized A1, @specialized A2, D1<:IterableDomain[A1], D2<:IterableDomain[A2],
   D<:Product2Domain[A1,A2,D1,D2]] =
  new DomainMap2CanSliceTableFrom
  [MutableDomainMap2[A1,A2,Double,D1,D2,D],A1,A2,Double,D1,D2,D,
   tensor.Matrix.SliceFromKeySeqs[A1,A2,D1,D2,D,MutableDomainMap2[A1,A2,Double,D1,D2,D]]] {
    override def apply(from : MutableDomainMap2[A1,A2,Double,D1,D2,D], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new tensor.Matrix.SliceFromKeySeqs[A1,A2,D1,D2,D,MutableDomainMap2[A1,A2,Double,D1,D2,D]](from, keys1, keys2);
  }

  implicit def canTransposeFrom
  [@specialized A2, @specialized A1, @specialized B,
   D2<:IterableDomain[A2], D1<:IterableDomain[A1], ID<:Product2Domain[A1,A2,D1,D2]] =
  new DomainMap2CanTransposeFrom
  [MutableDomainMap2[A1,A2,B,D1,D2,ID],A1,A2,B,D1,D2,ID,
   MutableDomainMap2Transpose[A2,A1,B,D2,D1,ID,Product2Domain[A2,A1,D2,D1],MutableDomainMap2[A1,A2,B,D1,D2,ID]]] {
    override def apply(input : MutableDomainMap2[A1,A2,B,D1,D2,ID]) =
      new MutableDomainMap2Transpose.Impl[A2,A1,B,D2,D1,ID,Product2Domain[A2,A1,D2,D1],MutableDomainMap2[A1,A2,B,D1,D2,ID]](
        input, input.domain.transpose);
  }
}
