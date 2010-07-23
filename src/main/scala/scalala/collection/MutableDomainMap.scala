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
 * Implementation trait for MutableDomainMapLike.  Supports assigning,
 * updating, and transforming values.
 *
 * @author dramage
 */
trait MutableDomainMapLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 D <: IterableDomain[A] with DomainLike[A,D],
 +Repr<:MutableDomainMap[A,B,D]]
extends DomainMapLike[A, B, D, Repr] {

  /**
   * Update an individual value.  The given key must be in the
   * map's domain, but need not be in its activeDomain.
   */
  def update(key : A, value : B) : Unit;

  /** Assigns the given value to all elements of this map. */
  def :=(value : B) =
    for (key <- keysIterator) update(key,value);

  /** Assigns the corresponding value to each element of this map. */
  def :=(that : DomainMap[A,B,D]) = {
    checkDomain(that.domain);
    for (key <- keysIterator) update(key,that(key));
  }

  /** Tranforms all key value pairs in this map by applying the given function. */
  def transform(f : (A,B)=>B) =
    for (key <- keysIterator) update(key,f(key,apply(key)));

  /** Tranforms all values in this map by applying the given function. */
  def transformValues(f : B=>B) =
    for (key <- keysIterator) update(key,f(apply(key)));
}

/**
 * Supports assigning, updating, and transforming values.
 *
 * @author dramage
 */
trait MutableDomainMap
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 D <: IterableDomain[A] with DomainLike[A,D]]
extends DomainMap[A,B,D] with MutableDomainMapLike[A,B,D,MutableDomainMap[A,B,D]];

object MutableDomainMap {

  def apply
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   D <: IterableDomain[A] with DomainLike[A,D]]
  (domain : D, default : B, map : scala.collection.Map[A,B] = scala.collection.mutable.Map[A,B]()) =
    new Impl[A,B,D](map, default, domain);

  class Impl
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   D <: IterableDomain[A] with DomainLike[A,D]]
  (protected var map : scala.collection.Map[A,B], val default : B, override val domain : D)
  extends MutableDomainMap[A, B, D] {
    override def apply(key : A) : B = {
      checkKey(key);
      map.getOrElse(key, default);
    }

    override def update(key : A, value : B) = {
      checkKey(key);
      map = map.updated(key, value);
    }
  }

  implicit def canSliceFrom[A1, A2, D<:IterableDomain[A1] with DomainLike[A1,D], B] =
  new DomainMapCanSliceFrom[MutableDomainMap[A1,B,D], A1, D, A2, B, MutableDomainMap[A2,B,SetDomain[A2]]] {
    override def apply(from : MutableDomainMap[A1,B,D], keymap : scala.collection.Map[A2,A1]) =
      new MutableDomainMapSlice.FromKeyMap[A1, D, A2, B, MutableDomainMap[A1,B,D]](from, keymap);
  }

  implicit def canSliceSeqFrom[A, D<:IterableDomain[A] with DomainLike[A,D], B] =
  new DomainMapCanSliceSeqFrom[MutableDomainMap[A,B,D], A, D, B, MutableDomainSeq[B]] {
    override def apply(from : MutableDomainMap[A,B,D], keys : Seq[A]) =
      new MutableDomainMapSliceSeq.FromKeySeq[A,D,B,MutableDomainMap[A,B,D]](from, keys);
  }

  /** Slicing on a double-valued MutableDomainMap results in a Vector type. */
  implicit def canSliceVectorFrom[A, D<:IterableDomain[A] with DomainLike[A,D]] =
  new DomainMapCanSliceSeqFrom[MutableDomainMap[A,Double,D], A, D, Double, tensor.Vector] {
    override def apply(from : MutableDomainMap[A,Double,D], keys : Seq[A]) =
      new tensor.Vector.SliceFromKeySeq[A,D,MutableDomainMap[A,Double,D]](from, keys);
  }
}
