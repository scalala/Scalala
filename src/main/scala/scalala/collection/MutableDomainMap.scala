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
 +D<:IterableDomain[A] with DomainLike[A,D], +Repr<:MutableDomainMap[A,B]]
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
  def :=(that : DomainMap[A,B]) = {
    checkDomain(that.domain);
    for (key <- keysIterator) update(key,that(key));
  }

  /** Assigns the corresponding value to each element of this map. */
  def :=[O](that : DomainMap[A,O])(implicit tf : (O=>B)) = {
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
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends DomainMap[A,B] with MutableDomainMapLike[A,B,IterableDomain[A],MutableDomainMap[A,B]];

object MutableDomainMap {

  def apply[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
  (domain : IterableDomain[A], default : B, map : scala.collection.Map[A,B] = scala.collection.mutable.Map[A,B]()) =
    new Impl[A,B](map, default, domain);

  class Impl[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
  (protected var map : scala.collection.Map[A,B], val default : B, override val domain : IterableDomain[A])
  extends MutableDomainMap[A, B] {
    override def apply(key : A) : B = {
      checkKey(key);
      map.getOrElse(key, default);
    }

    override def update(key : A, value : B) = {
      checkKey(key);
      map = map.updated(key, value);
    }
  }

  implicit def canSliceFrom[A1, A2, B] =
  new DomainMapCanSliceFrom[MutableDomainMap[A1,B], A1, A2, B, MutableDomainMap[A2,B]] {
    override def apply(from : MutableDomainMap[A1,B], keymap : scala.collection.Map[A2,A1]) =
      new MutableDomainMapSlice.FromKeyMap[A1, A2, B, MutableDomainMap[A1,B]](from, keymap);
  }

  implicit def canSliceSeqFrom[A, B] =
  new DomainMapCanSliceSeqFrom[MutableDomainMap[A,B], A, B, MutableDomainSeq[B]] {
    override def apply(from : MutableDomainMap[A,B], keys : Seq[A]) =
      new MutableDomainMapSliceSeq.FromKeySeq[A,B,MutableDomainMap[A,B]](from, keys);
  }

  /** Slicing on a double-valued MutableDomainMap results in a Vector type. */
  implicit def canSliceVectorFrom[A] =
  new DomainMapCanSliceSeqFrom[MutableDomainMap[A,Double], A, Double, tensor.Vector] {
    override def apply(from : MutableDomainMap[A,Double], keys : Seq[A]) =
      new tensor.Vector.SliceFromKeySeq[A,MutableDomainMap[A,Double]](from, keys);
  }
}
