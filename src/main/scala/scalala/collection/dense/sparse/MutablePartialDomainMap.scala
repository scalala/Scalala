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
package sparse;

import domain._;
import generic._;

/**
 * Implementation trait for a MutableDomainMap that is also a PartialMap.
 * 
 * @author dramage
 */
trait MutablePartialDomainMapLike
[@specialized A, @specialized B, D <: IterableDomain[A],
 ActiveDomain <: IterableDomain[A], Active <: MutableDomainMap[A,B,ActiveDomain], +This]
extends MutableDomainMapLike[A,B,D,This]
with PartialDomainMapLike[A,B,D,ActiveDomain,Active,This] {

  /** Update the default value. */
  def default_=(d : B) : Unit;

  override def update(key : A, value : B) = {
    checkKey(key);
    active.update(key, value);
  }

}

/**
 * A MutableDomainMap that is also a PatialMap.
 * 
 * @author dramage
 */
trait MutablePartialDomainMap
[@specialized A, @specialized B, D <: IterableDomain[A],
 ActiveDomain <: IterableDomain[A], Active <: MutableDomainMap[A,B,ActiveDomain]]
extends MutableDomainMap[A,B,D] with PartialDomainMap[A,B,D,ActiveDomain,Active]
with MutablePartialDomainMapLike[A,B,D,ActiveDomain,Active,MutablePartialDomainMap[A,B,D,ActiveDomain,Active]];

//object MutablePartialMap {
//
//  /**
//   * Returns a default immutable PartialMap for the given domain and default
//   * value.  The actual iterator of the given map are taken as the active iterator.
//   */
//  def apply[@specialized A,@specialized B, D<:IterableDomain[A]](domain : D, default : B) =
//    new Impl[A,B,D](domain, default);
//
//  class Impl[@specialized A, @specialized B, D <: IterableDomain[A]]
//  (val domain : D, initialDefault : B,
//   active : scala.collection.mutable.Map[A,B] = scala.collection.mutable.Map[A,B]())
//  extends MutablePartialMap[A,B,D] {
//    protected var _default = initialDefault;
//
//    override def default =
//      _default;
//
//    override def default_=(newDefault : B) =
//      _default = newDefault;
//
//    override def activeDomain =
//      MergeableSet(active.keySet);
//
//    override def apply(key : A) : B =
//      active.getOrElse(key, default);
//
//    override def update(key : A, value : B) =
//      active(key) = value;
//  }
//
//  class UpdateException(msg : String) extends RuntimeException(msg);
//}
