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
 * Implementation trait for pass-through views of underlying DomainMap.
 *
 * @author dramage
 */
trait DomainMapViewLike
[@specialized A, @specialized B, @specialized O, D<:IterableDomain[A],
 +Coll <: DomainMap[A,B,D], +This <: DomainMapViewLike[A,B,O,D,Coll,This]]
extends DomainMapLike[A,O,D,This] {
self =>

  /** The collection underlying this view. */
  def underlying: Coll;

  /** Transform a value in the underlying map to a value in the view. */
  def transform(value : B) : O;

  override def domain =
    underlying.domain;

  override def apply(key : A) =
    transform(underlying.apply(key));
}

/**
 * Pass-through view of an underlying DomainMap.
 *
 * @author dramage
 */
trait DomainMapView
[@specialized A, @specialized B, @specialized O, D<:IterableDomain[A],
 +Coll <: DomainMap[A,B,D]]
extends DomainMapViewLike[A,B,O,D,Coll,DomainMapView[A,B,O,D,Coll]];


object DomainMapView {

  /** Returns an unmodified view of the given DomainMap. */
  class Identity
  [@specialized A, @specialized B, D<:IterableDomain[A],
   +Coll <: DomainMap[A,B,D]](override val underlying : Coll)
  extends DomainMapView[A,B,B,D,Coll] {
    override def transform(value : B) = value;
  }

  /**
   * Returns an unmodified view of the given DomainMap with
   * values transformed by the given function.
   */
  class Transform
  [@specialized A, @specialized B, @specialized O, D<:IterableDomain[A],
   +Coll <: DomainMap[A,B,D]](override val underlying : Coll, fn : (B=>O))
  extends DomainMapView[A,B,O,D,Coll] {
     override def transform(value : B) = fn(value);
  }

  /** Override canMapValues on DomainMapView instances to construct a lazy view. */
  implicit def canMapValuesFrom
  [@specialized A, @specialized B1, @specialized B2, @specialized B3, D<:IterableDomain[A]] =
  new DomainMapCanMapValuesFrom
  [DomainMapView[A,B1,B2,D,DomainMap[A,B1,D]],A,B2,B3,D,DomainMapView[A,B1,B3,D,DomainMap[A,B1,D]]] {
    override def apply(from : DomainMapView[A,B1,B2,D,DomainMap[A,B1,D]], fn : (B2=>B3)) =
      new Transform[A,B1,B3,D,DomainMap[A,B1,D]](from.underlying, from.transform _ andThen fn)
  }
}
