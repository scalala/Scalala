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
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,_,D],
 +This <: DomainMapView[A,B,D,Coll]]
extends DomainMapLike[A,B,D,This] {
self =>

  /** The collection underlying this view. */
  def underlying: Coll;

  override def domain = underlying.domain;
}

/**
 * Pass-through view of an underlying DomainMap.
 *
 * @author dramage
 */
trait DomainMapView
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,_,D]]
extends DomainMap[A,B,D]
with DomainMapViewLike[A,B,D,Coll,DomainMapView[A,B,D,Coll]];


///**
// * Implementation trait for pass-through views of underlying DomainMap.
// *
// * @author dramage
// */
//trait DomainMapTransformViewLike
//[@specialized A, @specialized B, @specialized O, D<:IterableDomain[A],
// +Coll <: DomainMap[A,B,D], +This <: DomainMapView[A,B,O,D,Coll]]
//extends DomainMapLike[A,O,D,This] {
//self =>
//
//  /** The collection underlying this view. */
//  def underlying: Coll;
//
//  /** Transform a value in the underlying map to a value in the view. */
//  def transform(value : B) : O;
//
//  override def domain =
//    underlying.domain;
//
//  override def apply(key : A) =
//    transform(underlying.apply(key));
//
//  override def view = repr;
//}
//
///**
// * Pass-through view of an underlying DomainMap.
// *
// * @author dramage
// */
//trait DomainMapTransformView
//[@specialized A, @specialized B, @specialized O, D<:IterableDomain[A],
// +Coll <: DomainMap[A,B,D]]
//extends DomainMap[A,O,D]
//with DomainMapViewLike[A,B,O,D,Coll,DomainMapView[A,B,O,D,Coll]];


object DomainMapView {

  trait IdentityViewLike
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   D<:IterableDomain[A] with DomainLike[A,D],
   +Coll <: DomainMap[A,B,D], +This <: IdentityView[A,B,D,Coll]]
  extends DomainMapViewLike[A,B,D,Coll,This] {
    override def apply(key : A) =
      underlying.apply(key);

    def view = repr;
  }

  trait IdentityView
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,B,D]]
  extends DomainMapView[A,B,D,Coll]
  with IdentityViewLike[A,B,D,Coll,IdentityView[A,B,D,Coll]];

  /** Returns an unmodified view of the given DomainMap. */
  class IdentityViewImpl
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,B,D]]
  (override val underlying : Coll)
  extends IdentityView[A,B,D,Coll];


  trait TransformViewLike
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O,
   D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,B,D],
   +This <: TransformView[A,B,O,D,Coll]]
  extends DomainMapViewLike[A,O,D,Coll,This] {
    /** Transform a value in the underlying map to a value in the view. */
    def transform(value : B) : O;

    override def apply(key : A) =
      transform(underlying.apply(key));
  }

  trait TransformView
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O,
   D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,B,D]]
  extends DomainMapView[A,O,D,Coll]
  with TransformViewLike[A,B,O,D,Coll,TransformView[A,B,O,D,Coll]]

  /**
   * Returns an unmodified view of the given DomainMap with
   * values transformed by the given function.
   */
  class TransformImpl
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O,
   D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,B,D]]
  (override val underlying : Coll, fn : (B=>O))
  extends TransformView[A,B,O,D,Coll] {
     override def transform(value : B) = fn(value);
  }

//// TODO: not sure why this doesn't work
//  /** View of an IdentityView is a no-op. */
//  implicit def canViewFrom[A,B,D<:IterableDomain[A] with DomainLike[A,D]] =
//  new DomainMapCanViewFrom[IdentityView[A,B,D,DomainMap[A,B,D]],IdentityView[A,B,D,DomainMap[A,B,D]]] {
//    override def apply(view : IdentityView[A,B,D,DomainMap[A,B,D]]) = view;
//  }

  /** Override canMapValues on DomainMapView instances to construct a lazy view. */
  implicit def canMapValuesFromTransformView
  [A, B1, B2, B3, D<:IterableDomain[A] with DomainLike[A,D]] =
  new DomainMapCanMapValuesFrom
  [TransformView[A,B1,B2,D,DomainMap[A,B1,D]],A,B2,B3,D,TransformView[A,B1,B3,D,DomainMap[A,B1,D]]] {
    override def apply(from : TransformView[A,B1,B2,D,DomainMap[A,B1,D]], fn : (B2=>B3)) =
      new TransformImpl[A,B1,B3,D,DomainMap[A,B1,D]](from.underlying, from.transform _ andThen fn)
  }

  /** Override canMapValues on DomainMapView instances to construct a lazy view. */
  implicit def canMapValuesFromIdentityView
  [A, B1, B2, D<:IterableDomain[A] with DomainLike[A,D]] =
  new DomainMapCanMapValuesFrom
  [IdentityView[A,B1,D,DomainMap[A,B1,D]],A,B1,B2,D,TransformView[A,B1,B2,D,DomainMap[A,B1,D]]] {
    override def apply(from : IdentityView[A,B1,D,DomainMap[A,B1,D]], fn : (B1=>B2)) =
      new TransformImpl[A,B1,B2,D,DomainMap[A,B1,D]](from.underlying, fn)
  }
}
