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
 +D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,_],
 +This<:DomainMapView[A,B,Coll]]
extends DomainMapLike[A,B,D,This] {
self =>

  /** The collection underlying this view. */
  def underlying: Coll;

  /** Maps to underlying.domain */
  override def domain = underlying.domain.asInstanceOf[D];

  /** Views of views should just return this (cast as This) */
  def view = repr;
}

/**
 * Pass-through view of an underlying DomainMap.
 *
 * @author dramage
 */
trait DomainMapView
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll <: DomainMap[A,_]]
extends DomainMap[A,B]
with DomainMapViewLike[A,B,IterableDomain[A],Coll,DomainMapView[A,B,Coll]];


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
   +D<:IterableDomain[A] with DomainLike[A,D],
   +Coll <: DomainMap[A,B], +This <: IdentityView[A,B,Coll]]
  extends DomainMapViewLike[A,B,D,Coll,This] {
    override def apply(key : A) =
      underlying.apply(key);
  }

  trait IdentityView
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   +Coll <: DomainMap[A,B]]
  extends DomainMapView[A,B,Coll]
  with IdentityViewLike[A,B,IterableDomain[A],Coll,IdentityView[A,B,Coll]];

  /** Returns an unmodified view of the given DomainMap. */
  class IdentityViewImpl
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   +Coll <: DomainMap[A,B]]
  (override val underlying : Coll)
  extends IdentityView[A,B,Coll];


  trait TransformViewLike
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O,
   +D<:IterableDomain[A] with DomainLike[A,D], +Coll <: DomainMap[A,B],
   +This <: TransformView[A,B,O,Coll]]
  extends DomainMapViewLike[A,O,D,Coll,This] {
    /** Transform a value in the underlying map to a value in the view. */
    def transform(key : A, value : B) : O;

    override def apply(key : A) =
      transform(key, underlying.apply(key));
  }

  trait TransformView
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O, +Coll <: DomainMap[A,B]]
  extends DomainMapView[A,O,Coll]
  with TransformViewLike[A,B,O,IterableDomain[A],Coll,TransformView[A,B,O,Coll]]

  /**
   * Returns an unmodified view of the given DomainMap with
   * values transformed by the given function.
   */
  class TransformImpl
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O, +Coll <: DomainMap[A,B]]
  (override val underlying : Coll, fn : ((A,B)=>O))
  extends TransformView[A,B,O,Coll] {
     override def transform(key : A, value : B) = fn(key, value);
  }

//// TODO: not sure why this doesn't work
//  /** View of an IdentityView is a no-op. */
//  implicit def canViewFrom[A,B,D<:IterableDomain[A] with DomainLike[A,D]] =
//  new DomainMapCanViewFrom[IdentityView[A,B,D,DomainMap[A,B,D]],IdentityView[A,B,D,DomainMap[A,B,D]]] {
//    override def apply(view : IdentityView[A,B,D,DomainMap[A,B,D]]) = view;
//  }

  /** Override canMapValues on DomainMapView instances to construct a lazy view. */
  implicit def canMapValuesFromTransformView[A, B1, B2, B3] =
  new DomainMapCanMapValuesFrom
  [TransformView[A,B1,B2,DomainMap[A,B1]],A,B2,B3,TransformView[A,B1,B3,DomainMap[A,B1]]] {
    override def apply(from : TransformView[A,B1,B2,DomainMap[A,B1]], fn : (B2=>B3)) =
      new TransformImpl[A,B1,B3,DomainMap[A,B1]](
        from.underlying, ((k:A, v:B1) => fn(from.transform(k,v))));
    override def apply(from : TransformView[A,B1,B2,DomainMap[A,B1]], fn : ((A,B2)=>B3)) =
      new TransformImpl[A,B1,B3,DomainMap[A,B1]](
        from.underlying, ((k:A, v:B1) => fn(k,from.transform(k,v))));
  }

  /** Override canMapValues on DomainMapView instances to construct a lazy view. */
  implicit def canMapValuesFromIdentityView[A, B1, B2] =
  new DomainMapCanMapValuesFrom
  [IdentityView[A,B1,DomainMap[A,B1]],A,B1,B2,TransformView[A,B1,B2,DomainMap[A,B1]]] {
    override def apply(from : IdentityView[A,B1,DomainMap[A,B1]], fn : (B1=>B2)) =
      new TransformImpl[A,B1,B2,DomainMap[A,B1]](from.underlying, ((k:A,v:B1) => fn(v)));
    override def apply(from : IdentityView[A,B1,DomainMap[A,B1]], fn : ((A,B1)=>B2)) =
      new TransformImpl[A,B1,B2,DomainMap[A,B1]](from.underlying, fn);
  }
}
