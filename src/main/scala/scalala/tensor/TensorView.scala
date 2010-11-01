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

import scalar.Scalar;

import domain._;
import generic.collection._;

/**
 * Implementation trait for pass-through views of underlying Tensor.
 *
 * @author dramage
 */
trait TensorViewLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D], +Coll <: Tensor[A,_],
 +This<:TensorView[A,B,Coll]]
extends TensorLike[A,B,D,This] {
self =>

  /** The collection underlying this view. */
  def underlying: Coll;

  /** Maps to underlying.domain */
  override def domain = underlying.domain.asInstanceOf[D];

  /** Views of views should just return this (cast as This) */
  def view = repr;
}

/**
 * Pass-through view of an underlying Tensor.
 *
 * @author dramage
 */
trait TensorView
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll <: Tensor[A,_]]
extends Tensor[A,B]
with TensorViewLike[A,B,IterableDomain[A],Coll,TensorView[A,B,Coll]];


object TensorView {

  trait IdentityViewLike
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   +D<:IterableDomain[A] with DomainLike[A,D],
   +Coll <: Tensor[A,B], +This <: IdentityView[A,B,Coll]]
  extends TensorViewLike[A,B,D,Coll,This] {
    override def apply(key : A) =
      underlying.apply(key);
  }

  trait IdentityView
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   +Coll <: Tensor[A,B]]
  extends TensorView[A,B,Coll]
  with IdentityViewLike[A,B,IterableDomain[A],Coll,IdentityView[A,B,Coll]];

  /** Returns an unmodified view of the given Tensor. */
  class IdentityViewImpl[A, B, +Coll <: Tensor[A,B]]
  (override val underlying : Coll)
  (implicit override val scalar : Scalar[B])
  extends IdentityView[A,B,Coll];

  trait TransformViewLike
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O,
   +D<:IterableDomain[A] with DomainLike[A,D], +Coll <: Tensor[A,B],
   +This <: TransformView[A,B,O,Coll]]
  extends TensorViewLike[A,O,D,Coll,This] {
    /** Transform a value in the underlying map to a value in the view. */
    def transform(key : A, value : B) : O;

    override def apply(key : A) =
      transform(key, underlying.apply(key));
  }

  trait TransformView
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
   @specialized(Int,Long,Float,Double,Boolean) O, +Coll <: Tensor[A,B]]
  extends TensorView[A,O,Coll]
  with TransformViewLike[A,B,O,IterableDomain[A],Coll,TransformView[A,B,O,Coll]]

  /**
   * Returns an unmodified view of the given Tensor with
   * values transformed by the given function.
   */
  class TransformImpl[A, B, O, +Coll <: Tensor[A,B]]
  (override val underlying : Coll, fn : ((A,B)=>O))
  (implicit override val scalar : Scalar[O])
  extends TransformView[A,B,O,Coll] {
     override def transform(key : A, value : B) = fn(key, value);
  }

  implicit def mkTransformCanMapValues[A, B1, B2, B3:Scalar] =
  new CanMapValues[TransformView[A,B1,B2,Tensor[A,B1]],B2,B3,TransformView[A,B1,B3,Tensor[A,B1]]] {
    override def map(from : TransformView[A,B1,B2,Tensor[A,B1]], fn : (B2=>B3)) =
      new TransformImpl[A,B1,B3,Tensor[A,B1]](
        from.underlying, ((k:A, v:B1) => fn(from.transform(k,v))));
    override def mapNonZero(from : TransformView[A,B1,B2,Tensor[A,B1]], fn : (B2=>B3)) =
      map(from, fn);
  }

  implicit def mkTransformCanMapKeyValuePairs[A, B1, B2, B3:Scalar] =
  new CanMapKeyValuePairs[TransformView[A,B1,B2,Tensor[A,B1]],A,B2,B3,TransformView[A,B1,B3,Tensor[A,B1]]] {
    override def map(from : TransformView[A,B1,B2,Tensor[A,B1]], fn : ((A,B2)=>B3)) =
      new TransformImpl[A,B1,B3,Tensor[A,B1]](
        from.underlying, ((k:A, v:B1) => fn(k,from.transform(k,v))));
    override def mapNonZero(from : TransformView[A,B1,B2,Tensor[A,B1]], fn : ((A,B2)=>B3)) =
      map(from, fn);
  }

  /** Override canMapValues on TensorView instances to construct a lazy view. */
  implicit def mkIdentityCanMapValues[A, B1, B2:Scalar] =
  new CanMapValues[IdentityView[A,B1,Tensor[A,B1]],B1,B2,TransformView[A,B1,B2,Tensor[A,B1]]] {
    override def map(from : IdentityView[A,B1,Tensor[A,B1]], fn : (B1=>B2)) =
      new TransformImpl[A,B1,B2,Tensor[A,B1]](from.underlying, ((k:A,v:B1) => fn(v)));
    override def mapNonZero(from : IdentityView[A,B1,Tensor[A,B1]], fn : (B1=>B2)) =
      map(from, fn);
  }

  implicit def mkIdentityCanMapKeyValuePairs[A, B1, B2:Scalar] =
  new CanMapKeyValuePairs[IdentityView[A,B1,Tensor[A,B1]],A,B1,B2,TransformView[A,B1,B2,Tensor[A,B1]]] {
    override def map(from : IdentityView[A,B1,Tensor[A,B1]], fn : ((A,B1)=>B2)) =
      new TransformImpl[A,B1,B2,Tensor[A,B1]](from.underlying, fn);
    override def mapNonZero(from : IdentityView[A,B1,Tensor[A,B1]], fn : ((A,B1)=>B2)) =
      map(from, fn);
  }
}
