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

import domain.IterableDomain;

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * Implementation trait for pass-through views of underlying Tensor.
 *
 * @author dramage
 */
trait TensorViewLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
 +D<:IterableDomain[K], +Coll <: Tensor[K,_],
 +This<:TensorView[K,V,Coll]]
extends TensorLike[K,V,D,This] {
self =>

  /** The collection underlying this view. */
  def underlying: Coll;

  /** Maps to underlying.domain */
  override def domain = underlying.domain.asInstanceOf[D];

  override def size = underlying.size;

  /** Views of views should just return this (cast as This) */
  def view = repr;
}

/**
 * Pass-through view of an underlying Tensor.
 *
 * @author dramage
 */
trait TensorView
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
 +Coll <: Tensor[K,_]]
extends Tensor[K,V]
with TensorViewLike[K,V,IterableDomain[K],Coll,TensorView[K,V,Coll]];


object TensorView {

  trait IdentityViewLike
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
   +D<:IterableDomain[K],
   +Coll <: Tensor[K,V], +This <: IdentityView[K,V,Coll]]
  extends TensorViewLike[K,V,D,Coll,This] {
    override def apply(key : K) =
      underlying.apply(key);
  }

  trait IdentityView
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
   +Coll <: Tensor[K,V]]
  extends TensorView[K,V,Coll]
  with IdentityViewLike[K,V,IterableDomain[K],Coll,IdentityView[K,V,Coll]];

  /** Returns an unmodified view of the given Tensor. */
  class IdentityViewImpl[K, V, +Coll <: Tensor[K,V]]
  (override val underlying : Coll)
  (implicit override val scalar : Scalar[V])
  extends IdentityView[K,V,Coll];

  trait TransformViewLike
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
   @specialized(Int,Long,Float,Double,Boolean) O,
   +D<:IterableDomain[K], +Coll <: Tensor[K,V],
   +This <: TransformView[K,V,O,Coll]]
  extends TensorViewLike[K,O,D,Coll,This] {
    /** Transform a value in the underlying map to a value in the view. */
    def transform(key : K, value : V) : O;

    override def apply(key : K) =
      transform(key, underlying.apply(key));
  }

  trait TransformView
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
   @specialized(Int,Long,Float,Double,Boolean) O, +Coll <: Tensor[K,V]]
  extends TensorView[K,O,Coll]
  with TransformViewLike[K,V,O,IterableDomain[K],Coll,TransformView[K,V,O,Coll]]

  /**
   * Returns an unmodified view of the given Tensor with
   * values transformed by the given function.
   */
  class TransformImpl[K, V, O, +Coll <: Tensor[K,V]]
  (override val underlying : Coll, fn : ((K,V)=>O))
  (implicit override val scalar : Scalar[O])
  extends TransformView[K,V,O,Coll] {
     override def transform(key : K, value : V) = fn(key, value);
  }

  implicit def mkTransformCanMapValues[K, V1, V2, V3:Scalar] =
  new CanMapValues[TransformView[K,V1,V2,Tensor[K,V1]],V2,V3,TransformView[K,V1,V3,Tensor[K,V1]]] {
    override def map(from : TransformView[K,V1,V2,Tensor[K,V1]], fn : (V2=>V3)) =
      new TransformImpl[K,V1,V3,Tensor[K,V1]](
        from.underlying, ((k:K, v:V1) => fn(from.transform(k,v))));
    override def mapNonZero(from : TransformView[K,V1,V2,Tensor[K,V1]], fn : (V2=>V3)) =
      map(from, fn);
  }

  implicit def mkTransformCanMapKeyValuePairs[K, V1, V2, V3:Scalar] =
  new CanMapKeyValuePairs[TransformView[K,V1,V2,Tensor[K,V1]],K,V2,V3,TransformView[K,V1,V3,Tensor[K,V1]]] {
    override def map(from : TransformView[K,V1,V2,Tensor[K,V1]], fn : ((K,V2)=>V3)) =
      new TransformImpl[K,V1,V3,Tensor[K,V1]](
        from.underlying, ((k:K, v:V1) => fn(k,from.transform(k,v))));
    override def mapNonZero(from : TransformView[K,V1,V2,Tensor[K,V1]], fn : ((K,V2)=>V3)) =
      map(from, fn);
  }

  /** Override canMapValues on TensorView instances to construct a lazy view. */
  implicit def mkIdentityCanMapValues[K, V1, V2:Scalar] =
  new CanMapValues[IdentityView[K,V1,Tensor[K,V1]],V1,V2,TransformView[K,V1,V2,Tensor[K,V1]]] {
    override def map(from : IdentityView[K,V1,Tensor[K,V1]], fn : (V1=>V2)) =
      new TransformImpl[K,V1,V2,Tensor[K,V1]](from.underlying, ((k:K,v:V1) => fn(v)));
    override def mapNonZero(from : IdentityView[K,V1,Tensor[K,V1]], fn : (V1=>V2)) =
      map(from, fn);
  }

  implicit def mkIdentityCanMapKeyValuePairs[K, V1, V2:Scalar] =
  new CanMapKeyValuePairs[IdentityView[K,V1,Tensor[K,V1]],K,V1,V2,TransformView[K,V1,V2,Tensor[K,V1]]] {
    override def map(from : IdentityView[K,V1,Tensor[K,V1]], fn : ((K,V1)=>V2)) =
      new TransformImpl[K,V1,V2,Tensor[K,V1]](from.underlying, fn);
    override def mapNonZero(from : IdentityView[K,V1,Tensor[K,V1]], fn : ((K,V1)=>V2)) =
      map(from, fn);
  }
}
