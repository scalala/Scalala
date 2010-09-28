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
package mutable;

import domain._;
import generic.tensor._;

import generic.{CanMul,CanDiv,CanAdd,CanSub,CanPow,CanMod}

/**
 * Implementation trait for TensorLike.  Supports assigning,
 * updating, and transforming values.
 *
 * @author dramage
 */
trait TensorLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D], +Repr<:Tensor[A,B]]
extends tensor.TensorLike[A, B, D, Repr]
with operators.MutableNumericOps[Repr] {

  /**
   * Update an individual value.  The given key must be in the
   * map's domain, but need not be in its activeDomain.
   */
  def update(key : A, value : B) : Unit;

  /** Tranforms all key value pairs in this map by applying the given function. */
  def transform(f : (A,B)=>B) =
    this.foreach((k,v) => update(k,f(k,v)));

  /**
   * Uses the given function to update all elements of the domain
   * that have a non-zero values (and possibly some that have zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def transformNonZero(fn : ((A,B)=>B)) : Boolean = {
    this.transform(fn);
    true;
  }

  /** Tranforms all values in this map by applying the given function. */
  def transformValues(f : B=>B) =
    this.foreach((k,v) => update(k,f(v)));

  /**
   * Uses the given function to update all elements of the domain
   * that have a non-zero values (and possibly some that have zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def transformNonZeroValues(fn : (B=>B)) = {
    this.transformValues(fn);
    true;
  }


  //
  // Scalar updates
  //

  /** Assigns the given value to all elements of this map. */
  def := (s : B) = {
    if (s == scalar.zero) {
      transformNonZeroValues(v => s);
    } else {
      transformValues(v => s);
    }
  }

  /** Increments element by the given scalar. */
  def :+=[O](s : O)(implicit op : CanAdd[B,O,B], so : Scalar[O]) =
    transformValues(v => op(v, s));

  /** Decrements each element by the given scalar. */
  def :-=[O](s : O)(implicit op : CanSub[B,O,B], so : Scalar[O]) =
    transformValues(v => op(v, s));

  /** Multiplies each element by the given scale factor. */
  def :*=[O](s : O)(implicit op : CanMul[B,O,B], so : Scalar[O]) = {
    if (so.isNaN(s)) {
      transformValues(v => op(v, s));
    } else {
      transformNonZeroValues(v => op(v, s));
    }
  }

  /** Divides each element by the given scale factor. */
  def :/=[O](s : O)(implicit op : CanDiv[B,O,B], so : Scalar[O]) = {
    if (s == so.zero || so.isNaN(s)) {
      transformValues(v => op(v, s));
    } else {
      transformNonZeroValues(v => op(v,s));
    }
  }

  /** Raises each element to the the given power. */
  def :^=[O](s : O)(implicit op : CanPow[B,O,B], so : Scalar[O]) =
    transformValues(v => op(v, s));

  /** Each element becomes itself modulo the given scalar. */
  def :%=[O](s : O)(implicit op : CanMod[B,O,B], so : Scalar[O]) =
    transformValues(v => op(v, s));

  //
  // Scalar operator aliases
  //

  /** Alias for :+=(s) */
  final def +=[O](s : O)(implicit op : CanAdd[B,O,B], so : Scalar[O]) = this.:+=(s);

  /** Alias for :-=(s) */
  final def -=[O](s : O)(implicit op : CanSub[B,O,B], so : Scalar[O]) = this.:-=(s);

  /** Alias for :*=(s) */
  final def *=[O](s : O)(implicit op : CanMul[B,O,B], so : Scalar[O]) = this.:*=(s);

  /** Alias for :/=(s) */
  final def /=[O](s : O)(implicit op : CanDiv[B,O,B], so : Scalar[O]) = this.:/=(s);

  /** Alias for :^=(s) */
  final def ^=[O](s : O)(implicit op : CanPow[B,O,B], so : Scalar[O]) = this.:^=(s);

  /** Alias for :%=(s) */
  final def %=[O](s : O)(implicit op : CanMod[B,O,B], so : Scalar[O]) = this.:%=(s);


  //
  // Updates from another Tensor.
  //

  /** Assigns the corresponding value to each element of this map. */
  def :=(that : tensor.Tensor[A,B]) = {
    checkDomain(that.domain);
    for (key <- domain) update(key,that(key));
  }

  /** Assigns the corresponding value to each element of this map. */
  def :=[O](that : tensor.Tensor[A,O])(implicit tf : (O=>B)) = {
    checkDomain(that.domain);
    for (key <- domain) update(key,that(key));
  }

  /** Increments each value in this map by the corresponding value in the other map. */
  def :+=[O](t : tensor.Tensor[A,O])(implicit op : CanAdd[B,O,B]) = {
    checkDomain(t.domain);
    transform((k,v) => op(v,t(k)));
  }

  /** Decrements each value in this map by the corresponding value in the other map. */
  def :-=[O](t : tensor.Tensor[A,O])(implicit op : CanSub[B,O,B]) = {
    checkDomain(t.domain);
    transform((k,v) => op(v,t(k)));
  }

  /** Multiplies each value in this map by the corresponding value in the other map. */
  def :*=[O](t : tensor.Tensor[A,O])(implicit op : CanMul[B,O,B]) = {
    checkDomain(t.domain);
    transform((k,v) => op(v,t(k)));
  }

  /** Divides each value in this map by the corresponding value in the other map. */
  def :/=[O](t : tensor.Tensor[A,O])(implicit op : CanDiv[B,O,B]) = {
    checkDomain(t.domain);
    transform((k,v) => op(v,t(k)));
  }

  /** Modulos each value in this map by the corresponding value in the other map. */
  def :%=[O](t : tensor.Tensor[A,O])(implicit op : CanMod[B,O,B]) = {
    checkDomain(t.domain);
    transform((k,v) => op(v,t(k)));
  }

  /** Raises each value in this map by the corresponding value in the other map. */
  def :^=[O](t : tensor.Tensor[A,O])(implicit op : CanPow[B,O,B]) = {
    checkDomain(t.domain);
    transform((k,v) => op(v,t(k)));
  }

  //
  // Tensor method aliases
  //

  /** Alias for :+= */
  final def +=[O](t : tensor.Tensor[A,O])(implicit op : CanAdd[B,O,B]) = this.:+=(t);

  /** Alias for :-= */
  final def -=[O](t : tensor.Tensor[A,O])(implicit op : CanSub[B,O,B]) = this.:-=(t);
}

/**
 * Supports assigning, updating, and transforming values.
 *
 * @author dramage
 */
trait Tensor
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends tensor.Tensor[A,B] with TensorLike[A,B,IterableDomain[A],Tensor[A,B]];

object Tensor {

  def apply[A, B:Scalar](domain : IterableDomain[A], map : scala.collection.Map[A,B] = scala.collection.mutable.Map[A,B]()) =
    new Impl[A,B](map, domain);

  class Impl[A, B](protected var map : scala.collection.Map[A,B], override val domain : IterableDomain[A])
  (implicit override val scalar : Scalar[B])
  extends Tensor[A, B] {
    override def apply(key : A) : B = {
      checkKey(key);
      map.getOrElse(key, scalar.zero);
    }

    override def update(key : A, value : B) = {
      checkKey(key);
      map = map.updated(key, value);
    }
  }

  implicit def canSliceTensor[A1, A2, B:Scalar] =
  new CanSliceTensor[Tensor[A1,B], A1, A2, Tensor[A2,B]] {
    override def apply(from : Tensor[A1,B], keymap : scala.collection.Map[A2,A1]) =
      new TensorSlice.FromKeyMap[A1, A2, B, Tensor[A1,B]](from, keymap);
  }

  implicit def canSliceVector[A, B:Scalar] =
  new CanSliceVector[Tensor[A,B], A, Vector[B]] {
    override def apply(from : Tensor[A,B], keys : Seq[A]) =
      new VectorSlice.FromKeySeq[A,B,Tensor[A,B]](from, keys);
  }
}
