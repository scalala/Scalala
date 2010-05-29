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

import collection._;
import collection.domain._;

/**
 * A Tensor is a Double-valued MutableDomainMap.
 *
 * @author dramage
 */
trait TensorLike[@specialized A, D<:IterableDomain[A], +This<:Tensor[A,D]]
extends MutableDomainMapLike[A,Double,D,This] {

  //
  // Scalar updates.
  //

  /** Multiplies each element by the given scale factor. */
  def *= (s : Double) =
    if (s != 1) transformValues(_ * s);

  /** Divides each element by the given scale factor. */
  def /= (s : Double) =
    if (s != 1) transformValues(_ / s);

  /** Increments element by the given scalar. */
  def += (s : Double) =
    if (s != 0) transformValues(_ + s);

  /** Decrements each element by the given scalar. */
  def -= (s : Double) =
    if (s != 0) transformValues(_ - s);

  /** Raises each element to the the given power. */
  def :^= (s : Double) =
    if (s != 1) transformValues(v => math.pow(v, s));

  /** Each element becomes itself modulo the given scalar. */
  def %= (s : Double) =
    if (s != 0 && s != 1) transformValues(_ % s);

  //
  // Updates from another DomainMap.
  //

  /** Multiplies each value in this map by the corresponding value in the other map. */
  def :*= (t : DomainMap[A,Double,D]) {
    checkDomain(t.domain);
    transform((k,v) => v * t(k));
  }

  /** Divides each value in this map by the corresponding value in the other map. */
  def :/= (t : DomainMap[A,Double,D]) {
    checkDomain(t.domain);
    transform((k,v) => v / t(k));
  }

  /** Increments each value in this map by the corresponding value in the other map. */
  def :+= (t : DomainMap[A,Double,D]) {
    checkDomain(t.domain);
    transform((k,v) => v + t(k));
  }

  /** Decrements each value in this map by the corresponding value in the other map. */
  def :-= (t : DomainMap[A,Double,D]) {
    checkDomain(t.domain);
    transform((k,v) => v - t(k));
  }

  /** Raises each value in this map by the corresponding value in the other map. */
  def :^= (t : DomainMap[A,Double,D]) {
    checkDomain(t.domain);
    transform((k,v) => math.pow(v, t(k)));
  }

  /** Modulos each value in this map by the corresponding value in the other map. */
  def :%= (t : DomainMap[A,Double,D]) {
    checkDomain(t.domain);
    transform((k,v) => v % t(k));
  }

  /** += with another PartialMap is a fixed alias for :+= */
  final def += (t : DomainMap[A,Double,D]) = this.:+=(t);

  /** -= with another PartialMap is a fixed alias for :-= */
  final def -= (t : DomainMap[A,Double,D]) = this.:-=(t);

  /** Approximate equality at a given tolerance level. */
  def =~= (tolerance : Double)(that : DomainMap[A,Double,D]) : Boolean = {
    (this eq that) ||
    (that canEqual this) &&
    (this.domain == that.domain) &&
    (domain.iterator.forall((k:A) => Math.abs(this(k) - that(k)) < tolerance));
  }

  /** Approximate equality using the default Tensor.TOLERANCE value. */
  def =~= (that : DomainMap[A,Double,D]) : Boolean =
    this.=~=(Tensor.TOLERANCE)(that);

  /** Returns !(this.=~=(tolerance)(that)) */
  def !~= (tolerance : Double)(that : DomainMap[A,Double,D]) : Boolean =
    ! this.=~=(tolerance)(that);

  /** Returns !(this.=~=(that)) */
  def !~= (that : DomainMap[A,Double,D]) : Boolean =
    ! this.=~=(that);

  //
  // Collection level queries
  //

  /** Returns the sum of the values in this map. */
  def sum : Double = {
    var sum = 0.0;
    valuesIterator.foreach(v => sum += v);
    return sum;
  }

  /** Returns the max of the values in this map. */
  def max : Double = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty tensor .max");
    }
    var max = Double.NegativeInfinity;
    valuesIterator.foreach(v => { if (v > max) max = v; })
    return max;
  }

  /** Returns the min of the values in this map. */
  def min : Double = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty tensor .min");
    }
    var min = Double.PositiveInfinity;
    valuesIterator.foreach(v => { if (v < min) min = v; })
    return min;
  }


}

/**
 * A Tensor is a Double-valued MutableDomainMap.
 *
 * @author dramage
 */
trait Tensor[A,D<:IterableDomain[A]]
extends MutableDomainMap[A,Double,D]
with TensorLike[A,D,Tensor[A,D]];

object Tensor {
  /** Default tolerance value for element-wise equality. */
  val TOLERANCE = 1e-8;

  /** A Tensor slice of a numeric MutableDomainMap. */
  trait SliceLike
  [@specialized A1, D1<:IterableDomain[A1],
   @specialized A2, D2<:IterableDomain[A2],
   +Coll <: MutableDomainMap[A1, Double, D1],
   +This <: Slice[A1, D1, A2, D2, Coll]]
  extends MutableDomainMapSliceLike[A1,D1,A2,D2,Double,Coll,This]
  with Tensor1Like[A2,D2,This];

  /** A Tensor slice of a numeric MutableDomainMap. */
  trait Slice
  [@specialized A1, D1<:IterableDomain[A1],
   @specialized A2, D2<:IterableDomain[A2],
   +Coll <: MutableDomainMap[A1, Double, D1]]
  extends MutableDomainMapSlice[A1,D1,A2,D2,Double,Coll]
  with Tensor1[A2,D2]
  with SliceLike[A1,D1,A2,D2,Coll,Slice[A1,D1,A2,D2,Coll]];


//  implicit def canMapValues[@specialized A,D<:IterableDomain[A]] =
//  new DomainMapCanBuildFrom[DomainMap[A,B,D], A, O, D, MutableDomainMap[A,O,D]] {
//    override def apply(from : DomainMap[A,B,D], domain : D) =
//      DomainMapBuilder(MutableDomainMap[A,O,D](domain, default.value));
//  }
//
//  implicit def canSliceFrom[@specialized A1, @specialized A2, D<:IterableDomain[A1], @specialized B] =
//  new DomainMapCanSliceFrom[DomainMap[A1,B,D], A1, D, A2, B, DomainMap[A2,B,SetDomain[A2]]] {
//    override def apply(from : DomainMap[A1,B,D], keymap : scala.collection.Map[A2,A1]) =
//      new DomainMapSlice.FromKeyMap[A1, D, A2, B, DomainMap[A1,B,D]](from, keymap);
//  }
//
//  implicit def canSliceSeqFrom[@specialized A, D<:IterableDomain[A], @specialized B] =
//  new DomainMapCanSliceSeqFrom[DomainMap[A,B,D], A, D, B, DomainSeq[B]] {
//    override def apply(from : DomainMap[A,B,D], keys : Seq[A]) =
//      new DomainMapSliceSeq.FromKeySeq[A,D,B,DomainMap[A,B,D]](from, keys);
//  }

}
