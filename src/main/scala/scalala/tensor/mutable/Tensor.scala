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

import generic.{CanMul,CanDiv,CanAdd,CanSub,CanPow,CanMod}
import generic.{CanAssignInto,CanMulInto,CanDivInto,CanAddInto,CanSubInto,CanPowInto,CanModInto}
import generic.collection._;

import scalar.Scalar;

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
with operators.MutableNumericOps[Repr] { self =>

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

  /**
   * Returns a view of this tensor as a builder for itself.  This is used
   * so that you can construct the appropriate return instance and then
   * call asBuilder to get a builder view of it.
   */
  def asBuilder[RV>:Repr] : TensorBuilder[A,B,RV] = new TensorBuilder[A,B,RV] {
    def update(k : A, v : B) = self(k) = v;
    def result = self.asInstanceOf[RV];
  }
}

/**
 * Supports assigning, updating, and transforming values.
 *
 * @author dramage
 */
trait Tensor
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends tensor.Tensor[A,B] with TensorLike[A,B,IterableDomain[A],Tensor[A,B]];

object Tensor extends TensorCompanion[Tensor] {

  /** Constructs an open-domain tensor seeded with the given values. */
  def apply[K,V:Scalar](values : (K,V)*) : Tensor[K,V] = {
    new Impl[K,V](scala.collection.mutable.Map(values :_*)) {
      override def checkKey(key : K) = true;
    }
  }

  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K,V:Scalar](domain : IterableDomain[K]) : Tensor[K,V] = {
    val d = domain;
    new Impl[K,V](scala.collection.mutable.Map[K,V]()) {
      override val domain = d;
    }
  }

  class Impl[A, B](protected var map : scala.collection.Map[A,B])
  (implicit override val scalar : Scalar[B])
  extends Tensor[A, B] {
    override def domain : IterableDomain[A] =
      SetDomain(map.keySet);
    
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

/**
 * Companion for mutable tensors.
 *
 * @author dramage
 */
trait TensorCompanion[Bound[K,V] <: Tensor[K,V]] extends tensor.TensorCompanion[Bound] {

  //
  // Tensor-scalar
  //

  implicit def canAssignScalarInto[K,V](implicit scalar : Scalar[V])
  : CanAssignInto[Bound[K,V],V] = new CanAssignInto[Bound[K,V],V] {
    override def apply(a : Bound[K,V], s : V) = {
      if (s == scalar.zero) {
        a.transformNonZeroValues(v => s);
      } else {
        a.transformValues(v => s);
      }
    }
  }

  implicit def canAddScalarInto[K,V,O](implicit op : CanAdd[V,O,V], so : Scalar[O])
  : CanAddInto[Bound[K,V],O] = new CanAddInto[Bound[K,V],O] {
    override def apply(a : Bound[K,V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }

  implicit def canSubScalarInto[K,V,O](implicit op : CanSub[V,O,V], so : Scalar[O])
  : CanSubInto[Bound[K,V],O] = new CanSubInto[Bound[K,V],O] {
    override def apply(a : Bound[K,V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }

  implicit def canMulScalarInto[K,V,O](implicit op : CanMul[V,O,V], so : Scalar[O])
  : CanMulInto[Bound[K,V],O] = new CanMulInto[Bound[K,V],O] {
    override def apply(a : Bound[K,V], b : O) = {
      if (so.isNaN(b)) {
        a.transformValues(v => op(v, b));
      } else {
        a.transformNonZeroValues(v => op(v, b));
      }
    }
  }

  /** Divides each element by the given scale factor. */
  implicit def canDivScalarInto[K,V,O](implicit op : CanDiv[V,O,V], so : Scalar[O])
  : CanDivInto[Bound[K,V],O] = new CanDivInto[Bound[K,V],O] {
    override def apply(a : Bound[K,V], b : O) = {
      if (b == so.zero || so.isNaN(b)) {
        a.transformValues(v => op(v, b));
      } else {
        a.transformNonZeroValues(v => op(v, b));
      }
    }
  }

  /** Divides each element by the given scale factor. */
  implicit def canPowScalarInto[K,V,O](implicit op : CanPow[V,O,V], so : Scalar[O])
  : CanDivInto[Bound[K,V],O] = new CanDivInto[Bound[K,V],O] {
    override def apply(a : Bound[K,V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }

  /** Divides each element by the given scale factor. */
  implicit def canModScalarInto[K,V,O](implicit op : CanMod[V,O,V], so : Scalar[O])
  : CanModInto[Bound[K,V],O] = new CanModInto[Bound[K,V],O] {
    override def apply(a : Bound[K,V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }


  //
  // Updates from another Tensor.
  //

  implicit def canAssignInto[K,V1]
  : CanAssignInto[Bound[K,V1],Tensor[K,V1]] = new CanAssignInto[Bound[K,V1],Tensor[K,V1]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V1]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => b(k));
    }
  }

  implicit def canAssignInto[K,V1,V2](implicit s : Scalar[V2], tf : (V2=>V1))
  : CanAssignInto[Bound[K,V1],Tensor[K,V2]] = new CanAssignInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => b(k));
    }
  }

  implicit def canAddBoundInto[K,V1,V2](implicit op : CanAdd[V1,V2,V1], s : Scalar[V2])
  : CanAddInto[Bound[K,V1],Tensor[K,V2]] = new CanAddInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canSubBoundInto[K,V1,V2](implicit op : CanSub[V1,V2,V1], s : Scalar[V2])
  : CanSubInto[Bound[K,V1],Tensor[K,V2]] = new CanSubInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canMulBoundInto[K,V1,V2](implicit op : CanMul[V1,V2,V1], s : Scalar[V2])
  : CanMulInto[Bound[K,V1],Tensor[K,V2]] = new CanMulInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canDivBoundInto[K,V1,V2](implicit op : CanDiv[V1,V2,V1], s : Scalar[V2])
  : CanDivInto[Bound[K,V1],Tensor[K,V2]] = new CanDivInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canModBoundInto[K,V1,V2](implicit op : CanMod[V1,V2,V1], s : Scalar[V2])
  : CanModInto[Bound[K,V1],Tensor[K,V2]] = new CanModInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canPowBoundInto[K,V1,V2](implicit op : CanPow[V1,V2,V1], s : Scalar[V2])
  : CanPowInto[Bound[K,V1],Tensor[K,V2]] = new CanPowInto[Bound[K,V1],Tensor[K,V2]] {
    override def apply(a : Bound[K,V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }
}
