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

import domain.{IterableDomain,SetDomain,Domain1,IndexDomain,Domain2,TableDomain};
import generic.TensorBuilder;

import scalala.generic.collection._;
import scalala.scalar.Scalar
import operators._


/**
 * Implementation trait for TensorLike.  Supports assigning,
 * updating, and transforming values.
 *
 * @author dramage
 */
trait TensorLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
 +D<:IterableDomain[K], +Repr<:Tensor[K,V]]
extends tensor.TensorLike[K, V, D, Repr]
with operators.MutableNumericOps[Repr] { self =>

  /**
   * Update an individual value.  The given key must be in the
   * map's domain, but need not be in its activeDomain.
   */
  def update(key : K, value : V) : Unit;

  /** Tranforms all key value pairs in this map by applying the given function. */
  def transformPairs(f : (K,V)=>V) =
    this.foreachPair((k,v) => update(k,f(k,v)));

  /**
   * Uses the given function to update all elements of the domain
   * that have a non-zero values (and possibly some that have zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def transformNonZeroPairs(fn : ((K,V)=>V)) : Boolean = {
    this.transformPairs(fn);
    true;
  }

  /** Tranforms all values in this map by applying the given function. */
  def transformValues(f : V=>V) =
    this.foreachPair((k,v) => update(k,f(v)));

  /**
   * Uses the given function to update all elements of the domain
   * that have a non-zero values (and possibly some that have zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def transformNonZeroValues(fn : (V=>V)) = {
    this.transformValues(fn);
    true;
  }

  /**
   * Returns a view of this tensor as a builder for itself.  This is used
   * so that you can construct the appropriate return instance and then
   * call asBuilder to get a builder view of it.
   */
  def asBuilder : TensorBuilder[K,V,Tensor[K,V]] = new TensorBuilder[K,V,Tensor[K,V]] {
    def update(k : K, v : V) = self(k) = v;
    def result = repr;
  }
}

/**
 * Supports assigning, updating, and transforming values.
 *
 * @author dramage
 */
trait Tensor
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V]
extends tensor.Tensor[K,V] with TensorLike[K,V,IterableDomain[K],Tensor[K,V]];

object Tensor extends TensorImplicitsLevel1 {
  /** Constructs a tensor for the given domain. */
  def apply[K,V:Scalar](domain : IterableDomain[K]) : Tensor[K,V] = domain match {
    case d : IndexDomain => VectorCol(d);
    case d : Domain1[_] => Tensor1(d);
    case d : TableDomain => Matrix(d);
    case d : Domain2[_,_] => Tensor2(d);
    case _ => new Impl[K,V](domain, scala.collection.mutable.Map[K,V]());
  }

  /** Default implementation on a closed domain. */
  class Impl[K, V]
  (override val domain : IterableDomain[K],
   protected val data : scala.collection.mutable.Map[K,V])
  (implicit override val scalar : Scalar[V])
  extends Tensor[K, V] {
    override def size = domain.size;
  
    override def apply(key : K) : V = {
      checkKey(key);
      data.getOrElse(key, scalar.zero);
    }

    override def update(key : K, value : V) = {
      checkKey(key);
      data.update(key, value);
    }
  }

  implicit def canSliceTensor[K1, K2, V:Scalar] =
  new CanSliceTensor[Tensor[K1,V], K1, K2, Tensor[K2,V]] {
    override def apply(from : Tensor[K1,V], keymap : scala.collection.Map[K2,K1]) =
      new TensorSlice.FromKeyMap[K1, K2, V, Tensor[K1,V]](from, keymap);
  }

  implicit def canSliceVector[K, V:Scalar] =
  new CanSliceVector[Tensor[K,V], K, VectorCol[V]] {
    override def apply(from : Tensor[K,V], keys : Seq[K]) =
      new VectorColSlice.FromKeySeq[K,V,Tensor[K,V]](from, keys);
  }
}

/**
 * Low priority implicits for mutable tensor updates (with cast).
 *
 * @author dramage
 */
trait TensorImplicitsLevel0 {
  implicit def opSetTensorTensorCast[K,V1,V2,A,B]
  (implicit v1 : A=>Tensor[K,V1], v2 : B=>tensor.Tensor[K,V2], cast : CanCast[V2,V1],
   jj : CanJoin[A,B,K,V1,V2])
  : BinaryUpdateOp[A,B,OpSet]
  = new BinaryUpdateOp[A,B,OpSet] {
    def opType = OpSet;
    override def apply(a : A, b : B) = {
      val _a = v1(a);
      jj.joinAll(a,b,(k,v1,v2) => _a(k) = cast(v2));
    }
  }
  
  implicit def opSetTensorScalarCast[K,V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : BinaryUpdateOp[Tensor[K,V1],V2,OpSet]
  = new BinaryUpdateOp[Tensor[K,V1],V2,OpSet] {
    def opType = OpSet;
    override def apply(a : Tensor[K,V1], b : V2) = {
      val newV = cast(b);
      a.transformPairs((k,oldV) => newV);
    }
  }
}

/**
 * Higher priority implicits for mutable tensor updates (no cast).
 *
 * @author dramage
 */
trait TensorImplicitsLevel1 extends TensorImplicitsLevel0 {
  implicit def opUpdateTensorTensor[K,V1,V2,Op<:OpType,A,B]
  (implicit v1 : A=>Tensor[K,V1], v2 : B=>tensor.Tensor[K,V2],
   op : BinaryOp[V1,V2,Op,V1], jj : CanJoin[A,B,K,V1,V2])
  : BinaryUpdateOp[A,B,Op]
  = new BinaryUpdateOp[A,B,Op] {
    override def opType = op.opType;
    override def apply(a : A, b : B) = {
      val _a = v1(a);
      jj.joinEitherNonZero(a,b,(k,v1,v2) => _a(k) = op(v1,v2));
    }
  }
  
  implicit def opSetTensorTensor[K,V,A,B]
  (implicit v1 : A=>Tensor[K,V], v2 : B=>tensor.Tensor[K,V],
   jj : CanJoin[A,B,K,V,V])
  : BinaryUpdateOp[A,B,OpSet]
  = new BinaryUpdateOp[A,B,OpSet] {
    def opType = OpSet;
    override def apply(a : A, b : B) = {
      val _a = v1(a);
      jj.joinAll(a,b,(k,v1,v2) => _a(k) = v2);
    }
  }

  implicit def opUpdateTensorScalar[K,V1,V2,Op<:OpType]
  (implicit op : BinaryOp[V1,V2,Op,V1], s : Scalar[V2])
  : BinaryUpdateOp[Tensor[K,V1],V2,Op]
  = new BinaryUpdateOp[Tensor[K,V1],V2,Op] {
    override def opType = op.opType;
    override def apply(a : Tensor[K,V1], b : V2) = {
      a.transformValues(v => op(v, b));
    }
  }
  
  implicit def opSetTensorScalar[K,V:Scalar]
  : BinaryUpdateOp[Tensor[K,V],V,OpSet]
  = new BinaryUpdateOp[Tensor[K,V],V,OpSet] {
    def opType = OpSet;
    override def apply(a : Tensor[K,V], b : V) = {
      a.transformPairs((k,v) => b);
    }
  }
}

