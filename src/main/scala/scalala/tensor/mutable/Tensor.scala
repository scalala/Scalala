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

object Tensor {

  /** Constructs an open-domain tensor seeded with the given values. */
  def apply[K,V:Scalar](values : (K,V)*) : Tensor[K,V] = {
//    val mI = implicitly[Manifest[Int]];
//    val mL = implicitly[Manifest[Long]];
//    val mF = implicitly[Manifest[Float]];
//    val mD = implicitly[Manifest[Double]];
//    val mB = implicitly[Manifest[Boolean]];
//
//    val types = (implicitly[Manifest[K]],implicitly[Scalar[V]].manifest);
//
//    if (types == (mI,mI)) {
//      val rv = new IntIntImpl().asInstanceOf[Tensor[K,V]];
//      for ((k,v) <- values) rv(k) = v;
//      rv;
//    } else {
      new Impl[K,V](scala.collection.mutable.Map(values :_*)) with OpenDomain[K,V];
//    }
  }

  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K,V:Scalar](domain : IterableDomain[K]) : Tensor[K,V] = {
//    val mI = implicitly[Manifest[Int]];
//    val mL = implicitly[Manifest[Long]];
//    val mF = implicitly[Manifest[Float]];
//    val mD = implicitly[Manifest[Double]];
//    val mB = implicitly[Manifest[Boolean]];
//
//    val types = (implicitly[Manifest[K]],implicitly[Scalar[V]].manifest);
//
//    if (types == (mI,mI)) {
//      val rv = new ClosedDomainIntIntImpl(domain).asInstanceOf[Tensor[K,V]];
//      for ((k,v) <- values) rv(k) = v;
//      rv;
//    } else {
      val d = domain;
      new Impl[K,V](scala.collection.mutable.Map[K,V]()) {
        override val domain = d;
      }
//    }
  }

  trait OpenDomain[A,B] { this:Tensor[A,B] =>
    override def checkKey(a: A) = ()
    override def checkDomain(domain : scalala.tensor.domain.Domain[A]) = ()
  }

  class Impl[A, B](protected val map : scala.collection.mutable.Map[A,B])
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
      map.update(key, value);
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

  implicit def opUpdateTensorScalar[K,V1,V2,Op<:OpType]
  (implicit op : BinaryOp[V1,V2,Op,V1], s : Scalar[V2])
  : BinaryUpdateOp[Tensor[K,V1],V2,Op]
  = new BinaryUpdateOp[Tensor[K,V1],V2,Op] {
    override def opType = op.opType;
    override def apply(a : Tensor[K,V1], b : V2) = {
      a.transformValues(v => op(v, b));
    }
  }

  implicit def opUpdateTensorTensor[K,V1,V2,Op<:OpType]
  (implicit op : BinaryOp[V1,V2,Op,V1])
  : BinaryUpdateOp[Tensor[K,V1],tensor.Tensor[K,V2],Op]
  = new BinaryUpdateOp[Tensor[K,V1],tensor.Tensor[K,V2],Op] {
    override def opType = op.opType;
    override def apply(a : Tensor[K,V1], b : tensor.Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }
  
  implicit def opSetTensor[K,V] : BinaryUpdateOp[Tensor[K,V],tensor.Tensor[K,V],OpSet]
  = new BinaryUpdateOp[Tensor[K,V],tensor.Tensor[K,V],OpSet] {
    def opType = OpSet;
    override def apply(a : Tensor[K,V], b : tensor.Tensor[K,V]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => b(k));
    }
  }
  
  implicit def opSetTensorCast[K,V1,V2](implicit cast : CanCast[V2,V1])
  : BinaryUpdateOp[Tensor[K,V1],tensor.Tensor[K,V2],OpSet]
  = new BinaryUpdateOp[Tensor[K,V1],tensor.Tensor[K,V2],OpSet] {
    def opType = OpSet;
    override def apply(a : Tensor[K,V1], b : tensor.Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => cast(b(k)));
    }
  }
  
  implicit def opSetScalar[K,V:Scalar]
  : BinaryUpdateOp[Tensor[K,V],V,OpSet]
  = new BinaryUpdateOp[Tensor[K,V],V,OpSet] {
    def opType = OpSet;
    override def apply(a : Tensor[K,V], b : V) = {
      a.transform((k,v) => b);
    }
  }


  //
  // Primitive implementations
  // 

//  class IntIntImpl extends Tensor[Int,Int] {
//    import com.carrotsearch.hppc.{IntIntOpenHashMap => OpenHashMap};
//    import com.carrotsearch.hppc.predicates.{IntPredicate => Predicate};
//    private type K = Int;
//    private type V = Int;
//    
//    //
//    // Below this line is copy-and pasted
//    //
//
//    val map = new OpenHashMap();
//
//    override val scalar : Scalar[V] = implicitly[Scalar[V]];
//
//    /** By default, act like a map. */
//    override def checkKey(key : K) = true;
//
//    /** By default, return map's domain. */
//    override def domain : IterableDomain[K] = {
//      val set = map.keySet;
//      new IterableDomain[K] {
//        override def foreach[O](f : K => O) = {
//          set.forEach(new Predicate() { override def apply(arg : K) = { f(arg); true } });
//        }
//
//        override def iterator = new Iterator[K] {
//          val iter = set.iterator;
//          override def hasNext =
//            iter.hasNext;
//          override def next =
//            iter.next.value;
//        }
//
//        override def size =
//          set.size;
//
//        override def contains(key : K) =
//          set.contains(key);
//      }
//    }
//
//    override def apply(key : K) : V = {
//      checkKey(key); if (map.containsKey(key)) map.lget() else 0;
//    }
//
//    override def update(key : K, value : V) : Unit = {
//      checkKey(key); map.put(key, value);
//    }
//  }
//
//  class ClosedDomainIntIntImpl(override val domain : IterableDomain[Int])
//  extends IntIntImpl {
//    override def checkKey(key : Int) =
//      domain.contains(key);
//  }
}

