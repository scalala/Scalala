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
import generic.collection._;

import scalar.Scalar;

/**
 * Implementation trait for a mutable tensor.Tensor2.
 *
 * @author dramage
 */
trait Tensor2Like
[@specialized(Int) A1, @specialized(Int) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +This<:Tensor2[A1,A2,B]]
extends tensor.Tensor2Like[A1,A2,B,D1,D2,D,T,This]
with TensorLike[(A1,A2),B,D,This] { self =>

  /** Updates the value indexed by (i,j). */
  def update(i : A1, j : A2, value : B) : Unit;

  /** Fixed alias for update(i,j,value). */
  /* final */ override def update(pos : (A1,A2), value : B) : Unit =
    update(pos._1, pos._2, value);

  /** Tranforms all key value pairs in this map by applying the given function. */
  def transform(f : (A1,A2,B)=>B) =
    super.transform((k,v) => f(k._1, k._2, v));

  /** Fixed alias for transform((k1,k2,v) => f((k1,k2),v)) */
  /* final */ override def transform(f : ((A1,A2),B)=>B) =
    transform((k1,k2,v) => f((k1,k2),v));
}

/**
 * Mutable tensor.Tensor2.
 *
 * @author dramage
 */
trait Tensor2
[@specialized(Int) A1, @specialized(Int) A2,
 @specialized(Int,Long,Float,Double,Boolean) B]
extends Tensor[(A1,A2),B]
with tensor.Tensor2[A1,A2,B]
with Tensor2Like[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Tensor2[A1,A2,B]];


object Tensor2 {
  /** Constructs an open-domain tensor seeded with the given values. */
  def apply[K1,K2,V:Scalar](values : ((K1,K2),V)*) : Tensor2[K1,K2,V] = {
    new Impl[K1,K2,V](scala.collection.mutable.Map(values :_*)) {
      override def checkKey(k1 : K1, k2 : K2) = true;
    }
  }

  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K1,K2,V:Scalar](domain : Product2Domain[K1,K2]) : Tensor2[K1,K2,V] = {
    val d = domain;
    new Impl[K1,K2,V](scala.collection.mutable.Map[(K1,K2),V]()) {
      override val domain = d;
    }
  }

  class Impl[K1,K2,V:Scalar](m : scala.collection.Map[(K1,K2),V])
  extends Tensor.Impl[(K1,K2),V](m) with Tensor2[K1,K2,V] {
    override def domain =
      Product2Domain(
        SetDomain(map.keySet.map(_._1)),
        SetDomain(map.keySet.map(_._2)));

    override def apply(k1 : K1, k2 : K2) : V = {
      checkKey(k1,k2);
      map.getOrElse((k1,k2),scalar.zero);
    }

    override def update(k1 : K1, k2 : K2, value : V) = {
      checkKey(k1,k2);
      map = map.updated((k1,k2), value);
    }
  }

  implicit def canTranspose[A2,A1,B:Scalar] : CanTranspose[Tensor2[A1,A2,B],Tensor2[A2,A1,B]]
  = new CanTranspose[Tensor2[A1,A2,B],Tensor2[A2,A1,B]] {
    override def apply(from : Tensor2[A1,A2,B]) = {
      if (from.isInstanceOf[Tensor2Transpose[_,_,_,_]]) {
        from.asInstanceOf[Tensor2Transpose[_,_,_,_]].underlying.asInstanceOf[Tensor2[A2,A1,B]];
      } else {
        new Tensor2Transpose.Impl[A2,A1,B,Tensor2[A1,A2,B]](from);
      }
    }
  }

  implicit def canSliceRow[A1,A2,B:Scalar] : CanSliceRow[Tensor2[A1,A2,B],A1,Tensor1Row[A2,B]]
  = new CanSliceRow[Tensor2[A1,A2,B],A1,Tensor1Row[A2,B]] {
    override def apply(from : Tensor2[A1,A2,B], row : A1) =
      new RowSliceImpl[A1,A2,B,Tensor2[A1,A2,B]](from,row);
  }

  implicit def canSliceCol[A1,A2,B:Scalar] : CanSliceCol[Tensor2[A1,A2,B],A2,Tensor1Col[A1,B]]
  = new CanSliceCol[Tensor2[A1,A2,B],A2,Tensor1Col[A1,B]] {
    override def apply(from : Tensor2[A1,A2,B], col : A2) =
      new ColSliceImpl[A1,A2,B,Tensor2[A1,A2,B]](from, col);
  }

  implicit def canSliceMatrix[A1,A2,B:Scalar]
  : CanSliceMatrix[Tensor2[A1,A2,B],A1,A2,Matrix[B]]
  = new CanSliceMatrix[Tensor2[A1,A2,B],A1,A2,Matrix[B]] {
    override def apply(from : Tensor2[A1,A2,B], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new MatrixSliceImpl[A1,A2,B,Tensor2[A1,A2,B]](from, keys1, keys2);
  }

  trait RowSliceLike[A1,A2,B,+Coll<:Tensor2[A1,A2,B],+This<:RowSlice[A1,A2,B,Coll]]
  extends Tensor1SliceLike[(A1,A2),IterableDomain[(A1,A2)],A2,IterableDomain[A2],B,Coll,This] with Tensor1RowLike[A2,B,IterableDomain[A2],This] {
    def row : A1;
    override val domain = underlying.domain._2;
    override def lookup(key : A2) = (row,key);
  }

  trait RowSlice[A1,A2,B,+Coll<:Tensor2[A1,A2,B]]
  extends Tensor1Slice[(A1,A2),A2,B,Coll] with Tensor1Row[A2,B] with RowSliceLike[A1,A2,B,Coll,RowSlice[A1,A2,B,Coll]];

  class RowSliceImpl[A1,A2,B,+Coll<:Tensor2[A1,A2,B]]
  (override val underlying : Coll, override val row : A1)
  (implicit override val scalar : Scalar[B])
  extends RowSlice[A1,A2,B,Coll];

  trait ColSliceLike[A1,A2,B,+Coll<:Tensor2[A1,A2,B],+This<:ColSlice[A1,A2,B,Coll]]
  extends Tensor1SliceLike[(A1,A2),IterableDomain[(A1,A2)],A1,IterableDomain[A1],B,Coll,This] with Tensor1ColLike[A1,B,IterableDomain[A1],This] {
    def col : A2;
    override val domain = underlying.domain._1;
    override def lookup(key : A1) = (key,col);
  }

  trait ColSlice[A1,A2,B,+Coll<:Tensor2[A1,A2,B]]
  extends Tensor1Slice[(A1,A2),A1,B,Coll] with Tensor1Col[A1,B] with ColSliceLike[A1,A2,B,Coll,ColSlice[A1,A2,B,Coll]];

  class ColSliceImpl[A1,A2,B,+Coll<:Tensor2[A1,A2,B]]
  (override val underlying : Coll, override val col : A2)
  (implicit override val scalar : Scalar[B])
  extends ColSlice[A1,A2,B,Coll];

  trait MatrixSliceLike
  [@specialized(Int) A1, @specialized(Int) A2,
   @specialized(Int,Long,Float,Double,Boolean) B,
   +D1<:IterableDomain[A1] with DomainLike[A1,D1],
   +D2<:IterableDomain[A2] with DomainLike[A2,D2],
   +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
   +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
   +Coll<:Tensor2[A1,A2,B],
   +This<:MatrixSlice[A1,A2,B,Coll]]
  extends TensorSliceLike[(A1,A2),D,(Int,Int),TableDomain,B,Coll,This]
  with MatrixLike[B,This] {

    def lookup1(i : Int) : A1;
    def lookup2(j : Int) : A2;

    /* final */ override def lookup(tup : (Int,Int)) =
      (lookup1(tup._1), lookup2(tup._2));

    override def apply(i : Int, j : Int) : B =
      underlying.apply(lookup1(i), lookup2(j));

    override def update(i : Int, j : Int, value : B) =
      underlying.update(lookup1(i), lookup2(j), value);
  }

  trait MatrixSlice
  [@specialized(Int,Long) A1, @specialized(Int,Long) A2,
   @specialized(Int,Long,Float,Double,Boolean) B,
   +Coll<:Tensor2[A1,A2,B]]
  extends TensorSlice[(A1,A2),(Int,Int),B,Coll]
  with Matrix[B]
  with MatrixSliceLike[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Coll,MatrixSlice[A1,A2,B,Coll]];

  class MatrixSliceImpl[A1, A2, B, +Coll<:Tensor2[A1,A2,B]]
  (override val underlying : Coll, val keys1 : Seq[A1], val keys2 : Seq[A2])
  (implicit override val scalar : Scalar[B])
  extends MatrixSlice[A1, A2, B, Coll] {
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }
}
