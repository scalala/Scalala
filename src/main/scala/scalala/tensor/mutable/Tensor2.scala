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

import domain.{Domain1,Domain2,SetDomain,TableDomain};

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * Implementation trait for a mutable tensor.Tensor2.
 *
 * @author dramage
 */
trait Tensor2Like
[@specialized(Int) K1, @specialized(Int) K2,
 @specialized(Int,Long,Float,Double,Boolean) V,
 +D1<:Domain1[K1], +D2<:Domain1[K2], +D<:Domain2[K1,K2], +T<:Domain2[K2,K1],
 +This<:Tensor2[K1,K2,V]]
extends tensor.Tensor2Like[K1,K2,V,D1,D2,D,T,This]
with TensorLike[(K1,K2),V,D,This] { self =>

  /** Updates the value indexed by (i,j). */
  def update(i : K1, j : K2, value : V) : Unit;

  /** Fixed alias for update(i,j,value). */
  /* final */ override def update(pos : (K1,K2), value : V) : Unit =
    update(pos._1, pos._2, value);

  /** Tranforms all key value pairs in this map by applying the given function. */
  def transform(f : (K1,K2,V)=>V) =
    transformPairs((k,v) => f(k._1, k._2, v));
    
  override def t : Tensor2[K2,K1,V] =
    new Tensor2Transpose.Impl[K2,K1,V,This](repr)
}

/**
 * Mutable tensor.Tensor2.
 *
 * @author dramage
 */
trait Tensor2
[@specialized(Int) K1, @specialized(Int) K2,
 @specialized(Int,Long,Float,Double,Boolean) V]
extends Tensor[(K1,K2),V]
with tensor.Tensor2[K1,K2,V]
with Tensor2Like[K1,K2,V,Domain1[K1],Domain1[K2],Domain2[K1,K2],Domain2[K2,K1],Tensor2[K1,K2,V]];


object Tensor2 {
  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K1,K2,V:Scalar](domain : Domain2[K1,K2]) : Tensor2[K1,K2,V] = domain match {
    case d : TableDomain => Matrix(d);
    case _ => new Impl(domain, scala.collection.mutable.Map[(K1,K2),V]());
  }

  class Impl[K1,K2,V:Scalar]
  (override val domain : Domain2[K1,K2],
   protected override val data : scala.collection.mutable.Map[(K1,K2),V])
  extends Tensor.Impl[(K1,K2),V](domain, data) with Tensor2[K1,K2,V] {
    override def apply(k1 : K1, k2 : K2) : V = {
      checkKey(k1,k2);
      data.getOrElse((k1,k2),scalar.zero);
    }

    override def update(k1 : K1, k2 : K2, value : V) = {
      checkKey(k1,k2);
      data.update((k1,k2), value);
    }
  }

  implicit def canSliceRow[K1,K2,V:Scalar] : CanSliceRow[Tensor2[K1,K2,V],K1,Tensor1Row[K2,V]]
  = new CanSliceRow[Tensor2[K1,K2,V],K1,Tensor1Row[K2,V]] {
    override def apply(from : Tensor2[K1,K2,V], row : K1) =
      new RowSliceImpl[K1,K2,V,Tensor2[K1,K2,V]](from,row);
  }

  implicit def canSliceCol[K1,K2,V:Scalar] : CanSliceCol[Tensor2[K1,K2,V],K2,Tensor1Col[K1,V]]
  = new CanSliceCol[Tensor2[K1,K2,V],K2,Tensor1Col[K1,V]] {
    override def apply(from : Tensor2[K1,K2,V], col : K2) =
      new ColSliceImpl[K1,K2,V,Tensor2[K1,K2,V]](from, col);
  }

  implicit def canSliceMatrix[K1,K2,V:Scalar]
  : CanSliceMatrix[Tensor2[K1,K2,V],K1,K2,Matrix[V]]
  = new CanSliceMatrix[Tensor2[K1,K2,V],K1,K2,Matrix[V]] {
    override def apply(from : Tensor2[K1,K2,V], keys1 : Seq[K1], keys2 : Seq[K2]) =
      new MatrixSliceImpl[K1,K2,V,Tensor2[K1,K2,V]](from, keys1, keys2);
  }

  trait RowSliceLike[K1,K2,V,+Coll<:Tensor2[K1,K2,V],+This<:RowSlice[K1,K2,V,Coll]]
  extends Tensor1SliceLike[(K1,K2),Domain2[K1,K2],K2,Domain1[K2],V,Coll,This] with Tensor1RowLike[K2,V,Domain1[K2],This] {
    def row : K1;
    override def domain = underlying.domain._2;
    override def lookup(key : K2) = (row,key);
  }

  trait RowSlice[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  extends Tensor1Slice[(K1,K2),K2,V,Coll] with Tensor1Row[K2,V] with RowSliceLike[K1,K2,V,Coll,RowSlice[K1,K2,V,Coll]];

  class RowSliceImpl[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  (override val underlying : Coll, override val row : K1)
  (implicit override val scalar : Scalar[V])
  extends RowSlice[K1,K2,V,Coll];

  trait ColSliceLike[K1,K2,V,+Coll<:Tensor2[K1,K2,V],+This<:ColSlice[K1,K2,V,Coll]]
  extends Tensor1SliceLike[(K1,K2),Domain2[K1,K2],K1,Domain1[K1],V,Coll,This] with Tensor1ColLike[K1,V,Domain1[K1],This] {
    def col : K2;
    override def domain = underlying.domain._1;
    override def lookup(key : K1) = (key,col);
  }

  trait ColSlice[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  extends Tensor1Slice[(K1,K2),K1,V,Coll] with Tensor1Col[K1,V] with ColSliceLike[K1,K2,V,Coll,ColSlice[K1,K2,V,Coll]];

  class ColSliceImpl[K1,K2,V,+Coll<:Tensor2[K1,K2,V]]
  (override val underlying : Coll, override val col : K2)
  (implicit override val scalar : Scalar[V])
  extends ColSlice[K1,K2,V,Coll];

  trait MatrixSliceLike
  [@specialized(Int) K1, @specialized(Int) K2,
   @specialized(Int,Long,Float,Double,Boolean) V,
   +D1<:Domain1[K1],
   +D2<:Domain1[K2],
   +D<:Domain2[K1,K2],
   +T<:Domain2[K2,K1],
   +Coll<:Tensor2[K1,K2,V],
   +This<:MatrixSlice[K1,K2,V,Coll]]
  extends TensorSliceLike[(K1,K2),D,(Int,Int),TableDomain,V,Coll,This]
  with MatrixLike[V,This] {

    def lookup1(i : Int) : K1;
    def lookup2(j : Int) : K2;

    /* final */ override def lookup(tup : (Int,Int)) =
      (lookup1(tup._1), lookup2(tup._2));

    override def apply(i : Int, j : Int) : V =
      underlying.apply(lookup1(i), lookup2(j));

    override def update(i : Int, j : Int, value : V) =
      underlying.update(lookup1(i), lookup2(j), value);
  }

  trait MatrixSlice
  [@specialized(Int,Long) K1, @specialized(Int,Long) K2,
   @specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Tensor2[K1,K2,V]]
  extends TensorSlice[(K1,K2),(Int,Int),V,Coll]
  with Matrix[V]
  with MatrixSliceLike[K1,K2,V,Domain1[K1],Domain1[K2],Domain2[K1,K2],Domain2[K2,K1],Coll,MatrixSlice[K1,K2,V,Coll]];

  class MatrixSliceImpl[K1, K2, V, +Coll<:Tensor2[K1,K2,V]]
  (override val underlying : Coll, val keys1 : Seq[K1], val keys2 : Seq[K2])
  (implicit override val scalar : Scalar[V])
  extends MatrixSlice[K1, K2, V, Coll] {
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }
}

