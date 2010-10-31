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
 * Implementation trait for tensors indexed by two keys, such as Matrices.
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
extends TensorLike[(A1,A2),B,D,This] with operators.MatrixOps[This] {
  def checkKey(k1 : A1, k2 : A2) : Unit = {
    if (!domain._1.contains(k1) || !domain._2.contains(k2)) {
      throw new DomainException((k1,k2)+" not in domain");
    }
  }

  /* final */ override def checkKey(pos : (A1,A2)) : Unit =
    checkKey(pos._1, pos._2);

  /** Gets the value indexed by (i,j). */
  def apply(i : A1, j : A2) : B;

  /** Fixed alias for apply(i,j). */
  /* final */ override def apply(pos : (A1,A2)) : B =
    apply(pos._1, pos._2);

  /** Slice a sub-Tensor2 */
  def apply[TT>:This,That](i : Seq[A1], j : Seq[A2])(implicit bf : CanSliceMatrix[TT,A1,A2,That]) : That =
    bf.apply(repr, i, j);

  def apply[TT>:This,That](i : A1, j : SelectAll)(implicit bf : CanSliceRow[TT,A1,That]) : That =
    bf.apply(repr, i);

  def apply[TT>:This,That](i : SelectAll, j : A2)(implicit bf : CanSliceCol[TT,A2,That]) : That =
    bf.apply(repr, j);

  /** Transpose this Tensor2. */
  def transpose[TT>:This,That]
  (implicit bf : CanTranspose[TT,That]) : That =
    bf.apply(repr);

  /** Tranforms all key value pairs in this map by applying the given function. */
  def foreach[U](fn : (A1,A2,B)=>U) =
    super.foreach((k,v) => fn(k._1, k._2, v));

  /** Fixed alias for transform((k1,k2,v) => f((k1,k2),v)) */
  /* final */ override def foreach[U](f : ((A1,A2),B)=>U) =
    foreach((k1,k2,v) => f((k1,k2),v));

  /** Tranforms all key value pairs in this map by applying the given function. */
  def foreachNonZero[U](fn : (A1,A2,B)=>U) =
    super.foreachNonZero((k,v) => fn(k._1, k._2, v));

  /** Fixed alias for transform((k1,k2,v) => f((k1,k2),v)) */
  /* final */ override def foreachNonZero[U](f : ((A1,A2),B)=>U) =
    foreachNonZero((k1,k2,v) => f((k1,k2),v));

  override protected def canEqual(other : Any) : Boolean = other match {
    case that : Tensor2[_,_,_] => true;
    case _ => false;
  }
}

/**
 * Tensors indexed by two keys, such as matrices.
 *
 * @author dramage
 */
trait Tensor2
[@specialized(Int) A1, @specialized(Int) A2,
 @specialized(Int,Long,Float,Double,Boolean) B]
extends Tensor[(A1,A2),B]
with Tensor2Like[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Tensor2[A1,A2,B]]

object Tensor2 {
  implicit def canSliceMatrix[A1,A2,B:Scalar] = new CanSliceMatrix
  [Tensor2[A1,A2,B],A1,A2,MatrixSlice[A1,A2,B,Tensor2[A1,A2,B]]] {
    override def apply(from : Tensor2[A1,A2,B], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new MatrixSlice.FromKeySeqs[A1,A2,B,Tensor2[A1,A2,B]](from, keys1, keys2);
  }

  implicit def canTranspose[A2,A1,B:Scalar] : CanTranspose[Tensor2[A1,A2,B],Tensor2Transpose[A2,A1,B,Tensor2[A1,A2,B]]]
  = new CanTranspose[Tensor2[A1,A2,B],Tensor2Transpose[A2,A1,B,Tensor2[A1,A2,B]]] {
    override def apply(input : Tensor2[A1,A2,B]) =
      new Tensor2Transpose.Impl[A2,A1,B,Tensor2[A1,A2,B]](input);
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
}
