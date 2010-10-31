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
  def apply[A1, A2, B:Scalar]
  (domain : Product2Domain[A1,A2], map : scala.collection.Map[(A1,A2),B] = new scala.collection.mutable.HashMap[(A1,A2),B]) =
    new Impl[A1,A2,B](map, domain);

  class Impl[A1, A2, B]
  (protected var map : scala.collection.Map[(A1,A2),B], override val domain : Product2Domain[A1,A2])
  (implicit override val scalar : Scalar[B])
  extends Tensor2[A1,A2,B] {
    override def apply(k1 : A1, k2 : A2) : B = {
      checkKey(k1,k2);
      map.getOrElse((k1,k2),scalar.zero);
    }

    override def update(k1 : A1, k2 : A2, value : B) = {
      checkKey(k1,k2);
      map = map.updated((k1,k2), value);
    }
  }

  implicit def canSliceMatrix[A1, A2, B:Scalar] =
  new CanSliceMatrix[Tensor2[A1,A2,B],A1,A2,MatrixSlice[A1,A2,B,Tensor2[A1,A2,B]]] {
    override def apply(from : Tensor2[A1,A2,B], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new MatrixSlice.FromKeySeqs[A1,A2,B,Tensor2[A1,A2,B]](from, keys1, keys2);
  }

  implicit def canTranspose[A2, A1, B:Scalar] =
  new CanTranspose[Tensor2[A1,A2,B],Tensor2Transpose[A2,A1,B,Tensor2[A1,A2,B]]] {
    override def apply(input : Tensor2[A1,A2,B]) =
      new Tensor2Transpose.Impl[A2,A1,B,Tensor2[A1,A2,B]](input);
  }
}
