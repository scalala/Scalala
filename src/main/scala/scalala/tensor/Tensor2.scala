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

import domain._;
import generic.tensor._;

/**
 * Implementation trait for domain maps indexed by two keys.
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
extends TensorLike[(A1,A2),B,D,This] {
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
  def apply[That](i : Seq[A1], j : Seq[A2])
  (implicit bf : CanSliceMatrix[This,A1,A2,B,That]) : That =
    bf.apply(repr, i, j);

  /** Transpose this Tensor2. */
  def transpose[That]
  (implicit bf : CanTranspose[This,A1,A2,B,That]) : That =
    bf.apply(repr);
}

trait Tensor2
[@specialized(Int) A1, @specialized(Int) A2,
 @specialized(Int,Long,Float,Double,Boolean) B]
extends Tensor[(A1,A2),B]
with Tensor2Like[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Tensor2[A1,A2,B]]

object Tensor2 {
  implicit def canSliceMatrix[A1,A2,B:Scalar] = new CanSliceMatrix
  [Tensor2[A1,A2,B],A1,A2,B,MatrixSlice[A1,A2,B,Tensor2[A1,A2,B]]] {
    override def apply(from : Tensor2[A1,A2,B], keys1 : Seq[A1], keys2 : Seq[A2]) =
      new MatrixSlice.FromKeySeqs[A1,A2,B,Tensor2[A1,A2,B]](from, keys1, keys2);
  }

  implicit def canTranspose[A2,A1,B:Scalar] = new CanTranspose
  [Tensor2[A1,A2,B],A1,A2,B,Tensor2Transpose[A2,A1,B,Tensor2[A1,A2,B]]] {
    override def apply(input : Tensor2[A1,A2,B]) =
      new Tensor2Transpose.Impl[A2,A1,B,Tensor2[A1,A2,B]](input);
  }
}
