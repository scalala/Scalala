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

/**
 * Tensors indexed by a sequence of keys.
 *
 * @author dramage
 */
trait TensorNLike[@specialized(Int) K, @specialized(Int,Long,Float,Double,Boolean) V, +This<:TensorN[K,V]]
extends TensorLike[Seq[K],V,ProductNDomain[K],This] {
  /** Gets the value indexed by (i,j). */
  /* final */ def apply(k : K*) : V =
    apply(k : Seq[K]);


  override protected def canEqual(other : Any) : Boolean = other match {
    case that : TensorN[_,_] => true;
    case _ => false;
  }
}

trait TensorN[@specialized(Int) K, @specialized(Int,Long,Float,Double,Boolean) V]
extends Tensor[Seq[K],V] with TensorNLike[K,V,TensorN[K,V]]

object TensorN;
