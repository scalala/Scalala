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
package generic;
package collection;

import scalala.tensor.domain.CanGetDomain;
import scalala.tensor.generic.TensorBuilder;
import scalala.tensor.Tensor;

/**
 * Trait for building a new tensor from either A or B depending on the
 * given Op.  Default to building tensor for left operand.
 *
 * @author dramage
 */
trait CanBuildTensorForBinaryOp[-A, -B, Op, K, V, +To] {
  def apply(a : A, b : B) : TensorBuilder[K,V,To];
}

object CanBuildTensorForBinaryOp {
  implicit def canBuildTensorLeft[A,B,D,Op,K,V,To]
  (implicit va : A=>Tensor[K,_],
   df : CanGetDomain[A,D],
   bf : CanBuildTensorFrom[A,D,K,V,To])
  : CanBuildTensorForBinaryOp[A,B,Op,K,V,To]
  = new CanBuildTensorForBinaryOp[A,B,Op,K,V,To] {
    def apply(a : A, b : B)  = bf.apply(a, a.domain.asInstanceOf[D]);
  }
}

