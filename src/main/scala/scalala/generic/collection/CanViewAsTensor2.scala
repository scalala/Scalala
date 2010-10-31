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

import scalala.scalar.Scalar;
import scalala.tensor.Tensor2;
import scalala.tensor.dense.ArrayArrayMatrix;

/**
 * View something as a Tensor1.
 *
 * @author dramage
 */
trait CanViewAsTensor2[-From,K1,K2,V] {
  def apply(from : From) : Tensor2[K1,K2,V];
}

object CanViewAsTensor2 {
  //
  // View arrays
  //

  class ArrayArrayTensor2[V:ClassManifest:Scalar]
  extends CanViewAsTensor2[Array[Array[V]],Int,Int,V] {
    def apply(from : Array[Array[V]]) = new ArrayArrayMatrix[V](from);
  }

  implicit def mkArrayArrayTensor2[V:ClassManifest:Scalar] =
    new ArrayArrayTensor2[V]();

  implicit object ArrayArrayI extends ArrayArrayTensor2[Int];
  implicit object ArrayArrayS extends ArrayArrayTensor2[Short];
  implicit object ArrayArrayL extends ArrayArrayTensor2[Long];
  implicit object ArrayArrayF extends ArrayArrayTensor2[Float];
  implicit object ArrayArrayD extends ArrayArrayTensor2[Double];
  implicit object ArrayArrayB extends ArrayArrayTensor2[Boolean];

  //
  // View pre-constructed Tensor2 instances
  //

  class Tensor2Tensor2[K1,K2,V:Scalar]
  extends CanViewAsTensor2[Tensor2[K1,K2,V],K1,K2,V] {
    def apply(from : Tensor2[K1,K2,V]) = from;
  }

  implicit def mkTensor2Tensor2[K1,K2,V:Scalar] =
    new Tensor2Tensor2[K1,K2,V]();
}
