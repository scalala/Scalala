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

import scalala.collection.sparse.SparseArray;

import scalala.scalar.Scalar;
import scalala.tensor.Tensor1;
import scalala.tensor.dense.DenseVector;
import scalala.tensor.sparse.SparseVector;

/**
 * View something as a Tensor1.
 *
 * @author dramage
 */
trait CanViewAsTensor1[-From,K,V] {
  def apply(from : From) : Tensor1[K,V];
}

object CanViewAsTensor1 {
  //
  // View arrays
  //

  class ArrayTensor1[V:ClassManifest:Scalar]
  extends CanViewAsTensor1[Array[V],Int,V] {
    def apply(from : Array[V]) = new DenseVector[V](from);
  }

  implicit def mkArrayTensor1[V:ClassManifest:Scalar] =
    new ArrayTensor1[V]();

  implicit object ArrayI extends ArrayTensor1[Int];
  implicit object ArrayS extends ArrayTensor1[Short];
  implicit object ArrayL extends ArrayTensor1[Long];
  implicit object ArrayF extends ArrayTensor1[Float];
  implicit object ArrayD extends ArrayTensor1[Double];
  implicit object ArrayB extends ArrayTensor1[Boolean];

  //
  // View sparse arrays
  //

  class SparseArrayTensor1[V:ClassManifest:Scalar]
  extends CanViewAsTensor1[SparseArray[V],Int,V] {
    def apply(from : SparseArray[V]) = new SparseVector[V](from);
  }

  implicit def mkSparseArrayTensor1[V:ClassManifest:Scalar] =
    new SparseArrayTensor1[V]();

  implicit object SparseArrayI extends SparseArrayTensor1[Int];
  implicit object SparseArrayS extends SparseArrayTensor1[Short];
  implicit object SparseArrayL extends SparseArrayTensor1[Long];
  implicit object SparseArrayF extends SparseArrayTensor1[Float];
  implicit object SparseArrayD extends SparseArrayTensor1[Double];
  implicit object SparseArrayB extends SparseArrayTensor1[Boolean];

  //
  // View pre-constructed Tensor1 instances
  //

  class Tensor1Tensor1[K,V:Scalar]
  extends CanViewAsTensor1[Tensor1[K,V],K,V] {
    def apply(from : Tensor1[K,V]) = from;
  }

  implicit def mkTensor1Tensor1[K,V:Scalar] =
    new Tensor1Tensor1[K,V]();
}
