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
import scalala.tensor.Vector;
import scalala.tensor.dense.DenseVectorCol;
import scalala.tensor.sparse.SparseVector;

/**
 * View something as a Vector.
 *
 * @author dramage
 */
trait CanViewAsVector[-From,V] {
  def apply(from : From) : Vector[V];
}

object CanViewAsVector {
  //
  // View arrays
  //

  class ArrayVector[V:ClassManifest:Scalar]
  extends CanViewAsVector[Array[V],V] {
    def apply(from : Array[V]) = new DenseVectorCol[V](from);
  }

  implicit def mkArrayVector[V:ClassManifest:Scalar] =
    new ArrayVector[V]();

  implicit object ArrayI extends ArrayVector[Int];
  implicit object ArrayS extends ArrayVector[Short];
  implicit object ArrayL extends ArrayVector[Long];
  implicit object ArrayF extends ArrayVector[Float];
  implicit object ArrayD extends ArrayVector[Double];
  implicit object ArrayB extends ArrayVector[Boolean];

  //
  // View sparse arrays
  //

  class SparseArrayVector[V:ClassManifest:Scalar]
  extends CanViewAsVector[SparseArray[V],V] {
    def apply(from : SparseArray[V]) = new SparseVector[V](from);
  }

  implicit def mkSparseArrayVector[V:ClassManifest:Scalar] =
    new SparseArrayVector[V]();

  implicit object SparseArrayI extends SparseArrayVector[Int];
  implicit object SparseArrayS extends SparseArrayVector[Short];
  implicit object SparseArrayL extends SparseArrayVector[Long];
  implicit object SparseArrayF extends SparseArrayVector[Float];
  implicit object SparseArrayD extends SparseArrayVector[Double];
  implicit object SparseArrayB extends SparseArrayVector[Boolean];

  //
  // View pre-constructed Vector instances
  //

  class VectorVector[V:Scalar]
  extends CanViewAsVector[Vector[V],V] {
    def apply(from : Vector[V]) = from;
  }

  implicit def mkVectorVector[V:Scalar] =
    new VectorVector[V]();
}

