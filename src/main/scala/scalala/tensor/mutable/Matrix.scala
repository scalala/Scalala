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
import generic.tensor._;

/**
 * Implementation trait for a MutableTensor that is also a tensor.Matrix.
 *
 * @author dramage
 */
trait MatrixLike
[@specialized(Int,Long,Float,Double,Boolean) B, +This<:Matrix[B]]
extends tensor.MatrixLike[B,This]
with Tensor2Like[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,This];

/**
 * MutableTensor that is also a tensor.Matrix.
 *
 * @author dramage
 */
trait Matrix
[@specialized(Int,Long,Float,Double,Boolean) B]
extends tensor.Matrix[B]
with Tensor2[Int,Int,B]
with MatrixLike[B,Matrix[B]];

object Matrix extends MatrixCompanion[Matrix] {
  import scala.collection.mutable.IndexedSeq;

  class Impl[B](numRows : Int, numCols : Int, val data : IndexedSeq[IndexedSeq[B]])
  (implicit override val scalar : Scalar[B])
  extends Matrix[B] {
    override val domain = TableDomain(numRows, numCols);

    override def apply(i : Int, j : Int) =
      data(i)(j);

    override def update(i : Int, j : Int, v : B) =
      data(i)(j) = v;
  }

  def apply[B](numRows : Int, numCols : Int)(implicit scalar : Scalar[B]) =
    new Impl(numRows, numCols, IndexedSeq.tabulate(numRows)(r => IndexedSeq.fill(numCols)(scalar.zero)));

  implicit def canTranspose[B:Scalar] : CanTranspose[Matrix[B], MatrixTranspose[B,Matrix[B]]] =
  new CanTranspose[Matrix[B], MatrixTranspose[B,Matrix[B]]] {
    override def apply(from : Matrix[B]) = new MatrixTranspose.Impl[B,Matrix[B]](from);
  }
}

trait MatrixCompanion[Bound[V]<:Matrix[V]]
extends tensor.MatrixCompanion[Bound] with IndexedTensorCompanion[(Int,Int),Bound];
