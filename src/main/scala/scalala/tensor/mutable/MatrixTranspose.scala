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

import domain._
import generic.tensor._;

/**
 * A Transpose of any Matrix type is a Matrix.
 *
 * @author dramage
 */
trait MatrixTransposeLike
[@specialized(Int,Long,Float,Double) B, +Coll <: Matrix[B], +This <: MatrixTranspose[B,Coll]]
extends tensor.MatrixTransposeLike[B,Coll,This]
with Tensor2TransposeLike[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,Coll,This]
with MatrixLike[B,This] {
  override def domain = underlying.domain.transpose.asInstanceOf[TableDomain];
}

/**
 * A Transpose of any Matrix type is a Matrix.
 *
 * @author dramage
 */
trait MatrixTranspose[@specialized(Int,Long,Float,Double) B, +Coll <: Matrix[B]]
extends tensor.MatrixTranspose[B,Coll]
with Tensor2Transpose[Int,Int,B,Coll]
with Matrix[B] with MatrixTransposeLike[B, Coll, MatrixTranspose[B, Coll]];

object MatrixTranspose {
  class Impl[B, +Coll <: Matrix[B]]
  (override val underlying : Coll)
  (implicit override val scalar : Scalar[B])
  extends MatrixTranspose[B,Coll];
}
