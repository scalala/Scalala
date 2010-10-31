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
import generic.{CanAdd,CanMul,CanMulColumnBy};
import generic.collection.{CanTranspose};

/**
 * Implementation trait for a row vector.
 *
 * @author dramage
 */
trait VectorColLike[@specialized(Int,Long,Float,Double) B, +This<:VectorCol[B]]
extends VectorLike[B,This] with Tensor1ColLike[Int,B,IndexDomain,This];

/**
 * A vector shaped as a row.
 *
 * @author dramage
 */
trait VectorCol[@specialized(Int,Long,Float,Double) B]
extends Vector[B] with Tensor1Col[Int,B] with VectorColLike[B,VectorCol[B]];

object VectorCol extends VectorColCompanion[VectorCol] {
  implicit def canTranspose[V] : CanTranspose[VectorCol[V],VectorRow[V]]
  = new CanTranspose[VectorCol[V],VectorRow[V]] {
    override def apply(col : VectorCol[V]) =
      new VectorRow.View[V](col);
  }

  class View[V](override val inner : VectorRow[V])
  extends VectorProxy[V,VectorRow[V]] with VectorCol[V]
  with VectorLike[V,View[V]] {
    override def repr : View[V] = this;
  }
}

trait VectorColCompanion[Bound[V]<:VectorCol[V]] extends VectorCompanion[Bound] {
  implicit def canMulVectorColByRow[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], scalar : Scalar[RV])
  : CanMulColumnBy[Bound[V1],VectorRow[V2],Matrix[RV]]
  = new CanMulColumnBy[Bound[V1],VectorRow[V2],Matrix[RV]] {
    override def apply(a : Bound[V1], b : VectorRow[V2]) = {
      val builder = a.newBuilder(TableDomain(a.domain.size, b.domain.size));
      a.foreachNonZero((i,va) => b.foreachNonZero((j,vb) => builder((i,j)) = mul(va,vb)));
      builder.result.asInstanceOf[Matrix[RV]];
    }
  }
}
