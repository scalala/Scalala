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
import generic.collection.CanTranspose;

/**
 * Implementation trait for mutable VectorCol instances.
 *
 * @author dramage
 */
trait VectorColLike[@specialized(Int,Long,Float,Double) V, +This<:VectorCol[V]]
extends tensor.VectorColLike[V,This]
with Tensor1ColLike[Int,V,IndexDomain,This] with VectorLike[V,This];

/**
 * Mutable tensor.VectorCol.
 *
 * @author dramage
 */
trait VectorCol[@specialized(Int,Long,Float,Double) V]
extends tensor.VectorCol[V] with Tensor1Col[Int,V] with Vector[V]
with VectorColLike[V,VectorCol[V]];

object VectorCol extends VectorColCompanion[VectorCol] {
  implicit def canTranspose[V] : CanTranspose[VectorCol[V],VectorRow[V]]
  = new CanTranspose[VectorCol[V],VectorRow[V]] {
    override def apply(col : VectorCol[V]) =
      new VectorRow.View[V](col);
  }

  class View[V](override val inner : VectorRow[V])
  extends VectorProxy[V,VectorRow[V]] with VectorCol[V] with VectorLike[V,View[V]] {
    override def repr : View[V] = this;
  }
}

trait VectorColCompanion[Bound[V]<:VectorCol[V]]
extends tensor.VectorColCompanion[Bound] with VectorCompanion[Bound];
