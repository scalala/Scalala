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

import domain.IndexDomain;
import generic.TensorBuilder;

import scalala.generic.collection.{CanSliceCol};
import scalala.operators._;

/**
 * Implementation trait for a row vector.
 *
 * @author dramage
 */
trait VectorRowLike[@specialized(Int,Long,Float,Double) V, +This<:VectorRow[V]]
extends VectorLike[V,This] with Tensor1RowLike[Int,V,IndexDomain,This] {

  // TODO: improve this method to make it more Vector-like
  override def toString = {
    val rv = valuesIterator.take(10).map(mkValueString).mkString(" ");
    if (size > 10) {
      rv + " " + "... ("+(size-10) +" more)";
    } else {
      rv;
    }
  }
  
  override def t : VectorCol[V] =
    new VectorCol.View[V](repr);
}

/**
 * A vector shaped as a row.
 *
 * @author dramage
 */
trait VectorRow[@specialized(Int,Long,Float,Double) V]
extends Vector[V] with Tensor1Row[Int,V] with VectorRowLike[V,VectorRow[V]];

object VectorRow {
  class View[V](override val inner : Vector[V])
  extends VectorProxy[V,Vector[V]] with VectorRow[V]
  with VectorLike[V,View[V]] {
    override def repr : View[V] = this;
  }
}

