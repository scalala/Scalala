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

import dense.{DenseVector,DenseVectorRow};

/**
 * Implementation trait for a row vector.
 *
 * @author dramage
 */
trait VectorRowLike[@specialized(Int,Long,Float,Double) V, +This<:VectorRow[V]]
extends VectorLike[V,This] with Tensor1RowLike[Int,V,IndexDomain,This] {
  
  override def t : VectorCol[V] =
    new VectorCol.View[V](repr);

  /** Returns a copy of this vector as a DenseVectorRow. */
  override def toDense : DenseVectorRow[V] = {
    val rv = DenseVector.zeros(length).t;
    rv := repr;
    rv;
  }

  def toString(maxWidth : Int = ScalalaConsole.terminalWidth,
               mkValueString : V=>String = buildMkValueString) : String = {
    def colWidth(col : Int) = mkValueString(this(col)).length+2;

    val colWidths = new scala.collection.mutable.ArrayBuffer[Int];
    var col = 0;
    while (col < size && colWidths.sum < maxWidth) {
      colWidths += colWidth(col);
      col += 1;
    }
    // make space for "... (K total)"
    if (colWidths.size < length) {
      while (colWidths.sum + maxWidth.toString.length + 12 >= maxWidth) {
        colWidths.remove(colWidths.length - 1);
      }
    }

    val newline = System.getProperty("line.separator");

    var rv = new StringBuilder;
    for (col <- 0 until colWidths.length) {
      val cell = mkValueString(this(col));
      rv.append(cell);
      rv.append(" " * (colWidths(col) - cell.length));
      if (col == colWidths.length - 1) {
        if (col < size - 1) {
          rv.append("...");
          rv.append(" (");
          rv.append(length);
          rv.append(" total)");
        }
      }
    }
    rv.append(newline);
    rv.toString;
  }

  override def toString =
    toString(maxWidth = ScalalaConsole.terminalWidth,
             mkValueString = buildMkValueString);
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

