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
package dense;

import generic.collection._;

import domain.TableDomain;

/**
 * A DenseMatrix is backed by an array of doubles, with each column
 * stored before the next column begins.
 *
 * @author dramage
 */
class DenseMatrix[@specialized(Int,Long,Float,Double) B]
(numRows : Int, numCols : Int, override val data : Array[B])
(implicit override val scalar : Scalar[B])
extends DenseArrayTensor[(Int,Int),B] with DenseArrayTensorLike[(Int,Int),B,TableDomain,DenseMatrix[B]]
with mutable.Matrix[B] with mutable.MatrixLike[B,DenseMatrix[B]] {
  if (numRows * numCols != data.length)
    throw new IllegalArgumentException("data.length must equal numRows*numCols");

  override val domain = TableDomain(numRows, numCols);

  final def index(row : Int, col : Int) : Int = {
    checkKey(row,col);
    row + col * numRows;
  }

  final def unindex(index : Int) : (Int,Int) =
    (rowIndex(index), colIndex(index));

  final def rowIndex(index : Int) : Int =
    (index % numRows);

  final def colIndex(index : Int) : Int =
    (index / numRows);

  override def apply(row : Int, col : Int) =
    data(index(row,col));

  override def update(row : Int, col : Int, value : B) =
    data(index(row,col)) = value;

  override def foreach[U](f : (Int,Int,B)=>U) = {
    var i = 0;
    while (i < data.length) {
      f(rowIndex(i),colIndex(i),data(i));
      i += 1;
    }
  }

  override def foreachValue[U](f : (B)=>U) = {
    var i = 0;
    while (i < data.length) {
      f(data(i));
      i += 1;
    }
  }

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transform(f : (Int,Int,B)=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(rowIndex(i),colIndex(i),data(i));
      i += 1;
    }
  }
}

object DenseMatrix {
  /**
   * Static constructor that creates a dense matrix of the given size
   * initialized by iterator from the given values list (looping if
   * necessary).  The values are initialized column-major, i.e. values
   * is read in order to populate the matrix, filling up column 0 before
   * column 1, before column 2 ...
   */
  def apply[B:Scalar:ClassManifest](rows : Int, cols : Int)(values : B*) = {
    new DenseMatrix(rows, cols, Array.tabulate(rows * cols)(i => values(i % values.length)));
  }

  /** Tabulate a matrix from a function from row,col position to value. */
  def tabulate[B:Scalar:ClassManifest](rows : Int, cols : Int)(fn : (Int, Int) => B) = {
    new DenseMatrix(rows, cols, Array.tabulate(rows * cols)(i => fn(i % rows, i / rows)));
  }

  //
  // Capabilities
  //

  class DenseMatrixCanMapValues[@specialized(Int,Long,Float,Double) B, @specialized(Int,Long,Float,Double) R:ClassManifest:Scalar]
  extends CanMapValues[DenseMatrix[B],B,R,DenseMatrix[R]] {
    override def apply(from : DenseMatrix[B], fn : (B=>R)) = {
      val data = new Array[R](from.data.length);
      var i = 0;
      while (i < data.length) {
        data(i) = fn(from.data(i));
        i += 1;
      }
      new DenseMatrix[R](from.numRows, from.numCols, data);
    }
  }

  class DenseMatrixCanMapKeyValuePairs[@specialized(Int,Long,Float,Double) B, @specialized(Int,Long,Float,Double) R:ClassManifest:Scalar]
  extends CanMapKeyValuePairs[DenseMatrix[B],(Int,Int),B,R,DenseMatrix[R]] {
    override def apply(from : DenseMatrix[B], fn : (((Int,Int),B)=>R)) = {
      val data = new Array[R](from.data.length);
      var i = 0;
      while (i < data.length) {
        data(i) = fn(from.unindex(i), from.data(i));
        i += 1;
      }
      new DenseMatrix(from.numRows, from.numCols, data);
    }
  }

  implicit def mkDenseMatrixCanMapValues[B,R:ClassManifest:Scalar] =
    new DenseMatrixCanMapValues[B,R];
  
  implicit def mkDenseMatrixCanMapKeyValuePairs[B,R:ClassManifest:Scalar] =
    new DenseMatrixCanMapKeyValuePairs[B,R];
  
  implicit object DenseMatrixCanMapValuesDD extends DenseMatrixCanMapValues[Double,Double];
  implicit object DenseMatrixCanMapValuesII extends DenseMatrixCanMapValues[Int,Int];
  implicit object DenseMatrixCanMapValuesID extends DenseMatrixCanMapValues[Int,Double];
}
