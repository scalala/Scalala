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
import scalar.Scalar;

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

object DenseMatrix extends mutable.MatrixCompanion[DenseMatrix] with DenseMatrixConstructors {

  //
  // Capabilities
  //

  class DenseMatrixCanMapValues[@specialized(Int,Long,Float,Double) B, @specialized(Int,Long,Float,Double) R:ClassManifest:Scalar]
  extends CanMapValues[DenseMatrix[B],B,R,DenseMatrix[R]] {
    override def map(from : DenseMatrix[B], fn : (B=>R)) = {
      val data = new Array[R](from.data.length);
      var i = 0;
      while (i < data.length) {
        data(i) = fn(from.data(i));
        i += 1;
      }
      new DenseMatrix[R](from.numRows, from.numCols, data);
    }

    override def mapNonZero(from : DenseMatrix[B], fn : (B=>R)) =
      map(from, fn);
  }

  class DenseMatrixCanMapKeyValuePairs[@specialized(Int,Long,Float,Double) B, @specialized(Int,Long,Float,Double) R:ClassManifest:Scalar]
  extends CanMapKeyValuePairs[DenseMatrix[B],(Int,Int),B,R,DenseMatrix[R]] {
    override def map(from : DenseMatrix[B], fn : (((Int,Int),B)=>R)) = {
      val data = new Array[R](from.data.length);
      var i = 0;
      while (i < data.length) {
        data(i) = fn(from.unindex(i), from.data(i));
        i += 1;
      }
      new DenseMatrix(from.numRows, from.numCols, data);
    }
    
    override def mapNonZero(from : DenseMatrix[B], fn : (((Int,Int),B)=>R)) =
      map(from, fn);
  }

  implicit def mkDenseMatrixCanMapValues[B,R:ClassManifest:Scalar] =
    new DenseMatrixCanMapValues[B,R];
  
  implicit def mkDenseMatrixCanMapKeyValuePairs[B,R:ClassManifest:Scalar] =
    new DenseMatrixCanMapKeyValuePairs[B,R];
  
  implicit object DenseMatrixCanMapValuesDD extends DenseMatrixCanMapValues[Double,Double];
  implicit object DenseMatrixCanMapValuesII extends DenseMatrixCanMapValues[Int,Int];
  implicit object DenseMatrixCanMapValuesID extends DenseMatrixCanMapValues[Int,Double];
}

/**
 * Constructors for dense matrices.
 * 
 * @author dramage
 */
trait DenseMatrixConstructors {
  /** Constructs a dense matrix for the given table domain. */
  def apply[V:Scalar](domain : TableDomain) =
    zeros[V](domain._1.size, domain._2.size);

  /** Static constructor for a literal matrix. */
  def apply[R,V](rows : R*)(implicit rl : RowLiteral[R,V], scalar : Scalar[V]) = {
    val nRows = rows.length;
    val nCols = rl.length(rows(0));
    val rv = zeros(nRows, nCols);
    for ((row,i) <- rows.zipWithIndex) {
      rl.foreach(row, ((j, v) => rv(i,j) = v));
    }
    rv;
  }

  /** Creates a dense matrix of the given value repeated of the requested size. */
  def fill[V:Scalar](rows : Int, cols : Int)(value : V) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseMatrix[V](rows, cols, Array.fill(rows * cols)(value));
  }

  /** Creates a dense matrix of zeros of the requested size. */
  def zeros[V:Scalar](rows : Int, cols : Int) =
    fill(rows, cols)(implicitly[Scalar[V]].zero);

  /** Creates a dense matrix of zeros of the requested size. */
  def ones[V:Scalar](rows : Int, cols : Int) =
    fill(rows, cols)(implicitly[Scalar[V]].one);

  /** Creates an identity matrix with size rows and columsn. */
  def eye[V](size : Int)(implicit scalar : Scalar[V]) = {
    val rv = zeros(size, size);
    for (i <- 0 until size) {
      rv(i,i) = scalar.one;
    }
    rv;
  }

  /** Tabulate a matrix from a function from row,col position to value. */
  def tabulate[V:Scalar](rows : Int, cols : Int)(fn : (Int, Int) => V) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseMatrix(rows, cols, Array.tabulate(rows * cols)(i => fn(i % rows, i / rows)));
  }
}
