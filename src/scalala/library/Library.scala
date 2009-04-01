/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.library;

import scalala.collection.domain.{Domain,Domain2};
import scalala.tensor.{Tensor,Vector,Matrix};
import scalala.tensor.dense.{DenseVector,DenseMatrix};

/**
 * Where a function shares its name with a Matlab function, the
 * behavior should be more or less consist with its Matlab
 * counterpart.
 * 
 * The primary difference is that some commands that normally return an
 * n x n square matrix now return a column vector of size n. e.g.
 * ones(n) here is a vector that would be ones(n,1) in matlab.  The
 * main reason for the distinction is that Vector and Matrix are
 * different types in MTJ, so ones(n,1) returns a Matrix of size n by 1
 * which shouldn't need its own conversion back to a vector.
 */
trait Library {
  def Vector(values : Array[Double]) : Vector =
    new DenseVector(values);
  
  def Vector(values : Double*) : Vector =
    new DenseVector(values.toArray);
  
  def DenseVector(size : Int) : Vector =
    new DenseVector(size);
  
  def DenseVector(values : {def toArray : Array[Double]}) : Vector =
    new DenseVector(values.toArray);
  
  def DenseVector[T<:AnyVal](values : Seq[T])(implicit manifest : scala.reflect.Manifest[T]) : Vector = {
    import scalala.tensor.TensorImplicits._;
    val x = new DenseVector(values.size);
    x := values;
    x;
  }
  
  def Matrix(values : Array[Double], rows : Int, cols : Int) : Matrix =
    new DenseMatrix(values, rows, cols);
  
  def DenseMatrix(rows : Int, cols : Int) : Matrix =
    new DenseMatrix(rows,cols);
  
  def DiagonalMatrix(diagonal : Vector) : Matrix = {
    new Matrix {
      override def rows = diagonal.size;
      override def cols = diagonal.size;
      override def apply(row : Int, col : Int) = 
        if (row == col) diagonal(row) else 0.0;
      override def update(row : Int, col : Int, value : Double) = {
        if (row == col) {
          diagonal(row) = value;
        } else {
          throw new UnsupportedOperationException();
        }
      }
      override def activeDomain = diagonal.activeDomain.map(x => (x,x));
      override def copy = DiagonalMatrix(diagonal.copy).asInstanceOf[this.type];
      override def create[J](d : Domain[J]) = diagonal.create(d);
    }
  }
  
  /*
  def ScalarMatrix(value : Double, rows : Int, cols : Int) : Matrix = {
    val _rows = rows;
    val _cols = cols;
    new Matrix {
      override def rows = _rows;
      override def cols = _cols;
      override def get(row : Int, col : Int) = value;
      override def set(row : Int, col : Int, value : Double) =
        throw new UnsupportedOperationException();
      override def copy = this;
    }
  }
  
  def ColMatrix(vector : Vector) : Matrix = {
    new Matrix {
      override def rows = vector.size;
      override def cols = 1;
      override def get(row : Int, col : Int) = {
        check(row,col);
        vector(row);
      }
      override def set(row : Int, col : Int, value : Double) = {
        check(row,col);
        vector(row) = value;
      }
      override def copy = ColMatrix(vector.copy).asInstanceOf[this.type];
    }
  }
  
  def RowMatrix(vector : Vector) : Matrix = {
    new Matrix {
      def rows = 1;
      def cols = vector.size;
      override def get(row : Int, col : Int) = {
        check(row,col);
        vector(col);
      }
      override def set(row : Int, col : Int, value : Double) = {
        check(row,col);
        vector(col) = value;
      }
      override def copy = RowMatrix(vector.copy).asInstanceOf[this.type];
    }
  }
  
  def TransposeMatrix(matrix : Matrix) : Matrix = {
    new Matrix {
      def rows = matrix.cols;
      def cols = matrix.rows;
      override def get(row : Int, col : Int) = matrix(col,row);
      override def set(row : Int, col : Int, value : Double) = matrix(col,row) = value;
      override def copy = TransposeMatrix(matrix.copy).asInstanceOf[this.type];
    }
  }
  
  def BlockMatrix(blocks : Seq[Seq[Matrix]]) : Matrix = {
    throw new UnsupportedOperationException();
  }
  */
  
  //
  // basic scala ops from Math.
  //
  
  /** Log a numeric value */
  @inline def log(v : Double) : Double = Math.log(v);
  
  val NaN = java.lang.Double.NaN;
  
  @inline def isnan(a : Double) : Boolean = java.lang.Double.isNaN(a);

  /** Alias for Math.sqrt. */
  @inline def sqrt(x : Double) = Math.sqrt(x);
}
