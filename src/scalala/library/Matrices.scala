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
package scalala.library

import scalala.tensor.{Tensor,Vector,Matrix};

/**
 * Basic matrix functions.
 * 
 * @author dramage
 */
trait Matrices extends Library with Vectors {
  /** A matrix of size m by n with 1 everywhere */
  def ones(rows : Int, cols : Int) : Matrix = {
    DenseMatrix(Array.fromFunction(i => 1.0)(rows*cols),rows,cols);
  }
  
  /** A matrix of size m by n with 0 everywhere */
  def zeros(rows : Int, cols : Int) : Matrix = DenseMatrix(rows,cols);
  
  /** Sums the columns of the given matrix, returning a row vector */
  def sum(m : Matrix) : Matrix = sum(m, 1);
  
  /** Sums along the given dimension of the matrix (1 for rows, 2 for cols) */
  def sum(m : Matrix, dim : Int) : Matrix = {
    dim match {
      case 1 => {
        val sum = DenseMatrix(1, m.cols);
        for (entry <- m.elements) {
          sum(0, entry._1._2) += entry._2;
        }
        sum;
      }
      case 2 => {
        val sum = DenseMatrix(m.rows, 1);
        for (entry <- m.elements) {
          sum(entry._1._1, 0) += entry._2;
        }
        sum;
      }
      case _ => {
        throw new IndexOutOfBoundsException
      }
    }
  }
  
  /** The maximum element of a matrix. */
  def max(m : Matrix) : Double =
    max(vec(m));
  
  /** The minimum element of a matrix. */
  def min(m : Matrix) : Double =
    min(vec(m));
  
  
  //
  // Data formatters, mungers, etc
  //
  
  def vec(m : Matrix) : Vector = {
    Vector(Array.fromFunction(i => m(i % m.rows, i / m.rows))(m.rows * m.cols));
  }
    
  /** Returns a square diagonal matrix of the given size */
  def diag(n : Int) : Matrix = {
    return diag(ones(n));
  }
  
  /** Returns a dense vector with ones on the diagonal. */
  def eye(n : Int) : Matrix = {
    val rv = DenseMatrix(n,n);
    rv := diag(ones(n));
    rv;
  } 
  
  /**
   * Returns a diagonal matrix with the given vector on the diagonal.
   * Copies the contents of the underlying matrix.
   */
  def diag(v : Vector) : Matrix =
    DiagonalMatrix(v.copy.asInstanceOf[Vector]);
  
  /**
   * Turns the given matrices into a block diagonal matrix.
   */
  // TODO re-implement
  /*
  def blkdiag(blocks : Seq[Matrix]) : Matrix = {
    def zeros() : Array[Matrix] =
      blocks.map(m => ScalarMatrix(0.0, m.rows, m.cols)).toArray
      
    def row(pos : Int) = {
      val row = zeros();
      row(pos) = blocks(pos);
      row
    }
    
    BlockMatrix((0 until blocks.length).map(row).toArray)
  }
  */
}
