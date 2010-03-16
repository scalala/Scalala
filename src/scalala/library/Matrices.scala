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

import scalala.tensor.operators.TensorSelfOp
import scalala.tensor.operators.TensorShapes
import scalala.tensor.{Tensor,Vector,Matrix,DiagonalMatrix};
import scalala.tensor.dense.{DenseVector,DenseMatrix};

/**
 * Basic matrix functions.
 * 
 * @author dramage
 */
trait Matrices extends Library with PartialMaps with Vectors {
  /** A matrix of size m by n with 1 everywhere */
  def ones(rows : Int, cols : Int) =
    DenseMatrix(rows,cols)(1.0);
  
  /** A matrix of size m by n with 0 everywhere */
  def zeros(rows : Int, cols : Int) =
    DenseMatrix(rows,cols)(0.0);
  
  /** Sums the columns of the given matrix, returning a row vector */
  def sum(m : Matrix) : Matrix = sum(m, 1);
  
  /** Sums along the given dimension of the matrix (1 for rows, 2 for cols) */
  def sum(m : Matrix, dim : Int) : Matrix = {
    dim match {
      case 1 => {
        val sum = new DenseMatrix(1, m.cols);
        for (entry <- m.iterator) {
          sum(0, entry._1._2) += entry._2;
        }
        sum;
      }
      case 2 => {
        val sum = new DenseMatrix(m.rows, 1);
        for (entry <- m.iterator) {
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
  
  def vec(m : Matrix) =
    new DenseVector(Array.tabulate(m.rows * m.cols)(i => m(i % m.rows, i / m.rows)));
    
  /** Returns a square diagonal matrix of the given size */
  def diag(n : Int) : DiagonalMatrix =
    return diag(ones(n));

  /** Returns a dense vector with ones on the diagonal. */
  def eye(n : Int) = {
    val rv = new DenseMatrix(n,n);
    rv := diag(ones(n));
    rv;
  }

  /** Concatenates matrices on top of each other. Must have the same column dimension */
  def vertcat(m1:Matrix, matrices: Matrix*):DenseMatrix = {
    require(matrices.forall(_.cols == m1.cols),"Not all matrices have the same column dimension!");
    val newRowSize = matrices.foldLeft(m1.rows)(_+_.rows);
    val ret = new DenseMatrix(newRowSize,m1.cols);
    var start = 0;
    for( m <- Iterator.single(m1) ++ matrices.iterator) {
      for( ((i,j),v) <- m) {
        ret(i+start,j) = v;
      }
      start += m.rows;
    }
    ret
  }

  /** Concatenates matrices next to each other. Must have the same row dimension */
  def horzcat(m1:Matrix, matrices: Matrix*):DenseMatrix = {
    require(matrices.forall(_.rows == m1.rows),"Not all matrices have the same row dimension!");
    val newColumnSize = matrices.foldLeft(m1.cols)(_+_.cols);
    val ret = new DenseMatrix(m1.rows,newColumnSize);
    var start = 0;
    for( m <- Iterator.single(m1) ++ matrices.iterator) {
      for( ((i,j),v) <- m) {
        ret(i,j+start) = v; 
      }
      start += m.cols;
    }
    ret
  }

  /**
   * Returns a vector comprised of elements off the diagonal of the matrix
   */
  def diag(m : Matrix) : Vector = {
    val v = m.vectorLike(m.rows min m.cols);
    for( i <- 0 until v.size) {
      v(i) = m(i,i);
    }
    v
  }

  /*
  import TensorShapes._;
  def sqrt[M<:Matrix with TensorSelfOp[(Int,Int),M,Shape2]](m: M):M = {
    val r = m.like;
    for( (k,v) <- m.activeElements) {
      r(k) = sqrt(v);
    }
    r
  }
  */

  
  /**
   * Returns a diagonal matrix with the given vector on the diagonal.
   * Copies the contents of the underlying matrix.
   */
  def diag(v : Vector) : DiagonalMatrix =
    new DiagonalMatrix(v.copy);

  /**
   * Computes the Eigenvalue decomposition of the matrix.
   * Returns the eigenvectors (in a matrix) V and the eigenvalues in a vector D such that
   *  m * V = V * diag(d)
  def eig(m: DenseMatrix, symmetric:Boolean=false):(DenseMatrix,DenseVector) = {
    require(m.rows == m.cols,"Matrix must be square");
    val n = m.rows;
    val Wr = new Array[Double](n);
    val Wi = new Array[Double](n);
    val Vl = new Array[Double](0);
    val Vr = m.like;
    val work = new Array[Double](4 * n);
    import scalala.tensor.dense.Numerics.lapack._;
    val result = geev(Eigenvalues,EigenvaluesAndVectors,n,m.data,Wr,Wi,Vl,Vr.data,work,work.length);
    if(result != 0) error("Eig failure!");
    val D = new DenseVector(Wr);
    val V = Vr;
    (V,D)
  }
 */

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

/**
 * An object with access to the Matrices trait members.
 * 
 * @author dramage
 */
object Matrices extends Matrices { }
