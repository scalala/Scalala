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
package scalala;
package library;

import tensor.{Matrix, Vector}
import tensor.dense.{DenseVector, DenseMatrix}

import scalala.generic.CanMul;
import scalala.generic.collection.CanViewAsVector;
import scalala.scalar.Scalar;
import scalala.tensor.domain.TableDomain;

import org.netlib.lapack._;
import org.netlib.util.intW;

/**
 * Basic linear algebraic operations.
 *
 * @author dlwh,dramage,retronym
 */
trait LinearAlgebra {
  /**
   * Eigenvalue decomposition (right eigenvectors)
   *
   * This function returns the real and imaginary parts of the eigenvalues,
   * and the corresponding eigenvectors.  For most (?) interesting matrices,
   * the imaginary part of all eigenvalues will be zero (and the corresponding
   * eigenvectors will be real).  Any complex eigenvalues will appear in
   * complex-conjugate pairs, and the real and imaginary components of the
   * eigenvector for each pair will be in the corresponding columns of the
   * eigenvector matrix.  Take the complex conjugate to find the second
   * eigenvector.
   *
   * Based on EVD.java from MTJ 0.9.12
   */
  def eig(m : Matrix[Double]): (DenseVector[Double], DenseVector[Double], DenseMatrix[Double]) = {
    require(m.numRows == m.numCols, "Matrix is not square!");
    requireMatrixNonEmpty(m);

    val n = m.numRows;

    // Allocate space for the decomposition
    var Wr = DenseVector.zeros[Double](n);
    var Wi = DenseVector.zeros[Double](n);

    var Vr = DenseMatrix.zeros[Double](n,n);

    // Find the needed workspace
    val worksize = Array.ofDim[Double](1);
    val info = new intW(0);
    LAPACK.getInstance.dgeev(
      "N", "V", n,
      Array.empty[Double], math.max(1,n),
      Array.empty[Double], Array.empty[Double],
      Array.empty[Double], math.max(1,n),
      Array.empty[Double], math.max(1,n),
      worksize, -1, info);

    // Allocate the workspace
    val lwork: Int = if (info.`val` != 0)
      math.max(1,4*n);
    else
      math.max(1,worksize(0).toInt);

    val work = Array.ofDim[Double](lwork);

    // Factor it!

    val A = new DenseMatrix(n,n,Array.ofDim[Double](n*n));
    A := m
    LAPACK.getInstance.dgeev(
      "N", "V", n,
      A.data, math.max(1,n),
      Wr.data, Wi.data,
      Array.empty[Double], math.max(1,n),
      Vr.data, math.max(1,n),
      work, work.length, info);

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Reason.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (Wr, Wi, Vr)
  }

  class NotConvergedException (val reason: NotConvergedException.Reason.Value, msg: String = "")
          extends Exception(msg) { }

  object NotConvergedException {
    object Reason extends Enumeration {
      val Iterations, Divergence, Breakdown = Value
    }
  }

  /**
   * Computes the SVD of a m by n matrix
   * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
   */
  def svd(mat: DenseMatrix[Double]):(DenseMatrix[Double],DenseVector[Double],DenseMatrix[Double]) = {
    requireMatrixNonEmpty(mat)
    val m = mat.numRows;
    val n = mat.numCols;
    val S = DenseVector.zeros[Double](m min n);
    val U = DenseMatrix.zeros[Double](m,m);
    val Vt = DenseMatrix.zeros[Double](n,n);
    val iwork = new Array[Int](8 * (m min n) );
    val workSize = ( 3
                    * math.min(m, n)
                    * math.min(m, n)
                    + math.max(math.max(m, n), 4 * math.min(m, n)
                               * math.min(m, n) + 4 * math.min(m, n))
                   );
    val work = new Array[Double](workSize);
    val info = new intW(0);
    LAPACK.getInstance.dgesdd(
      "A", m, n,
      mat.copy.data, math.max(1,m),
      S.data, U.data, math.max(1,m),
      Vt.data, math.max(1,n),
      work,work.length,iwork, info);

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Reason.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (U,S,Vt);
  }


  private def requireMatrixNonEmpty[V](mat: Matrix[V]): Unit = {
    require(mat.numCols > 0, "Matrix is empty")
    require(mat.numRows > 0, "Matrix is empty")
  }

  /**
   * Returns the Kronecker product of the two matrices a and b,
   * usually denoted a ⊗ b.
   */
  def kron[V1,V2,RV](a : Matrix[V1], b : Matrix[V2])(implicit mul : CanMul[V1,V2,RV], s : Scalar[RV]) : Matrix[RV] = {
    val builder = a.newBuilder[(Int,Int),RV](TableDomain(a.numRows * b.numRows, a.numCols * b.numCols));
    a.foreachNonZero((ai,aj,av) => b.foreachNonZero((bi,bj,bv) =>
      builder((ai * b.numRows + bi, aj * b.numCols + bj)) = mul(av, bv)));
    builder.result.asInstanceOf[Matrix[RV]];
  }
  
  /**
   * Returns the rank of each element in the given vector, adjusting for
   * ties.
   */
  def ranks[X,V](x : X)(implicit xvt : CanViewAsVector[X,V], ord : Ordering[V]) : Array[Double] = {
    val a = xvt(x);
    val as = a.argsort;
    val rv = new Array[Double](as.length);
    var i = 0;
    while (i < as.length) {
      // count number of tied values at rank i
      var numTiedValuesAtI = 1;
      while (i + numTiedValuesAtI < as.length && a(as(i + numTiedValuesAtI)) == a(as(i))) {
        numTiedValuesAtI += 1;
      }
      
      // set return value for next numTiedValuesAtI indexes in as
      val rank = 1 + i + (numTiedValuesAtI - 1) / 2.0;
      var j = 0;
      while (j < numTiedValuesAtI) {
        rv(as(i + j)) = rank;
        j += 1;
      }
      
      i += numTiedValuesAtI;
    }
    
    rv;
  }
}

object LinearAlgebra extends LinearAlgebra { }

