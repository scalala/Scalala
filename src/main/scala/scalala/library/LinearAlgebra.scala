package scalala.library

import scalala.tensor.{Matrix, Vector}
import scalala.tensor.dense.{DenseVector, Numerics, DenseMatrix}

/**
 * Basic linear algebraic operations
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
  def eig(m : Matrix): Tuple3[DenseVector, DenseVector, DenseMatrix] = {
    val (rows,cols) = m.dimensions;
    require(rows == cols, "Matrix is not square!");

    val n = rows

    // Allocate space for the decomposition
    var Wr = new DenseVector(n)
    var Wi = new DenseVector(n)

    var Vr = new DenseMatrix(n,n)

    // Find the needed workspace
    var worksize = Array.ofDim[Double](1);
    var info = Numerics.lapack.geev(false, true, n, Array.empty[Double], Array.empty[Double],
      Array.empty[Double], Array.empty[Double], Array.empty[Double], worksize, -1 )

    // Allocate the workspace
    val lwork: Int = if (info != 0)
      Math.max(1,4*n)
    else
      Math.max(1,worksize(0).toInt)

    var work = Array.ofDim[Double](lwork)

    // Factor it!

    var A = new DenseMatrix(n,n)
    A := m
    info = Numerics.lapack.geev(false, true, n, A.data, Wr.data, Wi.data, Array.empty[Double],
      Vr.data, work, work.length)

    if (info > 0)
      throw new NotConvergedException(NotConvergedException.Reason.Iterations)
    else if (info < 0)
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
  def svd(mat: DenseMatrix):(DenseMatrix,DenseVector,DenseMatrix) = {
    val m = mat.rows;
    val n = mat.cols;
    val S = new DenseVector(m min n);
    val U = new DenseMatrix(m,m);
    val Vt = new DenseMatrix(n,n);
    val iwork = new Array[Int](8 * (m min n) );
    val workSize = ( 3
                    * Math.min(m, n)
                    * Math.min(m, n)
                    + Math.max(Math.max(m, n), 4 * Math.min(m, n)
                               * Math.min(m, n) + 4 * Math.min(m, n))
                   );
    val work = new Array[Double](workSize);
    import scalala.tensor.dense.Numerics.lapack._;
    val result = gesdd(JobSVD.All,m,n,mat.copy.data,S.data,U.data,Vt.data,work,work.length,iwork);
    if (result > 0)
      throw new NotConvergedException(NotConvergedException.Reason.Iterations)
    else if (result < 0)
      throw new IllegalArgumentException()

    (U,S,Vt);
  }


}


object LinearAlgebra extends LinearAlgebra { }
