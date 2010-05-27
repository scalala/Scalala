package scalala;
package library;

import tensor.{Matrix, Vector}
import tensor.dense.{DenseVector, Numerics, DenseMatrix}

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
    requireMatrixNonEmpty(m)

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
      math.max(1,4*n)
    else
      math.max(1,worksize(0).toInt)

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

  class MatrixNotPositiveDefiniteException(m: Matrix) extends Exception(m.toString);

  /**
   * Computes the SVD of a m by n matrix
   * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
   */
  def svd(mat: DenseMatrix):(DenseMatrix,DenseVector,DenseMatrix) = {
    requireMatrixNonEmpty(mat)
    val m = mat.rows;
    val n = mat.cols;
    val S = new DenseVector(m min n);
    val U = new DenseMatrix(m,m);
    val Vt = new DenseMatrix(n,n);
    val iwork = new Array[Int](8 * (m min n) );
    val workSize = ( 3
                    * math.min(m, n)
                    * math.min(m, n)
                    + math.max(math.max(m, n), 4 * math.min(m, n)
                               * math.min(m, n) + 4 * math.min(m, n))
                   );
    val work = new Array[Double](workSize);
    import tensor.dense.Numerics.lapack._;
    val result = gesdd(JobSVD.All,m,n,mat.copy.data,S.data,U.data,Vt.data,work,work.length,iwork);
    if (result > 0)
      throw new NotConvergedException(NotConvergedException.Reason.Iterations)
    else if (result < 0)
      throw new IllegalArgumentException()

    (U,S,Vt);
  }

  /**
   * Computes the Cholesky decomposition, or "matrix square root" of the square matrix.
   *
   * The matrix returned has the property that L*L.t == m
   */
  def cholesky(m: DenseMatrix) = {
    val r = m.copy;
    val (rows,cols) = r.dimensions;
    require(rows == cols, "Matrix is not square!");

    val ret = scalala.tensor.dense.Numerics.lapack.potrf("L", rows, r.data);
    if(ret > 0) throw new MatrixNotPositiveDefiniteException(m);
    else if (ret < 0) throw new IllegalArgumentException(-ret + "'th argument of " + m);

    // Now zero out the rest.
    for(k <- 1 until cols) {
      java.util.Arrays.fill(r.data, k * rows, k * rows + k,0.0);
    }

    r;
  }

  /**
   * Returns the determinant of the (square) matrix.
   * det(m) == 0 implies that the matrix is singular. Values near zero usually give
   * singularity too.
   *
   * The implementation computes the Cholesky decomposition, and then multiplies
   * the diagnnals. TODO: Cholesky requires positive definite. need to use LU.
   *
   * @author dlwh
   */
  def det(m: DenseMatrix):Double = {
    val c = cholesky(m);
    var sc = 1.0;
    for(i <- 0 until c.dimensions._1) {
      sc *= c(i,i);
    }

    sc
  }

  /**
   * Computes the matrix trace, which is the sum of the
   * elements along the diagonal of the matrix. Requires a square
   * matrix.
   *
   * @author dlwh
   */
  def trace(m: Matrix):Double = {
    require(m.rows == m.cols,"Matrix must be square!");
    var tr = 0.0;
    for( i <- 0 until m.rows) {
      tr += m(i,i);
    }
    tr
  }

  /**
   * Returns the inverse of a DenseMatrix.
   */
  def inv(m: DenseMatrix):DenseMatrix = {
    require(m.rows == m.cols);
    val I = Matrices.eye(m.rows);
    import Operators._;
    I \ m value
  }

  private def requireMatrixNonEmpty(mat: Matrix): Unit = {
    require(mat.cols > 0, "Matrix is empty")
    require(mat.rows > 0, "Matrix is empty")
  }
}


object LinearAlgebra extends LinearAlgebra { }
