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

import org.netlib.lapack._
import org.netlib.util.intW

import operators._
import scalar.Scalar
import generic.collection.CanViewAsVector
import tensor.domain.TableDomain
import tensor.dense.{DenseVector, DenseMatrix}
import tensor.{DiagonalMatrix, MatrixSingularException, Matrix, Vector}


/**
 * Basic linear algebraic operations.
 *
 * @author dlwh,dramage,retronym,afwlehmann,lancelet
 */
trait LinearAlgebra {

  @inline private def requireNonEmptyMatrix[V](mat: Matrix[V]) =
    if (mat.numCols == 0 || mat.numRows == 0)
      throw new MatrixEmptyException

  @inline private def requireSquareMatrix[V](mat: Matrix[V]) =
    if (mat.numRows != mat.numCols)
      throw new MatrixNotSquareException

  @inline private def requireSymmetricMatrix[V](mat: Matrix[V]) = {
    requireSquareMatrix(mat)

    for (i <- 0 until mat.numRows; j <- 0 until i)
      if (mat(i,j) != mat(j,i))
        throw new MatrixNotSymmetricException
  }

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
    requireNonEmptyMatrix(m)
    requireSquareMatrix(m)

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
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (Wr, Wi, Vr)
  }

  /**
   * Computes the SVD of a m by n matrix
   * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
   */
  def svd(mat: DenseMatrix[Double]):(DenseMatrix[Double],DenseVector[Double],DenseMatrix[Double]) = {
    requireNonEmptyMatrix(mat)

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
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (U,S,Vt);
  }

  /**
   * Returns the Kronecker product of the two matrices a and b,
   * usually denoted a ⊗ b.
   */
  def kron[V1,V2,RV](a : Matrix[V1], b : Matrix[V2])(implicit mul : BinaryOp[V1,V2,OpMul,RV], s : Scalar[RV]) : Matrix[RV] = {
    val builder = a.newBuilder[(Int,Int),RV](TableDomain(a.numRows * b.numRows, a.numCols * b.numCols));
    a.foreachNonZeroTriple((ai,aj,av) => b.foreachNonZeroTriple((bi,bj,bv) =>
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

  /**
   * The lower triangular portion of the given real quadratic matrix X. Note
   * that no check will be performed regarding the symmetry of X.
   */
  def lowerTriangular[T: Scalar](X: Matrix[T]): DenseMatrix[T] = {
      val N = X.numRows
      val builder = X.newBuilder[(Int, Int), T](TableDomain(N, N))
      for (i <- 0 until N; j <- 0 to i)
        builder((i,j)) = X(i,j)
      builder.result.asInstanceOf[DenseMatrix[T]]
  }

  /**
   * The upper triangular portion of the given real quadratic matrix X. Note
   * that no check will be performed regarding the symmetry of X.
   */
  def upperTriangular[T: Scalar](X: Matrix[T]): DenseMatrix[T] = {
      val N = X.numRows
      val builder = X.newBuilder[(Int, Int), T](TableDomain(N, N))
      for (i <- 0 until N; j <- i until N)
        builder((i,j)) = X(i,j)
      builder.result.asInstanceOf[DenseMatrix[T]]
  }

  /**
   * Computes the cholesky decomposition A of the given real symmetric
   * positive definite matrix X such that X = A A^T.
   *
   * XXX: For higher dimensionalities, the return value really should be a
   *      sparse matrix due to its inherent lower triangular nature.
   */
  def cholesky(X: Matrix[Double]): DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)

    // As LAPACK doesn't check if the given matrix is in fact symmetric,
    // we have to do it here (or get rid of this time-waster as long as
    // the caller of this function is clearly aware that only the lower
    // triangular portion of the given matrix is used and there is no
    // check for symmetry).
    requireSymmetricMatrix(X)

    // Copy the lower triangular part of X. LAPACK will store the result in A
    val A: DenseMatrix[Double] = lowerTriangular(X)

    val N = X.numRows
    val info = new intW(0)
    LAPACK.getInstance.dpotrf(
      "L" /* lower triangular */,
      N /* number of rows */, A.data, math.max(1, N) /* LDA */,
      info
    )
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dpotrf was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)

    A
  }

 /**
  * QR Factorization with pivoting
  *
  * input: A m x n matrix
  * output: (Q,R,P,pvt) where AP = QR
  *   Q: m x m
  *   R: m x n
  *   P: n x n : permutation matrix (P(pvt(i),i) = 1)
  *   pvt : pivot indices
  */
  def qrp(A: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Int], Array[Int]) = {
    val m = A.numRows
    val n = A.numCols
    val lapack = LAPACK.getInstance();

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    val work = new Array[Double](1)
    var info = new intW(0)
    lapack.dgeqrf(m, n, null, m, null, work, -1, info);
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, math.min(m,n), null, m, null, work, -1, info);
    val lwork2 = if(info.`val` != 0) n else work(0).toInt;
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](math.max(lwork1, lwork2));

    //Perform the QR factorization with dgep3
    val maxd = math.max(m,n)
    val AFact = DenseMatrix.zeros[Double](m,maxd)
    val pvt = new Array[Int](n)
    val tau = new Array[Double](math.min(m,n))
    for(r <- 0 until m; c <- 0 until n) AFact(r,c) = A(r,c)
      lapack.dgeqp3(m, n, AFact.data, m, pvt, tau, workspace, workspace.length, info);

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get R
    val R = DenseMatrix.zeros[Double](m,n)

    for(c <- 0 until maxd if(c < n);
        r <- 0 until m if(r <= c))
        R(r,c) = AFact(r,c)

    //Get Q from the matrix returned by dgep3
    val Q = DenseMatrix.zeros[Double](m,m)
    lapack.dorgqr(m, m, math.min(m,n), AFact.data, m, tau, workspace, workspace.length, info);
    for(r <- 0 until m;
        c <- 0 until maxd if(c < m))
        Q(r,c) = AFact(r,c)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get P
    for(i <- 0 until pvt.length) pvt(i)-=1
    val P = DenseMatrix.zeros[Int](n,n)
    for(i <- 0 until n)
      P(pvt(i), i) = 1

    (Q,R,P,pvt)
  }

  /**
  * QR Factorization
  *
  * input: A m x n matrix
  * optional: skipQ - if true, don't reconstruct orthogonal matrix Q (instead returns (null,R))
  * output: (Q,R)
  *   Q: m x m
  *   R: m x n
  */
  def qr(A: DenseMatrix[Double], skipQ : Boolean = false): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val m = A.numRows
    val n = A.numCols
    val lapack = LAPACK.getInstance();

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    val work = new Array[Double](1)
    var info = new intW(0)
    lapack.dgeqrf(m, n, null, m, null, work, -1, info);
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, math.min(m,n), null, m, null, work, -1, info);
    val lwork2 = if(info.`val` != 0) n else work(0).toInt;
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](math.max(lwork1, lwork2));

    //Perform the QR factorization with dgeqrf
    val maxd = math.max(m,n)
    val mind = math.max(m,n)
    val tau = new Array[Double](mind)
    val outputMat = DenseMatrix.zeros[Double](m,maxd)
    for(r <- 0 until m; c <- 0 until n)
      outputMat(r,c) = A(r,c)
    lapack.dgeqrf(m, n, outputMat.data, m, tau, workspace, workspace.length, info);

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get R
    val R = DenseMatrix.zeros[Double](m,n)
    for(c <- 0 until maxd if(c < n); r <- 0 until m if(r <= c))
      R(r,c) = outputMat(r,c)

    //unless the skipq flag is set
    if(!skipQ){
      //Get Q from the matrix returned by dgep3
      val Q = DenseMatrix.zeros[Double](m,m)
      lapack.dorgqr(m, m, math.min(m,n), outputMat.data, m, tau, workspace, workspace.length, info);
      for(r <- 0 until m;
          c <- 0 until maxd if(c < m))
        Q(r,c) = outputMat(r,c)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()
      (Q,R)
    }
    //skip Q and just return R
    else (null,R)
  }




  /**
   * Computes all eigenvalues (and optionally right eigenvectors) of the given
   * real symmetric matrix X.
   */
  def eigSym(X: Matrix[Double], rightEigenvectors: Boolean):
    (DenseVector[Double], Option[DenseMatrix[Double]]) =
  {
    requireNonEmptyMatrix(X)

    // As LAPACK doesn't check if the given matrix is in fact symmetric,
    // we have to do it here (or get rid of this time-waster as long as
    // the caller of this function is clearly aware that only the lower
    // triangular portion of the given matrix is used and there is no
    // check for symmetry).
    requireSymmetricMatrix(X)

    // Copy the lower triangular part of X. LAPACK will store the result in A.
    val A     = lowerTriangular(X)

    val N     = X.numRows
    val evs   = DenseVector.zeros[Double](N)
    val lwork = math.max(1, 3*N-1)
    val work  = Array.ofDim[Double](lwork)
    val info  = new intW(0)
    LAPACK.getInstance.dsyev(
      if (rightEigenvectors) "V" else "N" /* eigenvalues N, eigenvalues & eigenvectors "V" */,
      "L" /* lower triangular */,
      N /* number of rows */, A.data, math.max(1, N) /* LDA */,
      evs.data,
      work /* workspace */, lwork /* workspace size */,
      info
    )
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dsyev was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)

    (evs, if (rightEigenvectors) Some(A) else None)
  }

  /**
   * Computes the LU factorization of the given real M-by-N matrix X such that
   * X = P * L * U where P is a permutation matrix (row exchanges).
   *
   * Upon completion, a tuple consisting of a matrix A and an integer array P.
   *
   * The upper triangular portion of A resembles U whereas the lower triangular portion of
   * A resembles L up to but not including the diagonal elements of L which are
   * all equal to 1.
   *
   * For 0 <= i < M, each element P(i) denotes whether row i of the matrix X
   * was exchanged with row P(i-1) during computation (the offset is caused by
   * the internal call to LAPACK).
   */
  def LU[T](X: Matrix[T])(implicit td: T => Double): (DenseMatrix[Double], Array[Int]) = {
    requireNonEmptyMatrix(X)

    val M    = X.numRows
    val N    = X.numCols
    val Y    = DenseMatrix.tabulate[Double](M,N)(X(_,_))
    val ipiv = Array.ofDim[Int](math.min(M,N))
    val info = new intW(0)
    LAPACK.getInstance.dgetrf(
      M /* numRows */, N /* numCols */,
      Y.data, math.max(1,M) /* LDA */,
      ipiv /* pivot indices */,
      info
    )
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dsyev was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    (Y, ipiv)
  }

  /**
   * Computes the determinant of the given real matrix.
   */
  def det[T](X: Matrix[T])(implicit td: T => Double): Double = {
    requireSquareMatrix(X)

    // For triangular N-by-N matrices X, the determinant of X equals the product
    // of the diagonal elements X(i,i) where 0 <= i < N.
    // Since det(AB) = det(A) * det(B), the LU factorization is well-suited for
    // the computation of the determinant of general N-by-N matrices.
    val (m, ipiv) = LU(X)

    // Count the number of exchanged rows.  ipiv contains an array of swapped indices,
    //  but it also contains indices that weren't swapped.  To count the swapped
    //  indices, we have to compare them against their position within the array.  A
    //  final complication is that the array indices are 1-based, due to the LU call
    //  into LAPACK.
    val numExchangedRows = ipiv.map(_ - 1).zipWithIndex.count { piv => piv._1 != piv._2 }

    var acc = if (numExchangedRows % 2 == 1) -1.0 else 1.0
    for (i <- 0 until m.numRows)
      acc *= m(i,i)

    acc
  }

  /**
   * Computes the inverse of a given real matrix.
   */
  def inv[T](X: Matrix[T])(implicit td: T => Double): DenseMatrix[Double] = {
    requireSquareMatrix(X)

    val (m, ipiv) = LU(X)
    val N         = m.numRows
    val lwork     = math.max(1, N)
    val work      = Array.ofDim[Double](lwork)
    val info      = new intW(0)
    LAPACK.getInstance.dgetri(
      N, m.data, math.max(1, N) /* LDA */,
      ipiv,
      work /* workspace */, lwork /* workspace size */,
      info
    )
    assert(info.`val` >= 0, "Malformed argument %d (LAPACK)".format(-info.`val`))

    if (info.`val` > 0)
      throw new MatrixSingularException

    m
  }

  /**
   * Computes the Moore-Penrose pseudo inverse of the given real matrix X.
   */
  def pinv(X: DenseMatrix[Double]) : DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)

    // The pseudo inverse is nothing but the least-squares solution to AX=B,
    // hence:
    //       d/dX 1/2 (AX-B)^2 = A^T (AX-B)
    // Solving A^T (AX-B) = 0 for X yields
    //       A^T AX = A^T B
    //    =>      X = (A^T A)^(-1) A^T B

    inv(X.t * X) * X.t
  }

  /**
   * Computes the Moore-Penrose pseudo inverse of the given real matrix X.
   */
  def pinv[V](X: DenseMatrix[V])(implicit cast : V=>Double) : DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)

    // The pseudo inverse is nothing but the least-squares solution to AX=B,
    // hence:
    //       d/dX 1/2 (AX-B)^2 = A^T (AX-B)
    // Solving A^T (AX-B) = 0 for X yields
    //       A^T AX = A^T B
    //    =>      X = (A^T A)^(-1) A^T B

    pinv(X.mapValues(cast));
  }

  /**
   * A diagonal matrix whose elements are specified by the given vector.
   */
  def diag[S](v: scalala.tensor.Vector[S])(implicit s: Scalar[S]): DiagonalMatrix[Vector[S], S] =
    new DiagonalMatrix(v)

  /**
   * Vector cross product of 3D vectors a and b.
   */
  def cross[V1, V2, RV](a: DenseVector[V1], b: DenseVector[V2])(
    implicit mul: BinaryOp[V1, V2, OpMul, RV],
    sub: BinaryOp[RV, RV, OpSub, RV],
    s: Scalar[RV]
  ): DenseVector[RV] = {
    require(a.length == 3)
    require(b.length == 3)
    DenseVector[RV](
      sub(mul(a(1), b(2)), mul(a(2), b(1))),
      sub(mul(a(2), b(0)), mul(a(0), b(2))),
      sub(mul(a(0), b(1)), mul(a(1), b(0)))
    )
  }

  /**
   * Computes the rank of a DenseMatrix[Double].
   *
   * The rank of the matrix is computed using the SVD method.  The singular values of the SVD
   * which are greater than a specified tolerance are counted.
   *
   * @param m matrix for which to compute the rank
   * @param tol optional tolerance for singular values.  If not supplied, the default
   *   tolerance is: max(m.numCols, m.numRows) * eps * sigma_max, where
   *   eps is the machine epsilon and sigma_max is the largest singular value of m.
   * @return the rank of the matrix (number of singular values)
   */
  def rank(m: DenseMatrix[Double], tol: Option[Double] = None): Int = {    
    val (u, s, vt) = svd(m)
    val useTol = tol.getOrElse {
      // we called LAPACK for the SVD method, so this is the LAPACK definition of eps.
      val eps: Double = 2.0 * LAPACK.getInstance.dlamch("e")
      math.max(m.numCols, m.numRows) * eps * s.max
    }
    s.data.count(_ > useTol)  // TODO: Use DenseVector[_].count() if/when that is implemented
  }

}

object LinearAlgebra extends LinearAlgebra;


/**
 * Exception thrown if a routine has not converged.
 */
class NotConvergedException(val reason: NotConvergedException.Reason, msg: String = "")
extends RuntimeException(msg)

object NotConvergedException {
  trait Reason;
  object Iterations extends Reason;
  object Divergence extends Reason;
  object Breakdown extends Reason;
}

class MatrixNotSymmetricException
extends IllegalArgumentException("Matrix is not symmetric");

class MatrixNotSquareException
extends IllegalArgumentException("Matrix is not square");

class MatrixEmptyException
extends IllegalArgumentException("Matrix is empty");

