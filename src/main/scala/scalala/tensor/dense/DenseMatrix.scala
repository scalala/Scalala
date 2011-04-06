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


import domain.TableDomain;

import scalala.generic.collection._;
import scalala.scalar.Scalar;
import scalala.library.random.MersenneTwisterFast;
import scalala.library.Random;
import scalala.operators._;

import org.netlib.blas._;
import org.netlib.lapack._;
import org.netlib.util.intW;

/**
 * A DenseMatrix is backed by an array of doubles, with each column
 * stored before the next column begins.
 *
 * @author dramage
 */
class DenseMatrix[@specialized(Int,Long,Float,Double) V]
(numRows : Int, numCols : Int, data_ : Array[V])
(implicit override val scalar : Scalar[V])
extends DenseArrayTensor[(Int,Int),V] with DenseArrayTensorLike[(Int,Int),V,TableDomain,DenseMatrix[V]]
with mutable.Matrix[V] with mutable.MatrixLike[V,DenseMatrix[V]] {
  override val data = data_ // workaround for https://lampsvn.epfl.ch/trac/scala/ticket/4013
  
  if (numRows * numCols != data_.length)
    throw new IllegalArgumentException("data.length must equal numRows*numCols");

  override def domain = TableDomain(numRows, numCols);

  /**
   * Returns a view of this data matrix but with the given number of rows and columns.
   * 
   * @throws IllegalArgumentException if the requested size does not match the current size.
   */
  def reshape(rows : Int, cols : Int) : DenseMatrix[V] = {
    if (rows * cols != data.length) {
      throw new IllegalArgumentException("Cannot reshape "+numRows+"x"+numCols+" to "+rows+"x"+cols);
    }
    new DenseMatrix(rows, cols, data);
  }

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

  override def update(row : Int, col : Int, value : V) =
    data(index(row,col)) = value;

  override def foreach[U](f : (Int,Int,V)=>U) = {
    var i = 0;
    while (i < data.length) {
      f(rowIndex(i),colIndex(i),data(i));
      i += 1;
    }
  }

  override def foreachValue[U](f : (V)=>U) = {
    var i = 0;
    while (i < data.length) {
      f(data(i));
      i += 1;
    }
  }

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transform(f : (Int,Int,V)=>V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(rowIndex(i),colIndex(i),data(i));
      i += 1;
    }
  }

  /** Assigns the given value to all elements of this map. */
  def :=(value : V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = value;
      i += 1;
    }
  }

  /** Tranforms all values in this map by applying the given function. */
  override def transformValues(f : V=>V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(data(i));
      i += 1;
    }
  }

  def copy =
    new DenseMatrix[V](numRows, numCols, data.clone);
}

object DenseMatrix extends DenseMatrixConstructors {

  //
  // Capabilities
  //

  class DenseMatrixCanSliceRow[@specialized V:Scalar]
  extends CanSliceRow[DenseMatrix[V],Int,DenseVectorRow[V]] {
    override def apply(from : DenseMatrix[V], i : Int) =
      new DenseVectorRow[V](from.data, length = from.numCols, offset = i, stride = from.numRows);
  }
  
  class DenseMatrixCanSliceCol[@specialized V:Scalar]
  extends CanSliceCol[DenseMatrix[V],Int,DenseVectorCol[V]] {
    override def apply(from : DenseMatrix[V], j : Int) =
      new DenseVectorCol[V](from.data, length = from.numRows, offset = j * from.numRows, stride = 1);
  }

  class DenseMatrixCanMapValues[@specialized(Int,Long,Float,Double) V, @specialized(Int,Long,Float,Double) R:ClassManifest:Scalar]
  extends CanMapValues[DenseMatrix[V],V,R,DenseMatrix[R]] {
    override def map(from : DenseMatrix[V], fn : (V=>R)) = {
      val data = new Array[R](from.data.length);
      var i = 0;
      while (i < data.length) {
        data(i) = fn(from.data(i));
        i += 1;
      }
      new DenseMatrix[R](from.numRows, from.numCols, data);
    }

    override def mapNonZero(from : DenseMatrix[V], fn : (V=>R)) =
      map(from, fn);
  }

  class DenseMatrixCanMapKeyValuePairs[@specialized(Int,Long,Float,Double) V, @specialized(Int,Long,Float,Double) R:ClassManifest:Scalar]
  extends CanMapKeyValuePairs[DenseMatrix[V],(Int,Int),V,R,DenseMatrix[R]] {
    override def map(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) = {
      val data = new Array[R](from.data.length);
      var i = 0;
      while (i < data.length) {
        data(i) = fn(from.unindex(i), from.data(i));
        i += 1;
      }
      new DenseMatrix(from.numRows, from.numCols, data);
    }
    
    override def mapNonZero(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) =
      map(from, fn);
  }

  implicit def mkDenseMatrixCanSliceRow[@specialized V:Scalar] =
    new DenseMatrixCanSliceRow[V];

  implicit def mkDenseMatrixCanSliceCol[@specialized V:Scalar] =
    new DenseMatrixCanSliceCol[V];
    
  implicit def mkDenseMatrixCanMapValues[V,R:ClassManifest:Scalar] =
    new DenseMatrixCanMapValues[V,R];
  
  implicit def mkDenseMatrixCanMapKeyValuePairs[V,R:ClassManifest:Scalar] =
    new DenseMatrixCanMapKeyValuePairs[V,R];

  implicit object DenseMatrixCanSliceRowI extends DenseMatrixCanSliceRow[Int];
  implicit object DenseMatrixCanSliceRowL extends DenseMatrixCanSliceRow[Long];
  implicit object DenseMatrixCanSliceRowF extends DenseMatrixCanSliceRow[Float];
  implicit object DenseMatrixCanSliceRowD extends DenseMatrixCanSliceRow[Double];

  implicit object DenseMatrixCanSliceColI extends DenseMatrixCanSliceCol[Int];
  implicit object DenseMatrixCanSliceColL extends DenseMatrixCanSliceCol[Long];
  implicit object DenseMatrixCanSliceColF extends DenseMatrixCanSliceCol[Float];
  implicit object DenseMatrixCanSliceColD extends DenseMatrixCanSliceCol[Double];
  
  implicit object DenseMatrixCanMapValuesDD extends DenseMatrixCanMapValues[Double,Double];
  implicit object DenseMatrixCanMapValuesII extends DenseMatrixCanMapValues[Int,Int];
  implicit object DenseMatrixCanMapValuesID extends DenseMatrixCanMapValues[Int,Double];


  //
  // BLAS and LAPACK routines
  //
  
  implicit object DenseMatrixDMulDenseMatrixD
  extends BinaryOp[DenseMatrix[Double],DenseMatrix[Double],OpMulMatrixBy,DenseMatrix[Double]] {
    def opType = OpMulMatrixBy;
    def apply(a : DenseMatrix[Double], b : DenseMatrix[Double]) = {
      val rv = DenseMatrix.zeros[Double](a.numRows, b.numCols);
      // System.err.println("BLAS!");
      BLAS.getInstance().dgemm("n", "n",
        rv.numRows, rv.numCols, a.numCols,
        1.0, a.data, a.numRows, b.data, a.numCols,
        0.0, rv.data, a.numRows);
      rv;
    }
  }

  implicit object DenseMatrixDMulDenseVectorColD
  extends BinaryOp[DenseMatrix[Double],DenseVectorCol[Double],OpMulMatrixBy,DenseVectorCol[Double]] {
    def opType = OpMulMatrixBy;
    def apply(a : DenseMatrix[Double], b : DenseVectorCol[Double]) = {
      val rv = DenseVectorCol.zeros[Double](a.numRows);
      // System.err.println("BLAS!");
      org.netlib.blas.Dgemv.dgemv("n",
        a.numRows, a.numCols,
        1.0, a.data, 0, a.numRows,
             b.data, b.offset, b.stride,
        0.0, rv.data, rv.offset, rv.stride);
      rv;
    }
  }

  implicit object DenseMatrixCanSolveDenseMatrix
  extends BinaryOp[DenseMatrix[Double],DenseMatrix[Double],OpSolveMatrixBy,DenseMatrix[Double]] {
    override def opType = OpSolveMatrixBy;
    override def apply(A : DenseMatrix[Double], V : DenseMatrix[Double]) = {
      require(A.numRows == V.numRows,
              "Non-conformant matrix sizes");

      // from MTJ 0.9.9
      if (A.numRows == A.numCols) {
        // square: LUSolve
        val X = V.copy;
        LUSolve(X,A);
        X;
      } else {
        // non-square: QRSolve
        val X = DenseMatrix.zeros[Double](A.numCols, V.numCols);
        QRSolve(X,A,V,false);
        X;
      }
    }

    /** X := A \ X */
    def LUSolve(X : DenseMatrix[Double], A : DenseMatrix[Double]) = {
      val piv = new Array[Int](A.numRows);

      val info = new intW(0);
      LAPACK.getInstance().dgesv(
        A.numRows, X.numCols,
        A.data.clone(), math.max(1, A.numRows),
        piv,
        X.data, math.max(1, A.numRows), info);

      if (info.`val` > 0)
        throw new MatrixSingularException();
      else if (info.`val` < 0)
        throw new IllegalArgumentException();

      X;
    }

    /** X := A \ V */
    def QRSolve(X : DenseMatrix[Double], A : DenseMatrix[Double], V : DenseMatrix[Double], transpose : Boolean) = {
      require(X.numRows == A.numCols, "Wrong number of rows in return value");
      require(X.numCols == V.numCols, "Wrong number of rows in return value");

      val nrhs = V.numCols;
      
      // allocate temporary solution matrix
      val Xtmp = DenseMatrix.zeros[Double](math.max(A.numRows, A.numCols), nrhs);
      val M = if (!transpose) A.numRows else A.numCols;
      for (j <- 0 until nrhs; i <- 0 until M) { Xtmp(i,j) = V(i,j); }

      val newData = A.data.clone();

      // query optimal workspace
      val queryWork = new Array[Double](1);
      val queryInfo = new intW(0);
      LAPACK.getInstance().dgels(
        if (!transpose) "N" else "T",
        A.numRows, A.numCols, nrhs,
        newData, math.max(1,A.numRows),
        Xtmp.data, math.max(1,math.max(A.numRows,A.numCols)),
        queryWork, -1, queryInfo);

      // allocate workspace
      val work = {
        val lwork = {
          if (queryInfo.`val` != 0)
            math.max(1, math.min(A.numRows, A.numCols) + math.max(math.min(A.numRows, A.numCols), nrhs));
          else
            math.max(queryWork(0).toInt, 1);
        }
        new Array[Double](lwork);
      }

      // compute factorization
      val info = new intW(0);
      LAPACK.getInstance().dgels(
        if (!transpose) "N" else "T",
        A.numRows, A.numCols, nrhs,
        newData, math.max(1,A.numRows),
        Xtmp.data, math.max(1,math.max(A.numRows,A.numCols)),
        work, work.length, info);

      if (info.`val` < 0)
        throw new IllegalArgumentException;

      // extract solution
      val N = if (!transpose) A.numCols else A.numRows;
      for (j <- 0 until nrhs; i <- 0 until N) X(i,j) = Xtmp(i,j);

      X;
    }
  }

  implicit object DenseMatrixCanSolveDenseVector
  extends BinaryOp[DenseMatrix[Double],DenseVectorCol[Double],OpSolveMatrixBy,DenseVectorCol[Double]] {
    override def opType = OpSolveMatrixBy;
    override def apply(a : DenseMatrix[Double], b : DenseVectorCol[Double]) = {
      val rv = a \ new DenseMatrix[Double](b.size, 1, b.data);
      new DenseVectorCol[Double](rv.data);
    }
  }
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
  def apply[R,V](rows : R*)(implicit rl : LiteralRow[R,V], scalar : Scalar[V]) = {
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
  def zeros[V](rows : Int, cols : Int)(implicit s : Scalar[V]) = {
    if (s.isPrimitive) {
      new DenseMatrix(rows, cols, s.manifest.newArray(rows * cols));
    } else {
      fill(rows, cols)(s.zero);
    }
  }

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

  /** A vector of the given size with uniform random values between 0 and 1. */
  def rand(rows : Int, cols : Int, mt : MersenneTwisterFast = Random.mt) = mt.synchronized {
    tabulate(rows, cols)((i,j) => mt.nextDouble);
  }

  /**
   * A vector of the given size with normally distributed random values
   * with mean 0 and standard deviation 1.
   */
  def randn(rows : Int, cols : Int, mt : MersenneTwisterFast = Random.mt) = mt.synchronized {
    tabulate(rows, cols)((i,j) => mt.nextGaussian);
  }

  /** A vector of the given size of random integers in the range [0..max). */
  def randi(imax : Int, rows : Int, cols : Int, mt : MersenneTwisterFast = Random.mt) = mt.synchronized {
    tabulate(rows, cols)((i,j) => mt.nextInt(imax));
  }
}

