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

import generic.{CanMulMatrixBy,CanMulRowBy};
import generic.{CanSolveMatrix,MatrixSingularException};
import generic.collection._;

import domain.TableDomain;
import scalar.Scalar;

import scalala.library.random.MersenneTwisterFast;
import scalala.library.Random;

import org.netlib.lapack._;
import org.netlib.util.intW;

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

  def copy =
    new DenseMatrix[B](numRows, numCols, data.clone);


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

  implicit object DenseMatrixCanSolveDenseMatrix
  extends CanSolveMatrix[DenseMatrix[Double],DenseMatrix[Double],DenseMatrix[Double]] {
    override def apply(A : DenseMatrix[Double], B : DenseMatrix[Double]) = {
      require(A.numRows == B.numRows,
              "Non-conformant matrix sizes");

      // from MTJ 0.9.9
      if (A.numRows == A.numCols) {
        // square: LUSolve
        val X = B.copy;
        LUSolve(X,A);
        X;
      } else {
        // non-square: QRSolve
        val X = DenseMatrix.zeros[Double](A.numCols, B.numCols);
        QRSolve(X,A,B,false);
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

    /** X := A \ B */
    def QRSolve(X : DenseMatrix[Double], A : DenseMatrix[Double], B : DenseMatrix[Double], transpose : Boolean) = {
      require(X.numRows == A.numCols, "Wrong number of rows in return value");
      require(X.numCols == B.numCols, "Wrong number of rows in return value");

      val nrhs = B.numCols;
      
      // allocate temporary solution matrix
      val Xtmp = DenseMatrix.zeros[Double](math.max(A.numRows, A.numCols), nrhs);
      val M = if (!transpose) A.numRows else A.numCols;
      for (j <- 0 until nrhs; i <- 0 until M) { Xtmp(i,j) = B(i,j); }

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
  extends CanSolveMatrix[DenseMatrix[Double],DenseVectorCol[Double],DenseVectorCol[Double]] {
    override def apply(a : DenseMatrix[Double], b : DenseVectorCol[Double]) = {
      val rv = a \ new DenseMatrix[Double](b.size, 1, b.data);
      new DenseVectorCol[Double](rv.data);
    }
  }

  /** Tighten bound on return value to be dense. */
  override implicit def canMulMatrixByCol[V1,V2,RV]
  (implicit sr : CanSliceRow[DenseMatrix[V1],Int,tensor.VectorRow[V1]],
   mul : CanMulRowBy[tensor.VectorRow[V1],tensor.VectorCol[V2],RV],
   scalar : Scalar[RV])
  : CanMulMatrixBy[DenseMatrix[V1], tensor.VectorCol[V2], DenseVectorCol[RV]] =
  super.canMulMatrixByCol[V1,V2,RV](sr,mul,scalar).asInstanceOf[CanMulMatrixBy[DenseMatrix[V1], tensor.VectorCol[V2], DenseVectorCol[RV]]];

  /** Tighten bound on return value to be dense. */
  override implicit def canMulMatrixByMatrix[V1,V2,RV]
  (implicit sr : CanSliceRow[DenseMatrix[V1],Int,tensor.VectorRow[V1]],
   sc : CanSliceCol[tensor.Matrix[V2],Int,tensor.VectorCol[V2]],
   mul : CanMulRowBy[tensor.VectorRow[V1],tensor.VectorCol[V2],RV],
   scalar : Scalar[RV])
  : CanMulMatrixBy[DenseMatrix[V1], tensor.Matrix[V2], DenseMatrix[RV]] =
  super.canMulMatrixByMatrix[V1,V2,RV](sr,sc,mul,scalar).asInstanceOf[CanMulMatrixBy[DenseMatrix[V1], tensor.Matrix[V2], DenseMatrix[RV]]];

  /** Tighten bound on return value to be dense. */
  override implicit def canAppendMatrixColumns[V]
  : CanAppendColumns[DenseMatrix[V],tensor.Matrix[V],DenseMatrix[V]]
  = super.canAppendMatrixColumns[V].asInstanceOf[CanAppendColumns[DenseMatrix[V],tensor.Matrix[V], DenseMatrix[V]]];

  /** Tighten bound on return value to be dense. */
  override implicit def canAppendVectorColumn[V]
  : CanAppendColumns[DenseMatrix[V],tensor.VectorCol[V],DenseMatrix[V]]
  = super.canAppendVectorColumn[V].asInstanceOf[CanAppendColumns[DenseMatrix[V],tensor.VectorCol[V],DenseMatrix[V]]];
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
