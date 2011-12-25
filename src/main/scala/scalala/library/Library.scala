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

import scalala.generic.math._
import scalala.tensor.mutable.Counter;
import tensor._
import dense._
import domain.CanGetDomain
import scalar.Scalar
import operators._
import scalala.generic.collection.{CanMapValues, CanSliceCol, CanBuildTensorFrom, CanSliceRow}

/**
 * Library of scalala basic mathematical functions.
 *
 * @author dramage, afwlehmann
 */
trait Library {

  //
  // Aliases to scala math package.
  //

  /** Alias for math.log. */
  final def log(v : Double) : Double = scala.math.log(v);

  /** Alias for math.log1p. */
  final def log1p(v : Double) : Double = scala.math.log1p(v);

  /** Alias for math.exp. */
  final def exp(v : Double) : Double = scala.math.exp(v);

  /** Alias for math.pow. */
  final def pow(base : Double, exponent : Double) : Double =
    scala.math.pow(base, exponent);

  /** Alias for math.abs. */
  final def abs(v : Double) : Double = scala.math.abs(v);

  /** Alias for x.isNaN. */
  final def isnan(x : Double) = java.lang.Double.isNaN(x);

  /** Alias for math.sqrt. */
  final def sqrt(x : Double) = scala.math.sqrt(x);

  /** Alias for Double.NaN */
  final val NaN = Double.NaN;

  /** Alias for Double.NaN */
  final val nan = NaN;

  /** Alias for Double.PositiveInfinity */
  final val Inf = Double.PositiveInfinity;

  /** Alias for Double.PositiveInfinity */
  final val inf = Inf;

  //
  // Collection level operations
  //

  /** Take the log of the given value. */
  def log[V,That](value : V)(implicit log : CanLog[V,That]) : That =
    log(value);

  /** Take the exp of the given value. */
  def exp[V,That](value : V)(implicit exp : CanExp[V,That]) : That =
    exp(value);

    /** Take the log of the given value. */
  def abs[V,That](value : V)(implicit abs : CanAbs[V,That]) : That =
    abs(value);

  /** Take the n-norm of the given values. */
  def mean[V,That](value : V)(implicit mean : CanMean[V,That]) : That =
    mean(value);

  /** Take the n-norm of the given value. */
  def norm[V](value : V, n : Double)(implicit norm : CanNorm[V]) : Double =
    norm(value, n);

  /** Take the sqrt of the given value. */
  def sqrt[V,That](value : V)(implicit sqrt : CanSqrt[V,That]) : That =
    sqrt(value);

  /** Take the standard deviation of a collection. */
  def stddev[V,VV,That](data : V)
  (implicit variance : CanVariance[V,VV], sqrt : CanSqrt[VV,That]) : That =
    sqrt(variance(data));

  /** Take the variance of a collection. */
  def variance[V,That](data : V)(implicit variance : CanVariance[V,That]) =
    variance(data);

  /** Take the softmax of a collection. */
  def softmax[V](value: V)(implicit softmax: CanSoftmax[V]) : Double =
    softmax.softmax(value)

  sealed trait Axis;
  object Axis {
    type Value = Axis
    case object Horizontal extends Axis;
    case object Vertical extends Axis;
  }

  /**
   * Minimum vector of the given matrix along the specified axis.
   */
  def min[T: Scalar](X: Matrix[T], axis: Axis.Value): DenseVector[T] = {
    axis match {
      case Axis.Horizontal => DenseVectorCol.tabulate[T](X.numRows)(X(_,::).min)
      case Axis.Vertical => DenseVectorRow.tabulate[T](X.numCols)(X(::,_).min)
    }
  }

  /**
   * Maximum vector of the given matrix along the specified axis.
   */
  def max[T: Scalar](X: Matrix[T], axis: Axis.Value): DenseVector[T] = {
    axis match {
      case Axis.Horizontal => DenseVectorCol.tabulate[T](X.numRows)(X(_,::).max)
      case Axis.Vertical => DenseVectorRow.tabulate[T](X.numCols)(X(::,_).max)
    }
  }

  /**
   * Mean vector of the given matrix along the specified axis.
   */
  def mean[@specialized T](X: Matrix[T], axis: Axis.Value)(implicit xv: T => Double)
  : DenseVector[Double] = {
    // TODO: This calculation of the mean is rather slow. It should
    //       be transformed into several while loops.
    axis match {
      case Axis.Horizontal =>
        var mu = DenseVector.zeros[Double](X.numRows)
        X foreachTriple ( (i, j, value) =>
          mu(i) += (value - mu(i)) / (j + 1)
        )
        mu

      case Axis.Vertical =>
        var mu = DenseVector.zeros[Double](X.numCols)
        X foreachTriple ( (i, j, value) =>
          mu(j) += (value - mu(j)) / (i + 1)
        )
        mu.t
    }
  }

  /**
   * The covariance matrix and mean of the given dataset X where the samples of
   * a multivariate random distribution are stacked along the given axis.
   */
  def covariance(X: Matrix[Double], axis: Axis.Value = Axis.Horizontal):
    (DenseMatrix[Double], DenseVector[Double]) =
  {
    require(X.numCols > 0 && X.numRows > 0)

    axis match {
      case Axis.Horizontal =>
        val dim   = X.numRows
        val mu    = DenseVector.tabulate[Double](dim)(X(_,0))
        val Sigma = DenseMatrix.zeros[Double](dim, dim)
        var K     = 1.0
        for (i <- 1 until X.numCols) {
          val xMinusMu = X(::,i) - mu
          K     += 1
          mu    += xMinusMu / K
          Sigma += xMinusMu * xMinusMu.t * (1. - 1. / K)
        }
        (Sigma / math.max(1, K-1), mu)

      case Axis.Vertical =>
        val dim   = X.numCols
        val mu    = DenseVector.tabulate[Double](dim)(X(0,_))
        val Sigma = DenseMatrix.zeros[Double](dim, dim)
        var K     = 1.0
        for (i <- 1 until X.numRows) {
          val xMinusMu = X(i,::) - mu
          K     += 1
          mu    += xMinusMu / K
          Sigma += xMinusMu.t * xMinusMu * (1. - 1. / K)
        }
        (Sigma / math.max(1, K-1), mu)
    }
  }

  /** Sums the tensor2 along the horizontal axis */
  def sum[K1,K2,V,T,ADomain,Row](matrix: T,
                                 axis: Axis.Horizontal.type = Axis.Horizontal)
                                (implicit view: T=>Tensor2[K1,K2,V],
                                 sliceRows: CanSliceRow[T,K1,Row],
                                 opAdd: BinaryOp[Row,Row,OpAdd,Row]):Row  = {
    matrix.domain._1.view.map(sliceRows(matrix,_)).reduceLeft(opAdd);
  }


  /** Sums the tensor2 along the vertical axis */
  def sum[K1,K2,V,T,ADomain,Col](matrix: T,
                                 axis: Axis.Vertical.type)
                                (implicit view: T=>Tensor2[K1,K2,V],
                                 sliceCols: CanSliceCol[T,K2,Col],
                                 opAdd: BinaryOp[Col,Col,OpAdd,Col]):Col  = {
    matrix.domain._2.view.map(sliceCols(matrix,_)).reduceLeft(opAdd);
  }

  //
  // Constructors
  //

  /** Counts the given items. */
  def count[X](items : TraversableOnce[X]) : Counter[X,Int] =
    Counter.count(items);

  //
  // normalization and log-normalization:
  //

  /**
   * Normalizes the argument such that its norm is 1.0 (with respect to the argument n).
   * Returns value if value's norm is 0.
   */
  def normalize[V,K,That](value: V, n: Double)(implicit _norm: CanNorm[V], st: V<:<NumericOps[V], op: BinaryOp[V,Double,OpDiv,V]):V = {
    val norm = _norm(value,n)
    if(norm == 0) value
    else value / norm;
  }

  /**
   * logNormalizes the argument such that the softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logNormalize[V,K](value: V)(implicit view: V => NumericOps[V],
                                  sm: CanSoftmax[V],
                                  op : BinaryOp[V,Double,OpSub,V]): V = {
    val max = softmax(value)
    if(max.isInfinite) value
    else value - max;
  }

  /**
   * logs and then logNormalizes the argument such that the softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logAndNormalize[V](value: V)(implicit canLog : CanLog[V,V],
                                     view: V => NumericOps[V], sm: CanSoftmax[V],
                                     op : BinaryOp[V,Double,OpSub,V]):V = {
    logNormalize(log(value))
  }

  def logAndNormalizeRows[T,K1,K2,Row,ADomain,That](matrix: T)(implicit view: T=>Tensor2[K1,K2,Double],
                                                    domainA : CanGetDomain[T,ADomain],
                                                    sliceRows: CanSliceRow[T,K1,Row],
                                                    canLog: CanMapValues[Row,Double,Double,Row],
                                                    view2: Row => Tensor1[K2,Double] with NumericOps[Row], sm: CanSoftmax[Row],
                                                    op : BinaryOp[Row,Double,OpSub,Row],
                                                    bf : CanBuildTensorFrom[T,ADomain,(K1,K2),Double,That]): That = {
    val builder = bf(matrix,domainA(matrix));
    for(k1 <- matrix.domain._1) {
      val row: Row = sliceRows(matrix, k1);
      val ln: Row = logAndNormalize(row);
      for((k2,v) <- ln.pairsIterator) {
        builder(k1 -> k2) = v;
      }
    }
    builder.result;
  }

  /**
   * Generates a vector of linearly spaced values between a and b (inclusive).
   * The returned vector will have length elements, defaulting to 100.
   */
  def linspace(a : Double, b : Double, length : Int = 100) : DenseVectorCol[Double] = {
    val increment = (b - a) / (length - 1);
    DenseVectorCol.tabulate(length)(i => a + increment * i);
  }
}

object Library extends Library;
