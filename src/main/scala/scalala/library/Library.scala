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

import scalala.generic.math._;

import scalala.operators.{OpSub, NumericOps, OpDiv, BinaryOp}
import scalala.tensor.{VectorCol, ::, Matrix, Vector}
import scalala.tensor.mutable.Counter;
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.generic.collection.{CanSliceCol, CanViewAsVector}

/**
 * Library of scalala basic mathematical functions.
 *
 * @author dramage
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

  object Axis extends Enumeration {
    val Horizontal, Vertical = Value
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
   * The covariance matrix and mean of the given dataset X where each column
   * of X represents one sample of a multivariate random distribution.
   */
  def covariance[T](X: Matrix[T])
  (implicit css: CanSliceCol[Matrix[T],Int,Vector[Double]], td: T => Double)
  : (DenseMatrix[Double], DenseVector[Double]) = {
    if (X.numRows < 1 || X.numCols < 1)
      throw new IllegalArgumentException

    val N        = X.numRows
    var mu       = DenseVector.tabulate[Double](N)(X(_,0))
    var Sigma    = DenseMatrix.zeros[Double](N, N)
    var K        = 1.0
    for (i <- 1 until X.numCols) {
      val xMinusMu: VectorCol[Double] = X(::,i) - mu
      K     += 1
      mu    += xMinusMu / K
      Sigma += xMinusMu * xMinusMu.t * (1. - 1. / K)
    }

    (Sigma / math.max(1, K-1), mu)
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
  def logNormalize[V,K](value: V)(implicit view: V<:<NumericOps[V],
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
  def logAndNormalize[V,K](value: V)(implicit canLog : CanLog[V,V],
                                     view: V <:< NumericOps[V], sm: CanSoftmax[V],
                                     op : BinaryOp[V,Double,OpSub,V]):V = {
    logNormalize(log(value))
  }

}

object Library extends Library;
