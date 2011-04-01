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

import random.MersenneTwisterFast
import tensor.{::, Matrix, Vector}
import tensor.dense.DenseMatrix

/**
 * <p>Random number generation.  This class uses the MersenneTwisterFast
 * implementation by Sean Luke http://www.cs.gmu.edu/~sean/research/
 * as its underlying random number generator.  The Mersenne Twister
 * is very fast with an excellent pseudo-random distribution, but it
 * is not cryptographically strong.</p>
 *
 * <p>Each random number generating method accepts a
 * MersenneTwisterFast implementation as an implicit argument, defaulting
 * to the <code>Random.mt</code> instance.  That instance's seed is set
 * with the long value held in the scalala.library.random.seed System property.
 * If the property is not defined, the current time in milliseconds is used
 * as the random seed.</p>
 *
 * <p>The MersenneTwisterFast implementation is not thread-safe, so all
 * accessors to an instance (<code>mt</code>) wrap calls in a mt.synchronized
 * block.  Therefore, calling a vector constructor is substantially faster
 * than calling rand() many times.</p>
 *
 * @author dramage,afwlehmann
 */
trait Random {

  /** Returns a pseudo-random number from the interval 0 to 1. */
  def rand()(implicit mt : MersenneTwisterFast) : Double = mt.synchronized {
    mt.nextDouble;
  }

  /** Returns vector of size n, each element from 0 to 1 */
  def rand(n : Int)(implicit mt : MersenneTwisterFast) : Array[Double] = mt.synchronized {
    val v = new Array[Double](n);
    var i = 0;
    while (i < v.length) {
      v(i) = mt.nextDouble;
      i += 1;
    }
    v;
  }

  /** Returns a random matrix of the given size, each element drawn from 0 to 1 */
  def rand(rows : Int, cols : Int)(implicit mt : MersenneTwisterFast) : Array[Array[Double]] = mt.synchronized {
    val m = Array.tabulate(rows)(i => new Array[Double](cols));
    for (i <- 0 until rows; j <- 0 until cols) {
      m(i)(j) = mt.nextDouble;
    }
    return m;
  }

  /** Returns a pseudo-random gaussian variable. */
  def randn()(implicit mt : MersenneTwisterFast) = mt.synchronized {
    mt.nextGaussian;
  }

  /** Returns a random array of size n, each element from a gaussian. */
  def randn(n : Int)(implicit mt : MersenneTwisterFast) = mt.synchronized {
    val v = new Array[Double](n);
    var i = 0;
    while (i < v.length) {
      v(i) = mt.nextGaussian;
      i += 1;
    }
    v;
  }

  /** Returns a random matrix of the given size, each element drawn from a gaussian */
  def randn(rows : Int, cols : Int)(implicit mt : MersenneTwisterFast) : Array[Array[Double]] = mt.synchronized {
    val m = Array.tabulate(rows)(i => new Array[Double](cols));
    for (i <- 0 until rows; j <- 0 until cols) {
      m(i)(j) = mt.nextGaussian;
    }
    return m;
  }

  /**
   * Computes a matrix whose columns represent samples drawn from a multivariate
   * Gaussian distribution obeying both the given mean `mu' and covariance
   * matrix `Sigma'.
   *
   * @throws NotConvergedException in case of a malformed covariance matrix
   */
  def randn(mu: Vector[Double], Sigma: Matrix[Double], numSamples: Int): DenseMatrix[Double] = {
    if (numSamples < 1)
      throw new IllegalArgumentException

    require(Sigma.numRows == Sigma.numCols, "Matrix is not square!")
    for (i <- 0 until Sigma.numRows; j <- 0 until i) {
      if (Sigma(i,j) != Sigma(j,i))
        throw new IllegalArgumentException
    }

    if (mu.size != Sigma.numCols)
      throw new IllegalArgumentException

    // Assuming multivariate samples with zero mean, the general form of
    // the covariance matrix is X X^T where the scale factor 1/(N-1) has
    // been left out for improved readabilty.
    // Let Y = AX, then Y Y^T = (AX) (AX)^T = A (X X^T) A^T.
    // In case of a random matrix X where each element of X has been
    // drawn from a standard normal distribution, and where thus all
    // random variables are i.i.d. it holds that X X^T = I (identity)
    // yielding A (X X^T) A^T = A A^T.
    // So we're looking for a "square root" A of the given covariance
    // matrix Sigma:
    val sqrtSigma = LinearAlgebra.cholesky(Sigma)
    val samples: DenseMatrix[Double] =
      sqrtSigma * DenseMatrix.randn(mu.size, numSamples)
    // Due to the row-major storage order of (dense) matrices it's probably
    // best to use row-wise scalar addition instead of column-wise vector
    // addition:
    for (i <- 0 until mu.size)
      samples(i, ::) += mu(i)

    samples
  }

  /** Returns a random integer in the range [0..max). */
  def randi(max : Int)(implicit mt : MersenneTwisterFast) = mt.synchronized {
    mt.nextInt(max);
  }

  /** Returns an array of n random integers in the range [0..max). */
  def randi(max : Int, n : Int)(implicit mt : MersenneTwisterFast) = mt.synchronized {
    val v = new Array[Int](n);
    var i = 0;
    while (i < v.length) {
      v(i) = mt.nextInt(max);
      i += 1;
    }
    v;
  }
}

/**
 * Random number generation.  The seed for the global random number generator
 * can be set with the system property during Java invocation, e.g. by
 * -Dscalala.library.random.seed=1l
 *
 * @author dramage
 */
object Random extends Random {
  lazy val seed : Long = {
    val prop = System.getProperty("scalala.library.random.seed");
    if (prop != null) prop.toLong else System.currentTimeMillis;
  }

  implicit val mt : MersenneTwisterFast =
    new MersenneTwisterFast(seed);
}
