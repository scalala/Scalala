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

import scalala.generic._;
import scalala.generic.math._;

import scalala.tensor.mutable.Tensor;

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

  /** Alias for math.exp. */
  final def exp(v : Double) : Double = scala.math.exp(v);

  /** Alias for math.pow. */
  final def pow(base : Double, exponent : Double) : Double =
    scala.math.pow(base, exponent);

  /** Alias for x.isNaN. */
  final def isnan(x : Double) = x.isNaN;

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

  /** Take the exp of the given value. */
  def exp[V,That](value : V)(implicit exp : CanExp[V,That]) : That =
    exp(value);

  /** Take the log of the given value. */
  def log[V,That](value : V)(implicit log : CanLog[V,That]) : That =
    log(value);

  /** Take the n-norm of the given values. */
  def mean[V,That](value : V)(implicit mean : CanMean[V,That]) : That =
    mean(value);

  /** Take the n-norm of the given value. */
  def norm[V](value : V, n : Double)(implicit norm : CanNorm[V]) : Double =
    norm(value, n);

  /** Take the sqrt of the given value. */
  def sqrt[V,That](value : V)(implicit sqrt : CanSqrt[V,That]) : That =
    sqrt(value);

  //
  // Constructors
  //

  /** Counts the given items. */
  def count[X](items : TraversableOnce[X]) : Tensor[X,Int] = {
    val m = scalala.tensor.mutable.Tensor1Col[X,Int]();
    for (item <- items) {
      m(item) += 1;
    }
    m;
  }
}

object Library extends Library;
