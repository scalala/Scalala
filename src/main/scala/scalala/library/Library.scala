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

import scalala.counter.MapCounter;

import scalala.generic._;
import scalala.generic.math._;

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

  def exp[V,That](value : V)(implicit exp : CanExp[V,That]) : That =
    exp(value);

  def log[V,That](value : V)(implicit log : CanLog[V,That]) : That =
    log(value);

  def mean[V,That](value : V)(implicit mean : CanMean[V,That]) : That =
    mean(value);

  //
  // Constructors
  //

  /** Counts the given items. */
  def count[X](items : Traversable[X]) : MapCounter[X,Int] = {
    val c = MapCounter[X,Int]();
    for (item <- items) {
      c(item) += 1;
    }
    c;
  }

  /** Counts the given items. */
  def count[X](items : Iterator[X]) : MapCounter[X,Int] = {
    val c = MapCounter[X,Int]();
    for (item <- items) {
      c(item) += 1;
    }
    c;
  }


}

object Library extends Library;
