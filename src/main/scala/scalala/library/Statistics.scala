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

import tensor.{Tensor,Vector,Matrix};

/**
 * Some statistical routines.
 * 
 * @author dramage
 */
trait Statistics extends Library with Traversables with Vectors with Implicits {
  /**
   * Computes the Pearson correlation coefficient between the two vectors.
   * Code adapted excerpted from Wikipedia:
   *   http://en.wikipedia.org/wiki/Pearson%27s_correlation_coefficient
   */
  def corr(x : Vector, y : Vector) : Double = {
    if (x.size != y.size) {
      throw new IllegalArgumentException("Vectors must have same length");
    }
    if (x.size == 0) {
      throw new IllegalArgumentException("Vectors must contain data");
    }
    
    val N = x.size;
    var sum_sq_x = 0.0;
    var sum_sq_y = 0.0;
    var sum_coproduct = 0.0;
    var mean_x = x(0);
    var mean_y = y(0);
    for (i <- 2 to N) {
      val sweep = (i - 1.0) / i;
      val delta_x = x(i-1) - mean_x;
      val delta_y = y(i-1) - mean_y;
      sum_sq_x += (delta_x * delta_x * sweep);
      sum_sq_y += (delta_y * delta_y * sweep);
      sum_coproduct += (delta_x * delta_y * sweep);
      mean_x += (delta_x / i);
      mean_y += (delta_y / i);
    }
    val pop_sd_x = Math.sqrt( sum_sq_x / N );
    val pop_sd_y = Math.sqrt( sum_sq_y / N );
    val cov_x_y = sum_coproduct / N;
    return cov_x_y / (pop_sd_x * pop_sd_y);
  }
  
  /** Returns n choose k, how many ways to pick k objects from n. */
  def nchoosek(n : Int, k : Int) : Long =
    exp(sum(log(k+1 to n))-sum(log(2 to n-k))).round
  
  /** Returns n factorial, the number of orderings of n objects. */
  def factorial(n : Int) : Long = {
    var i = n;
    var rv = 1;
    while (i > 1) {
      rv *= i;
      i -= 1;
    }
    rv;
  }
  
  /**
   * Returns the cumulative distribution function of the binomial evaluated
   * at x; i.e. returns the probability that at most x draws out of n draws
   * of a binomial with paramater p come up heads.
   */
  def binomialCDF(n:Int,p:Double)(x:Double) =
    sum(for (i <- 0 to x.toInt) yield nchoosek(n,i) * pow(p,i) * pow(1-p,n-i));
}

/**
 * An object with access to the Statistics trait members.
 * 
 * @author dramage
 */
object Statistics extends Statistics { }
