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

import generic.collection.{CanViewAsVector};

/**
 * Matlab-like statistical methods.
 *
 * @author dramage
 */
trait Statistics {

  /**
   * Computes the Pearson correlation coefficient between the two vectors.
   * Code adapted excerpted from Wikipedia:
   *   http://en.wikipedia.org/wiki/Pearson%27s_correlation_coefficient
   */
  def corr[X,XV,Y,YV](x : X, y : Y)
  (implicit xvt : CanViewAsVector[X,XV], yvt : CanViewAsVector[Y,YV]) : Double = {
    val _x = xvt(x);
    val _y = yvt(y);
    @inline implicit def xvtod(v : XV) = _x.scalar.toDouble(v);
    @inline implicit def yvtod(v : YV) = _y.scalar.toDouble(v);
    require(_x.size == _y.size, "Vectors must have same length");
    
    val N = _x.size;
    if (N == 0) {
      return Double.NaN;
    }
    
    var sum_sq_x = 0.0;
    var sum_sq_y = 0.0;
    var sum_coproduct = 0.0;
    var mean_x : Double = _x(0);
    var mean_y : Double = _y(0);
    var i = 2;
    while (i <= N) {
      val sweep = (i - 1.0) / i;
      val delta_x = _x(i-1) - mean_x;
      val delta_y = _y(i-1) - mean_y;
      sum_sq_x += (delta_x * delta_x * sweep);
      sum_sq_y += (delta_y * delta_y * sweep);
      sum_coproduct += (delta_x * delta_y * sweep);
      mean_x += (delta_x / i);
      mean_y += (delta_y / i);
      i += 1;
    }
    val pop_sd_x = math.sqrt( sum_sq_x / N );
    val pop_sd_y = math.sqrt( sum_sq_y / N );
    val cov_x_y = sum_coproduct / N;
    return cov_x_y / (pop_sd_x * pop_sd_y);
  }
  
  /**
   * Computes Kendall's Tau correlation coefficient between the two vectors
   * x and y.  The measure is a correlation based on ranks, essentially counting
   * the number of concordant minus discordant pairs, normalized by the number
   * of pairs.  Breaking ties is tricky and important.  See:
   *
   *  "A modification of Kendall's Tau for the case of arbitrary ties in both
   *   rankings" by L. M. Adler, 1957.  http://www.jstor.org/stable/2281397?seq=1
   */
  def kendall[X,XV,Y,YV](x : X, y : Y)
  (implicit xvt : CanViewAsVector[X,XV], yvt : CanViewAsVector[Y,YV]) : Double = {
    val _x = xvt(x); @inline implicit def xvtod(v : XV) = _x.scalar.toDouble(v);
    val _y = yvt(y); @inline implicit def yvtod(v : YV) = _y.scalar.toDouble(v);
    require(_x.size == _y.size, "Vectors must have same length");
    
    val N = _x.size;
    if (N == 0) {
      return Double.NaN;
    }
    
    // keep track of ties in x and in y
    val xties = new scala.collection.mutable.HashMap[Double,scala.collection.mutable.HashSet[Int]];
    val yties = new scala.collection.mutable.HashMap[Double,scala.collection.mutable.HashSet[Int]];
   
    var numer = 0.0;
    for (i <- 0 until N; j <- 0 until i) {
      if (_x(i) == _x(j)) {
        val s = xties.getOrElseUpdate(_x(i), new scala.collection.mutable.HashSet[Int]);
        s += i;
        s += j;
      }
      if (_y(i) == _y(j)) {
        val s = yties.getOrElseUpdate(_y(i), new scala.collection.mutable.HashSet[Int]);
        s += i;
        s += j;
      }
      numer += math.signum(_x(i) - _x(j)) * math.signum(_y(i) - _y(j));
    }
    
    var denom = N * (N - 1.0) / 2.0;
    var xdenom = xties.valuesIterator.map(s => s.size * (s.size - 1.0)).sum / 2.0;
    var ydenom = yties.valuesIterator.map(s => s.size * (s.size - 1.0)).sum / 2.0;
    
    return numer / math.sqrt((denom - xdenom) * (denom - ydenom));
  }
  
  /** Returns n choose k, how many ways to pick k objects from n. */
  def nchoosek(n : Int, k : Int) : Long = {
    var aa = 0.0;
    var ai = k+1;
    while (ai <= n) aa += math.log(ai);
    
    var bb = 0.0;
    var bi = 2;
    while (bi <= n-k) bb += math.log(bi);
    
    math.exp(aa - bb).round;
  }
  
  /** Returns n factorial, the number of orderings of n objects. */
  def factorial(n : Int) : Long = {
    var i = n;
    var rv = 1l;
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
  def binomialCDF(n:Int,p:Double)(x:Int) = {
    var rv = 0.0;
    var i = 0;
    while (i < x) {
      rv += nchoosek(n,i) * math.pow(p,i) * math.pow(1-p,n-i);
      i += 1;
    }
    rv;
  }
  
  
}

/**
 * An object with access to the Statistics trait members.
 * 
 * @author dramage
 */
object Statistics extends Statistics { }

