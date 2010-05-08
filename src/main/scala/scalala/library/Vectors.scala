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

import collection.PartialMap;
import tensor.operators.TensorShapes;
import tensor.{Tensor,Vector};
import tensor.operators.TensorSelfOp;
import tensor.dense.{DenseVector};
import tensor.operators.TensorOp;

/**
 * Basic vector functions.
 * 
 * @author dramage
 */
trait Vectors extends Library with PartialMaps with Operators {
  /** 100 evenly spaced points between a and b */
  def linspace(a : Double, b : Double) : DenseVector =
    linspace(a,b,100);
  
  /** n evenly spaced points between a and b */
  def linspace(a : Double, b : Double, n : Int) : DenseVector = {
    val delta = (b - a) / (n - 1.0);
    new DenseVector(Array.tabulate(n)(i => a + i*delta));
  }
  
  /** A vector of ones of the given size */
  def ones(n : Int) =
    DenseVector(n)(1.0);

  /** A vector of zeros of the given size */
  def zeros(n : Int) : Vector =
    DenseVector(n)(0.0);
  
  /** Returns the sum of the iterator and how many there were. */
  private def sumcount(v : Iterator[Double]) : (Double,Int) = {
    var sum = 0.0;
    var count = 0;
    for (x <- v) {
      sum += x;
      count += 1;
    }
    return (sum,count);
  }
  
  /**
   * Returns the sum of the iterator in the map and how many there were.  If the
   * map has an infinite domain, return -1 as count.
   */
  private def sumcount[I](v : PartialMap[I,Double]) : (Double,Int) = {
    if (v.default == 0.0) {
      return (sum(v.activeValues), v.domain.size);
    } else {
      // non-zero default but finite domain
      var (sum,count) = sumcount(v.activeValues);
      if (v.domain.size > count) {
        sum += ((v.domain.size - count) * v.default);
      }
      return (sum, v.domain.size);
    }
  }
  
  /** Returns the sum of the iterator of iterator. */
  def sum(v : Iterator[Double]) : Double =
    sumcount(v)._1;
  
  /** Returns the sum of the iterator of collection. */
  def sum(v : Iterable[Double]) : Double =
    sum(v.iterator);
  
  /** Returns the sum of the values of the map. */
  def sum[I](v : PartialMap[I,Double]) : Double =
    sumcount(v)._1;
  
  /** Log each element of a vector or matrix */
  def log[I](v : PartialMap[I,Double]) = v.map(Math.log _);

  import TensorShapes._;
  def sqrt[V<:Vector with TensorSelfOp[Int,V,Shape1Col]](vec: V):V = {
    val r : V = (vec:TensorSelfOp[Int,V,Shape1Col]).like;
    for( (k,v) <- vec.activeElements) {
      r(k) = sqrt(v);
    }
    r
  }


  
  /**
   * Returns the sum of the squares of the iterator of the vector.
   */
  def sumsq[I](v : PartialMap[I,Double]) : Double = sum(v.map((x:Double) => x*x));
  def sumsq(v : Iterator[Double]) = sum(v.map(x => x*x));
  def sumsq(v : Iterable[Double]) = sum(v.map(x => x*x));

  /** Returns the mean of the given iterator.  Based on a Knuth online algorithm. */
  def mean(v : Iterator[Double]) : Double = {
    var n = 0;
    var mean = 0.0;
    
    for (x <- v) {
      n += 1;
      val delta = x - mean;
      mean += delta / n;
    }
    
    return mean;
  }
  
  /** Returns the mean value of the partial map. */
  def mean[I](v : PartialMap[I,Double]) : Double = {
    val activeMean = mean(v.activeValues);
    val remaining = v.domain.size - v.activeDomain.size;
    if (remaining > 0) {
      (activeMean * v.activeDomain.size + v.default * remaining) / v.domain.size;
    } else {
      activeMean;
    }
  }
  
  /** Returns the mean of the given iterator. */
  def mean(v : Iterable[Double]) : Double = mean(v.iterator);
  
  /**
   * Online algorithm for variance computation from Knuth vol 2.
   * See also http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance.
   */
  def variance(v : Iterator[Double]) : Double = {
    var n = 0;
    var mean = 0.0;
    var m2 = 0.0;
    
    for (x <- v) {
      n += 1;
      val delta = x - mean;
      mean += delta / n;
      m2 += delta * (x - mean);
    }
    
    return m2 / (n - 1);
  }
  
  def variance(v : Iterable[Double]) : Double =
    variance(v.iterator);
  
  /**
   * The variance of the values in the given map accounting for default values.
   */
  def variance[I](v : PartialMap[I,Double]) : Double = {
    if (v.activeDomain.size == v.domain.size) {
      variance(v.valuesIterator);
    } else {
      val m = mean(v);
      sumsq(v.map((x:Double) => x - m)) / (v.domain.size - 1);
    }
  }
  
  /**
   * Returns the square root of the variance of the values.
   */
  def std(v : Iterator[Double]) : Double =
    sqrt(variance(v));
  
  /**
   * Returns the square root of the variance of the values.
   */
  def std(v : Iterable[Double]) : Double =
    sqrt(variance(v));
  
  /**
   * Returns the square root of the variance of the values.
   */
  def std[I](v : PartialMap[I,Double]) : Double =
    sqrt(variance(v));
  
  /** Returns the n'th euclidean norm of the given vector. */
  def norm[I](v : PartialMap[I,Double], n : Double) : Double = {
    if (n == 1) {
      return sum(v.map((x:Double) => Math.abs(x)));
    } else if (n == 2) {
      return sqrt(sum(v.map((x:Double) => x * x)));
    } else if (n % 2 == 0) {
      return Math.pow(sum(v.map((x:Double) => Math.pow(x, n))), 1.0 / n);
    } else if (n % 2 == 1) {
      return Math.pow(sum(v.map((x:Double) => Math.pow(Math.abs(x), n))), 1.0 / n);
    } else if (n == Double.PositiveInfinity) {
      return max(v.map((x : Double) => Math.abs(x)));
    } else {
      throw new UnsupportedOperationException();
    }
  }
  
  /** Returns the sum vector of a bunch of vectors. */
  def sum(vectors : Seq[Vector]) : Vector = {
    val sum = vectors(0).copy;
    for (vector <- vectors.iterator.drop(1)) {
      sum += vector;
    }
    sum;
  }
  
  /** Returns the mean vector of a bunch of vectors. */
  def mean(vectors : Seq[Vector]) : Vector = {
    val rv = sum(vectors);
    rv /= vectors.size;
    rv;
  }
  
}

/**
 * Basic vector functions.
 * 
 * @author bethard
 */
object Vectors extends Vectors { }
