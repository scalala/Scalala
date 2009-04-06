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
package scalala.library

import scalala.collection.PartialMap;
import scalala.tensor.Vector;

/**
 * Basic vector functions.
 * 
 * @author dramage
 */
trait Vectors extends Library with Operators {
  /** 100 evenly spaced points between a and b */
  def linspace(a : Double, b : Double) : Vector = linspace(a,b,100);
  
  /** n evenly spaced points between a and b */
  def linspace(a : Double, b : Double, n : Int) : Vector = {
    val delta = (b - a) / (n - 1.0);
    Vector(Array.fromFunction(i => a + i*delta)(n));
  }
  
  /** A vector of ones of the given size */
  def ones(n : Int) : Vector = {
    Vector(Array.fromFunction(i => 1.0)(n));
  }

  /** A vector of zeros of the given size */
  def zeros(n : Int) : Vector = DenseVector(n);
  
  /** Sums the elements of a vector */
  def sum[I](v : PartialMap[I,Double]) : Double = {
    var sum = 0.0;
    if (v.default == 0.0) {
      for (x <- v.activeValues) { sum += x; } 
    } else if (v.domain.isFinite) {
      var numActive = 0;
      for (x <- v.activeValues) {
        sum += x;
        numActive += 1;
      } 
      sum += (v.domain.asInstanceOf[scalala.collection.domain.FiniteDomain[_]].size - numActive) * v.default;
    } else {
      for (x <- v.values) { sum += x; }
    }
    return sum;
  }
  
  /** Log each element of a vector or matrix */
  def log(v : Vector) : Vector = {
    val x = v.copy.asInstanceOf[Vector];
    x := v.map(Math.log _);
    x;
  }

  /** The maximum active value of the map (does not consider default). */
  def max[I](v : PartialMap[I,Double]) : Double =
    v.activeValues.foldLeft(Double.MinValue)(Math.max);
  
  /** The minimum active value of the map (does not consider default). */
  def min[I](v : PartialMap[I,Double]) : Double =
    v.activeValues.foldLeft(Double.MaxValue)(Math.min);

  /**
   * Returns the sum of the squares of the elements of the vector.
   */
  def sumsq[I](v : PartialMap[I,Double]) : Double =
    sum(v.map(x => x*x));
  
  /** Returns the mean of the vector: sum(v) / v.size. */
  def mean[I](v : PartialMap[I,Double]) : Double =
    sum(v) / v.domain.asInstanceOf[scalala.collection.domain.FiniteDomain[_]].size;
  
  /** Returns the sum vector of a bunch of vectors. */
  def sum(vectors : Seq[Vector]) : Vector = {
    val sum = vectors(0).copy.asInstanceOf[Vector];
    for (vector <- vectors.elements.drop(1)) {
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
  
  /**
   * Returns the standard deviation of the values in the vector:
   * sqrt(sumsq (v - mean(v)) / (v.size - 1)).
   */
  def std(v : Vector) : Double =
    sqrt(sumsq(v - mean(v)) / (v.size - 1));
  
  /** Returns the n'th euclidean norm of the given vector. */
  def norm[I](v : PartialMap[I,Double], n : Double) : Double = {
    if (n == 1) {
      return sum(v.map(Math.abs));
    } else if (n == 2) {
      return sqrt(sum(v.map(x => x * x)));
    } else if (n % 2 == 0) {
      return Math.pow(sum(v.map(x => Math.pow(x, n))), 1.0 / n);
    } else if (n % 2 == 1) {
      return Math.pow(sum(v.map(x => Math.pow(Math.abs(x), n))), 1.0 / n);
    } else if (n == Double.PositiveInfinity) {
      return max(v.map(Math.abs));
    } else {
      throw new UnsupportedOperationException();
    }
  }
}

/**
 * Some tests for the vectors package.
 * 
 * @author dramage
 */
trait VectorsTest extends Library with Vectors {  
  import scalala.ScalalaTest._;
  
  def _norm_test() {
    val v = Vector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertEquals(norm(v,1), 3.6577, 1e-4);
    assertEquals(norm(v,2), 2.0915, 1e-4);
    assertEquals(norm(v,3), 1.8405, 1e-4);
    assertEquals(norm(v,4), 1.7541, 1e-4);
    assertEquals(norm(v,5), 1.7146, 1e-4);
    assertEquals(norm(v,6), 1.6940, 1e-4);
    assertEquals(norm(v,Double.PositiveInfinity), 1.6656, 1e-4);
  }
}
