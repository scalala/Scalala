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

import scalala.Vector;
import scalala.ScalalaTest._;

trait Vectors extends Library with Operators with Implicits {
  /** 100 evenly spaced points between a and b */
  def linspace(a : Double, b : Double) : Vector = linspace(a,b,100);
  
  /** n evenly spaced points between a and b */
  def linspace(a : Double, b : Double, n : Int) : Vector = {
    val v = DenseVector(n);
    val delta = (b - a) / (n - 1.0);
    for (i <- 0 until n) { v.set(i, a + i*delta); }
    return v;
  }
  
  /** A vector of ones of the given size */
  def ones(n : Int) : Vector = {
    val v = DenseVector(n);
    for (i <- 0 until n) v.set(i,1.0);
    return v;
  }

  /** A vector of zeros of the given size */
  def zeros(n : Int) : Vector = DenseVector(n);
  
  /** Sums the elements of a vector */
  def sum(v : Vector) : Double = {
    var sum = 0.0;
    for (e <- v.elements) { sum += e.get }
    return sum;
  }
  
  type HasDoubleValues = {
    def values : Iterator[Double];
  }
  
  def sum(v : HasDoubleValues) : Double = v.values.foldLeft(0.0)(_ + _);
  
  /** Log each element of a vector or matrix */
  def log(v : Vector) : Vector = v.map(Math.log);

  /** The maximum element of a vector */
  def max(v : Vector) : Double = {
    var m = Double.MinValue;
    for (e <- v.elements) {
      m = Math.max(m,e.get);
    }
    return m;
  }
  
  /** The minimum element of a vector */
  def min(v : Vector) : Double = {
    var m = Double.MaxValue;
    for (e <- v.elements) {
      m = Math.min(m,e.get);
    }
    return m;
  }

  /**
   * Returns the sum of the squares of the elements of the vector.
   */
  def sumsq(v : Vector) : Double = {
    var s = 0.0;
    v.foreach(e => s += e.get * e.get )
    return s;
  }
  
  /** Returns the mean of the vector: sum(v) / v.size. */
  def mean(v : Vector) : Double =
    sum(v) / v.size;
  
  /** Returns the sum vector of a bunch of vectors. */
  def sum(vectors : Seq[Vector]) : Vector = {
    val sum = vectors(0).copy;
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
    sqrt(sumsq (v - mean(v)) / (v.size - 1));
  
  /** Returns the n'th euclidean norm of the given vector. */
  def norm(v : Vector, n : Int) : Double = {
    if (n == 1) {
      return v.elements.map(e => Math.abs(e.get)).foldLeft(0.0)(_+_);
    } else if (n == 2) {
      return Math.sqrt(v.elements.map(x => x.get * x.get).foldLeft(0.0)(_+_));
    } else if (n % 2 == 0) {
      return Math.pow(v.elements.map(x => Math.pow(x.get,n)).foldLeft(0.0)(_+_),1.0/n);
    } else if (n % 2 == 1) {
      return Math.pow(v.elements.map(x => Math.pow(Math.abs(x.get),n)).foldLeft(0.0)(_+_), 1.0/n);
    } else {
      throw new UnsupportedOperationException();
    }
  }
  
  def _norm_test() {
    val v = Vector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertEquals(norm _, v, 1, 3.6577);
    assertEquals(norm _, v, 2, 2.0915);
    assertEquals(norm _, v, 3, 1.8405);
    assertEquals(norm _, v, 4, 1.7541);
    assertEquals(norm _, v, 5, 1.7146);
    assertEquals(norm _, v, 6, 1.6940);
  }
  
}
