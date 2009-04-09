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

import scalala.tensor.{Tensor,Vector};

import scalala.tensor.operators.TensorOp;

/**
 * Basic vector functions.
 * 
 * @author dramage
 */
trait Vectors extends Library with Operators {
  /** 100 evenly spaced points between a and b */
  def linspace(a : Double, b : Double) : Vector =
    linspace(a,b,100);
  
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
  
  /** Returns the sum of the elements and how many there were. */
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
   * Returns the sum of the elements in the map and how many there were.  If the
   * map has an infinite domain, return -1 as count.
   */
  private def sumcount[I](v : PartialMap[I,Double]) : (Double,Int) = {
    val domainsize =
      if (v.domain.isFinite)
        v.domain.asInstanceOf[scalala.collection.domain.FiniteDomain[_]].size
      else -1;
      
    if (v.default == 0.0) {
      return (sum(v.activeValues),domainsize);
    } else if (v.domain.isFinite) {
      // non-zero default but finite domain
      val (sum,count) = sumcount(v.activeValues);
      return (sum + ((domainsize - count) * v.default), domainsize);
    } else {
      // infinite domain with non-zero default
      return (Math.signum(v.default) * Double.PositiveInfinity, domainsize);
    }
  }
  
  /** Returns the sum of the elements of iterator. */
  def sum(v : Iterator[Double]) : Double =
    sumcount(v)._1;
  
  /** Returns the sum of the elements of collection. */
  def sum(v : Collection[Double]) : Double =
    sum(v.elements);
  
  /** Returns the sum of the values of the map. */
  def sum[I](v : PartialMap[I,Double]) : Double =
    sumcount(v)._1;
  
  /** Returns the sum of the values of the value of the operation. */
  def sum[I,T<:Tensor[I]](op : TensorOp[I,T]) : Double =
    sum(op.value);
  
  /** Log each element of a vector or matrix */
  def log[I](v : PartialMap[I,Double]) = v.map(Math.log _);
  def log(v : Iterator[Double]) = v.map(Math.log);
  def log[S<:Collection[Double]](v : S) = v.map(Math.log);
  def log[I,T<:Tensor[I]](op : TensorOp[I,T]) : Double =
    log(op.value);
  
  /** The maximum active value of the map (does not consider default). */
  def max[I](v : PartialMap[I,Double]) : Double =
    v.activeValues.reduceLeft(Math.max);
  def max(v : Iterator[Double]) : Double = v.reduceLeft(Math.max);
  def max(v : Collection[Double]) : Double = max(v.elements);
  def max[I,T<:Tensor[I]](op : TensorOp[I,T]) : Double = max(op.value);
  
  /** The minimum active value of the map (does not consider default). */
  def min[I](v : PartialMap[I,Double]) : Double =
    v.activeValues.reduceLeft(Math.min);

  def min(v : Iterator[Double]) : Double = v.reduceLeft(Math.min);
  def min(v : Collection[Double]) : Double = min(v.elements);
  def min[I,T<:Tensor[I]](op : TensorOp[I,T]) : Double = min(op.value);
  
  /**
   * Returns the sum of the squares of the elements of the vector.
   */
  def sumsq[I](v : PartialMap[I,Double]) : Double = sum(v.map((x:Double) => x*x));
  def sumsq(v : Iterator[Double]) = sum(v.map(x => x*x));
  def sumsq(v : Seq[Double]) = sum(v.map(x => x*x));
  def sumsq[I,T<:Tensor[I]](op : TensorOp[I,T]) : Double = sumsq(op.value);
  
  /** Returns the mean of the vector: sum(v) / v.size. */
  def mean[I](v : PartialMap[I,Double]) : Double = {
    val (sum,count) = sumcount(v);
    if (count < 0)
      throw new IllegalArgumentException("Cannot take mean of infinite domain");
    sum / count;
  }
  
  /** Returns the mean of the given elements. */
  def mean(v : Iterator[Double]) : Double = {
    val (sum,count) = sumcount(v);
    sum / count;
  }
  
  /** Returns the mean of the given elements. */
  def mean(v : Seq[Double]) : Double = mean(v.elements);
  
  def mean[I,T<:Tensor[I]](op : TensorOp[I,T]) : Double = mean(op.value);
  
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
    sqrt(sumsq(v - mean(v) value) / (v.size - 1));
  
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
      return max(v.map((x:Double) => Math.abs(x)));
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
