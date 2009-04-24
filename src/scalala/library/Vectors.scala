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
  
  /** Returns the sum of the elements of iterator. */
  def sum(v : Iterator[Double]) : Double =
    sumcount(v)._1;
  
  /** Returns the sum of the elements of collection. */
  def sum(v : Iterable[Double]) : Double =
    sum(v.elements);
  
  /** Returns the sum of the values of the map. */
  def sum[I](v : PartialMap[I,Double]) : Double =
    sumcount(v)._1;
  
  /** Returns the sum of the values of the value of the operation. */
  def sum[I](op : TensorOp[I]) : Double =
    sum(op.value);
  
  /** Log each element of a vector or matrix */
  def log[I](v : PartialMap[I,Double]) = v.map(Math.log _);
  def log(v : Iterator[Double]) = v.map(Math.log);
  def log(v : Iterable[Double]) = v.map(Math.log);
  def log(v : Seq[Double]) = v.map(Math.log);
  def log[I](op : TensorOp[I]) : Double = log(op.value);
  
  /** The maximum value of the map. */
  def max[I](v : PartialMap[I,Double]) : Double = {
    val maxActive = max(v.activeValues);
    if (!(v.domain -- v.activeDomain).isEmpty) {
      Math.max(v.default, maxActive)
    } else {
      maxActive;
    }
  }
  
  def max(v : Iterator[Double]) : Double = v.reduceLeft(Math.max);
  def max(v : Iterable[Double]) : Double = max(v.elements);
  def max[I](op : TensorOp[I]) : Double = max(op.value);

  /** Returns a key i of the map such that v(i)=max(v). */
  def argmax[I](v : PartialMap[I,Double]) : I = {
    val nonActive = v.domain -- v.activeDomain;
    if (nonActive.isEmpty) {
      argmax(v.activeElements);
    } else {
      argmax(v.activeElements ++ Iterator.single((nonActive.elements.next, v.default)));
    }
  }
  
  def argmax[I](v : Iterator[(I,Double)]) : I =
    v.reduceLeft((tupA,tupB) => if (tupA._2 >= tupB._2) tupA else tupB)._1;
  def argmax[I](v : Iterator[Double]) : Int =
    argmax(v.zipWithIndex.map(tup => (tup._2,tup._1)));
  
  /**
   * Due to iteration ordering requirements, argmax is ill-defined on general Iterables,
   * but perfectly valid on sequences.
   */
  def argmax[I](v : Seq[Double]) : Int =
    argmax(v.elements);
  
  def argmax[I](op : TensorOp[I]) : I =
    argmax(op.value);
  
  /** The minimum value of the map. */
  def min[I](v : PartialMap[I,Double]) : Double = {
    val minActive = min(v.activeValues);
    if (!(v.domain -- v.activeDomain).isEmpty) {
      Math.min(v.default, minActive)
    } else {
      minActive;
    }
  }

  def min(v : Iterator[Double]) : Double = v.reduceLeft(Math.min);
  def min(v : Iterable[Double]) : Double = min(v.elements);
  def min[I](op : TensorOp[I]) : Double = min(op.value);
  
  /** Returns a key i of the map such that v(i)=min(v). */
  def argmin[I](v : PartialMap[I,Double]) : I = {
    val nonActive = v.domain -- v.activeDomain;
    if (nonActive.isEmpty) {
      argmin(v.activeElements);
    } else {
      argmin(v.activeElements ++ Iterator.single((nonActive.elements.next, v.default)));
    }
  }
  
  def argmin[I](v : Iterator[(I,Double)]) : I =
    v.reduceLeft((tupA,tupB) => if (tupA._2 <= tupB._2) tupA else tupB)._1;
  def argmin[I](v : Iterator[Double]) : Int =
    argmin(v.zipWithIndex.map(tup => (tup._2,tup._1)));
  
  /**
   * Due to iteration ordering requirements, argmax is ill-defined on general Iterables,
   * but perfectly valid on sequences.
   */
  def argmin[I](v : Seq[Double]) : Int =
    argmin(v.elements);
  def argmin[I](op : TensorOp[I]) : I =
    argmin(op.value);
  
  /**
   * Returns the sum of the squares of the elements of the vector.
   */
  def sumsq[I](v : PartialMap[I,Double]) : Double = sum(v.map((x:Double) => x*x));
  def sumsq(v : Iterator[Double]) = sum(v.map(x => x*x));
  def sumsq(v : Iterable[Double]) = sum(v.map(x => x*x));
  def sumsq[I](op : TensorOp[I]) : Double = sumsq(op.value);

  /** Returns the mean of the given elements.  Based on a Knuth online algorithm. */
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
  
  /** Returns the mean of the given elements. */
  def mean(v : Iterable[Double]) : Double = mean(v.elements);
  
  def mean[I](op : TensorOp[I]) : Double = mean(op.value);
  
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
    variance(v.elements);
  
  /**
   * The variance of the values in the given map accounting for default values.
   */
  def variance[I](v : PartialMap[I,Double]) : Double = {
    if (v.activeDomain.size == v.domain.size) {
      variance(v.values);
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
      return max(v.map((x:Double) => Math.abs(x)));
    } else {
      throw new UnsupportedOperationException();
    }
  }
  
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
  
}

/**
 * Some tests for the vectors package.
 * 
 * @author dramage
 */
trait VectorsTest extends Library with Vectors with Implicits with Random with scalala.ScalalaTest {  
  
 test("Tensor:Moments") {
    val v = SparseVector(1000);
    v += 1;
    v(0 until 100) = rand(100).values.collect;
    assertEquals(mean(v.toArray), mean(v), 1e-10);
    assertEquals(variance(v.toArray), variance(v), 1e-10);
    assertEquals(std(v.toArray), std(v), 1e-10);
    
    assertEquals((1 + 3 + 22 + 17) / 4.0, mean(Vector(1,3,22,17)), 1e-10);
    
    assertEquals(0.08749136216928063,
                 variance(Vector(0.29854716128994807,0.9984567314422015,0.3056949899038196,
                                 0.8748240977963917,0.6866542395503176,0.48871321020847913,
                                 0.23221169231853678,0.992966911646403,0.8839015907147733,
                                 0.6435495508602755)),
                 1e-10);
  }
  
  test("Tensor:Norm") {
    val v = Vector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertEquals(norm(v,1), 3.6577, 1e-4);
    assertEquals(norm(v,2), 2.0915, 1e-4);
    assertEquals(norm(v,3), 1.8405, 1e-4);
    assertEquals(norm(v,4), 1.7541, 1e-4);
    assertEquals(norm(v,5), 1.7146, 1e-4);
    assertEquals(norm(v,6), 1.6940, 1e-4);
    assertEquals(norm(v,Double.PositiveInfinity), 1.6656, 1e-4);
  }
  
  test("Tensor:MinMax") {
    val v = SparseVector(10);
    v(3) = 1;
    assertEquals(1, max(v));
    assertEquals(3, argmax(v));
    assertEquals(0, min(v));
    assertEquals(0, argmin(v));
    
    v += 2;
    v(3) = 1;
    assertEquals(2, max(v));
    assertEquals(0, argmax(v));
    assertEquals(1, min(v));
    assertEquals(3, argmin(v));
  }
}
