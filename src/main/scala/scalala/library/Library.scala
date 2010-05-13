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

import tensor.dense.DenseVector;

/**
 * Basic data type construction and manipulation.
 * 
 * @author dramage
 */
trait Library {
  /**
   * DenseVector literal, equivalent to
   * <pre>DenseVector(values.size)(values :_*)</pre>.
   */
  def Vector(values : Double*) =
    DenseVector(values.size)(values :_*);
  
  //
  // basic scala ops from Math.
  //
  
  /** Log a value. */
  final def log(v : Double) : Double = math.log(v);
  
  /** Exponentiate a value. */
  final def exp(v : Double) : Double = math.exp(v);

  final def pow(base : Double, exponent : Double) : Double =
    math.pow(base, exponent);

  /** Alias for x.isNaN. */
  final def isnan(x : Double) = x.isNaN;

  /** Alias for math.sqrt. */
  final def sqrt(x : Double) = math.sqrt(x);
  
  /** Alias for Double.NaN */
  final val NaN = Double.NaN;
  
  /** Alias for Double.NaN */
  final val nan = NaN;
  
  /** Alias for Double.PositiveInfinity */
  final val Inf = Double.PositiveInfinity;
  
  /** Alias for Double.PositiveInfinity */
  final val inf = Inf;

  /**
   * Sums in log space.
   * @return log(exp(a) + exp(b))
   */
  def logSum(a : Double, b : Double) = {
    if(a == Double.NegativeInfinity) b
    else if (b == Double.NegativeInfinity) a
    else if(a < b) b + log(1 + exp(a-b))
    else a + log(1+exp(b-a));    
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   */
  def logSum(a: Double, b:Double, c: Double*):Double ={
    logSum(Array(a,b) ++ c);
  }


  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   */
  def logSum(iter:Iterator[Double], max: Double):Double = {
    max + log(iter.foldLeft(0.)( (a,b) => if(b == Double.NegativeInfinity) a else a+exp( b - max )))
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSum(a:Seq[Double]):Double = {
    a.length match {
      case 0 => Double.NegativeInfinity;
      case 1 => a(0)
      case 2 => logSum(a(0),a(1));
      case _ =>
        val m = a reduceLeft(_ max _);
        if(m.isInfinite) m
        else m + log(a.foldLeft(0.)( (a,b) => a+exp( b - m )))
    }
  }

}

/**
 * An object with access to the Library trait members.
 * 
 * @author dramage
 */
object Library extends Library { }
