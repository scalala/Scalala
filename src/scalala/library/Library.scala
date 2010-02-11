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
package scalala.library;

import scalala.tensor.dense.DenseVector;

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
  final def log(v : Double) : Double = Math.log(v);
  
  /** Exponentiate a value. */
  final def exp(v : Double) : Double = Math.exp(v);

  final def pow(base : Double, exponent : Double) : Double =
    Math.pow(base, exponent);

  /** Alias for x.isNaN. */
  final def isnan(x : Double) = x.isNaN;

  /** Alias for Math.sqrt. */
  final def sqrt(x : Double) = Math.sqrt(x);
  
  /** Alias for Double.NaN */
  final val NaN = Double.NaN;
  
  /** Alias for Double.NaN */
  final val nan = NaN;
  
  /** Alias for Double.PositiveInfinity */
  final val Inf = Double.PositiveInfinity;
  
  /** Alias for Double.PositiveInfinity */
  final val inf = Inf;
}

/**
 * An object with access to the Library trait members.
 * 
 * @author dramage
 */
object Library extends Library { }
