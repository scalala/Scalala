/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */

package scalala;
package generic;
package math;

import collection.CanMapValues;

/**
 * Constructiond delegate for exp(A).
 *
 * @author dramage
 */
trait CanExp[-A,+RV] extends UnaryOp[A,RV];

object CanExp {
  type Op[A,RV] = CanExp[A,RV];

  implicit object OpI extends Op[Int,Double]
  { def apply(v : Int) = scala.math.exp(v); }

  implicit object OpL extends Op[Long,Double]
  { def apply(v : Long) = scala.math.exp(v); }

  implicit object OpF extends Op[Float,Double]
  { def apply(v : Float) = scala.math.exp(v); }

  implicit object OpD extends Op[Double,Double]
  { def apply(v : Double) = scala.math.exp(v); }

  // TODO: this causes all sorts of problems
//  implicit def opMapValues[From,A,B,To]
//  (implicit op : Op[A,B], map : CanMapValues[From,A,B,To]) =
//    new OpMapValues[From,A,B,To];

  class OpMapValues[From,@specialized A,@specialized B,To]
  (implicit op : Op[A,B], map : CanMapValues[From,A,B,To])
  extends Op[From,To] {
    def apply(v : From) = map(v, op);
  }

  implicit object OpArrayI extends OpMapValues[Array[Int],Int,Double,Array[Double]]()(OpI,CanMapValues.OpArrayID);
  implicit object OpArrayL extends OpMapValues[Array[Long],Long,Double,Array[Double]]()(OpL,CanMapValues.OpArrayLD);
  implicit object OpArrayF extends OpMapValues[Array[Float],Float,Double,Array[Double]]()(OpF,CanMapValues.OpArrayFD);
  implicit object OpArrayD extends OpMapValues[Array[Double],Double,Double,Array[Double]]()(OpD,CanMapValues.OpArrayDD);
}
