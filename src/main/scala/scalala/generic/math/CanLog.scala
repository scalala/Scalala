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
 * Constructiond delegate for log(A).
 * 
 * @author dramage
 */
trait CanLog[-A,+RV] extends UnaryOp[A,RV];

object CanLog {
  implicit object OpI extends CanLog[Int,Double] {
    def apply(v : Int) = scala.math.log(v);
  }

  implicit object OpL extends CanLog[Long,Double] {
    def apply(v : Long) = scala.math.log(v);
  }

  implicit object OpF extends CanLog[Float,Double] {
    def apply(v : Float) = scala.math.log(v);
  }

  implicit object OpD extends CanLog[Double,Double] {
    def apply(v : Double) = scala.math.log(v);
  }

  class OpMapValues[From,A,B,To](implicit op : CanLog[A,B], map : CanMapValues[From,A,B,To]) extends CanLog[From,To] {
    def apply(v : From) = map.map(v, op);
  }

  implicit def opMapValues[From,A,B,To](implicit map : CanMapValues[From,A,B,To], op : CanLog[A,B])
  : CanLog[From,To] = new OpMapValues[From,A,B,To]()(op, map);

  implicit object OpArrayI extends OpMapValues[Array[Int],Int,Double,Array[Double]]()(OpI,CanMapValues.OpArrayID);
  implicit object OpArrayL extends OpMapValues[Array[Long],Long,Double,Array[Double]]()(OpL,CanMapValues.OpArrayLD);
  implicit object OpArrayF extends OpMapValues[Array[Float],Float,Double,Array[Double]]()(OpF,CanMapValues.OpArrayFD);
  implicit object OpArrayD extends OpMapValues[Array[Double],Double,Double,Array[Double]]()(OpD,CanMapValues.OpArrayDD);
}
