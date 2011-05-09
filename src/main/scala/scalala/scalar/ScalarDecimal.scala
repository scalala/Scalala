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
package scalar;

import scala.annotation.implicitNotFound;
import scalala.collection.sparse.DefaultArrayValue;

/**
 * Get a decimal value for the given scalar type.
 *
 * @author dramage
 */
@implicitNotFound(msg="No decimal defined for ${SV}")
trait ScalarDecimal[SV,DV] {
  def decimal : Scalar[DV];
}

object ScalarDecimal {
  class DefaultScalarDecimal[SV:Scalar,DV:Scalar] extends ScalarDecimal[SV,DV] {
    def decimal = implicitly[Scalar[DV]];
  }

  implicit object ScalarDecimalBD extends DefaultScalarDecimal[Boolean,Double];
  implicit object ScalarDecimalID extends DefaultScalarDecimal[Int,Double];
  implicit object ScalarDecimalSD extends DefaultScalarDecimal[Short,Double];
  implicit object ScalarDecimalLD extends DefaultScalarDecimal[Long,Double];
  implicit object ScalarDecimalFD extends DefaultScalarDecimal[Float,Double];
  implicit object ScalarDecimalDD extends DefaultScalarDecimal[Double,Double];
}

