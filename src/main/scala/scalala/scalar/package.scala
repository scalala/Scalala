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

/**
 * Defines i.
 *
 * @author dramage
 */
package object scalar {
  val i = Complex(0,1);

  class RichScalar(value : Double) {
    def + (c : Complex) : Complex = Complex(value,0) + c;
    def - (c : Complex) : Complex = Complex(value,0) - c;
    def * (c : Complex) : Complex = Complex(value,0) * c;
    def / (c : Complex) : Complex = Complex(value,0) / c;
  }

  implicit def richInt(value : Int) =
    new RichScalar(value);

  implicit def richLong(value : Long) =
    new RichScalar(value);

  implicit def richFloat(value : Float) =
    new RichScalar(value);

  implicit def richDouble(value : Double) =
    new RichScalar(value);
}
