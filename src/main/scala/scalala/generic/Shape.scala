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

/**
 * Marker trait for the shape of an argument.
 *
 * @author dramage
 */
trait Shape[V,S];

trait LowPriorityShapeImplicits {
  /** The shape of any scalar is Unit. */
  @inline implicit def any[S]
  : Shape[S,Unit] = null;
}

object Shape extends LowPriorityShapeImplicits {

  /** Returns a string representation of the shape for the given value. */
  def shapeOf[V,VS](value : V)(implicit shape : Shape[V,VS], mf : Manifest[VS]) =
    mf.toString;

  /** The shape of any map is KeyShape=>ValShape. */
  @inline implicit def map[K,V,KS,VS]
  (implicit ks : Shape[K,KS], vs : Shape[V,VS])
  : Shape[Map[K,V],(KS => VS)] = null;

  /** The shape of any array is Unit=>TShape. */
  @inline implicit def array[T,TS]
  (implicit ts : Shape[T,TS])
  : Shape[Array[T],(Unit => TS)] = null;

}
