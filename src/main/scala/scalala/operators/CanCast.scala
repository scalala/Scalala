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
package operators;

import scala.annotation.implicitNotFound;

/**
 * Cast a value from type A to B.
 *
 * @author dramage
 */
@implicitNotFound(msg="Could not cast ${A} to ${B}")
trait CanCast[-A,+B] extends (A => B);

object CanCast {
  implicit object CanCastII extends CanCast[Int,Int]
    { override def apply(v : Int) = v; }

  implicit object CanCastIL extends CanCast[Int,Long]
    { override def apply(v : Int) = v; }

  implicit object CanCastIF extends CanCast[Int,Float]
    { override def apply(v : Int) = v; }

  implicit object CanCastID extends CanCast[Int,Double]
    { override def apply(v : Int) = v; }

  implicit object CanCastLL extends CanCast[Long,Long]
    { override def apply(v : Long) = v; }

  implicit object CanCastLD extends CanCast[Long,Double]
    { override def apply(v : Long) = v; }

  implicit object CanCastFF extends CanCast[Float,Float]
    { override def apply(v : Float) = v; }

  implicit object CanCastFD extends CanCast[Float,Double]
    { override def apply(v : Float) = v; }

  implicit object CanCastDD extends CanCast[Double,Double]
    { override def apply(v : Double) = v; }

  object CanCastIdentityAnyRef extends CanCast[AnyRef,AnyRef]
    { override def apply(v : AnyRef) = v; }

  implicit def CanCastIdentity[V] : CanCast[V,V] = 
    CanCastIdentity.asInstanceOf[CanCast[V,V]];
}

