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
import scalala.scalar.Scalar;

/**
 * Marker trait describing the shape of a value V as having
 * shape signature S.  This class is never instantiated and
 * is used only as an implicit to ensure compatibility between
 * shapes of arguments when operating on collections.
 *
 * @author dramage
 */
@implicitNotFound(msg="Could not recover shape of ${V}")
trait Shape[-V,S];

trait LowPriorityShapeImplicits {
  /** Arbitrary objects have shape Unit. */
  @inline implicit def any[V] : Shape[V,Unit] = null;
}

object Shape extends LowPriorityShapeImplicits {
  def apply[V,S](v : V)(implicit shape : Shape[V,S]) = shape;

  /** Tuples have shape (Unit,Unit, ...). */
  @inline implicit def tuple2[V1,V2]
  : Shape[(V1,V2),(Unit,Unit)] = null;
  
  /** Tuples have shape (Unit,Unit, ...). */
  @inline implicit def tuple3[V1,V2,V3]
  : Shape[(V1,V2,V3),(Unit,Unit,Unit)] = null;
  
  /** Tuples have shape (Unit,Unit, ...). */
  @inline implicit def tuple4[V1,V2,V3,V4]
  : Shape[(V1,V2,V3,V4),(Unit,Unit,Unit,Unit)] = null;

  // TODO: add more tuples
  
  /** Maps have shape (KeyShape=>ValueShape). */
  @inline implicit def map[K,KeyShape,V,ValueShape]
  (implicit kShape : Shape[K,KeyShape], vShape : Shape[V,ValueShape])
  : Shape[scala.collection.Map[K,V],(KeyShape=>ValueShape)] = null;
  
  /** Seqs have shape (Unit=>ValueShape). */
  @inline implicit def seq[V,ValueShape](implicit vShape : Shape[V,ValueShape])
  : Shape[scala.collection.Seq[V],(Unit=>ValueShape)] = null;
  
  /** Arrays have shape (Unit=>ValueShape). */
  @inline implicit def array[V,ValueShape](implicit vShape : Shape[V,ValueShape])
  : Shape[Array[V],(Unit=>ValueShape)] = null;
}

/**
 * Marker trait that says that A and B have statically
 * compatible shape.  This trait is never instantiated and is
 * used only as an implicit to ensure that types A and B have
 * compatible shapes for performing operations.
 *
 * @author dramage 
 */
@implicitNotFound(msg="Types ${A} and ${B} have incompatible shape")
sealed trait CompatibleShape[A,B]

object CompatibleShape {
  @inline implicit def apply[A,SA,B,SB]
  (implicit sa : Shape[A,SA], sb : Shape[B,SB], eq : =:=[SA,SB])
  : CompatibleShape[A,B] = null;
}

