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
package collection;
package numeric;

import domain._;

/**
 * A Mutable DomainMap with numeric values.
 *
 * @author dramage
 */
trait MutableNumericDomainMapLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B,
 +D<:IterableDomain[A] with DomainLike[A,D],
 +This<:MutableNumericDomainMap[A,B]]
extends NumericDomainMapLike[A,B,D,This] with MutableDomainMapLike[A,B,D,This] {

  //
  // Scalar updates.
  //

  /** Multiplies each element by the given scale factor. */
  def *= (s : B) =
    if (s != 1) transformValues(v => numeric.*(v, s));

  /** Divides each element by the given scale factor. */
  def /= (s : B) =
    if (s != 1) transformValues(v => numeric./(v, s));

  /** Increments element by the given scalar. */
  def += (s : B) =
    if (s != 0) transformValues(v => numeric.+(v, s));

  /** Decrements each element by the given scalar. */
  def -= (s : B) =
    if (s != 0) transformValues(v => numeric.-(v, s));

  /** Raises each element to the the given power. */
  def :^= (s : B) =
    if (s != 1) transformValues(v => numeric.pow(v, s));

  /** Each element becomes itself modulo the given scalar. */
  def %= (s : B) =
    if (s != 0 && s != 1) transformValues(v => numeric.%(v, s));

  //
  // Updates from another DomainMap.
  //

  /** Multiplies each value in this map by the corresponding value in the other map. */
  def :*= (t : DomainMap[A,B]) {
    checkDomain(t.domain);
    transform((k,v) => numeric.*(v,t(k)));
  }

  /** Divides each value in this map by the corresponding value in the other map. */
  def :/= (t : DomainMap[A,B]) {
    checkDomain(t.domain);
    transform((k,v) => numeric./(v,t(k)));
  }

  /** Increments each value in this map by the corresponding value in the other map. */
  def :+= (t : DomainMap[A,B]) {
    checkDomain(t.domain);
    transform((k,v) => numeric.+(v,t(k)));
  }

  /** Decrements each value in this map by the corresponding value in the other map. */
  def :-= (t : DomainMap[A,B]) {
    checkDomain(t.domain);
    transform((k,v) => numeric.-(v,t(k)));
  }

  /** Modulos each value in this map by the corresponding value in the other map. */
  def :%= (t : DomainMap[A,B]) {
    checkDomain(t.domain);
    transform((k,v) => numeric.%(v,t(k)));
  }

  /** Raises each value in this map by the corresponding value in the other map. */
  def :^= (t : DomainMap[A,B]) {
    checkDomain(t.domain);
    transform((k,v) => numeric.pow(v, t(k)));
  }

  /** += with another PartialMap is a fixed alias for :+= */
  final def += (t : DomainMap[A,B]) = this.:+=(t);

  /** -= with another PartialMap is a fixed alias for :-= */
  final def -= (t : DomainMap[A,B]) = this.:-=(t);

}

trait MutableNumericDomainMap
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B]
extends NumericDomainMap[A,B] with MutableDomainMap[A,B]
with MutableNumericDomainMapLike[A,B,IterableDomain[A],MutableNumericDomainMap[A,B]]

object MutableNumericDomainMap {

  def apply
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
  (domain : IterableDomain[A], default : B, map : scala.collection.Map[A,B] = scala.collection.mutable.Map[A,B]())
  (implicit numeric : Numeric[B]) =
    new Impl[A,B](map, default, domain)(numeric);

  class Impl
  [@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
  (_map : scala.collection.Map[A,B], _default : B, _domain : IterableDomain[A])
  (implicit override val numeric : Numeric[B])
  extends MutableDomainMap.Impl[A,B](_map, _default, _domain)
  with MutableNumericDomainMap[A,B];
}
