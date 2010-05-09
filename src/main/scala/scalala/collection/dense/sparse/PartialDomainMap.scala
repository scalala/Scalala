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
package sparse;

import domain._;
import generic._;

/**
 * Implementation trait for a DomainMap where only some of the values are
 * active, and the rest take a default value.
 * 
 * @author dramage
 */
trait PartialDomainMapLike
[@specialized A, @specialized B, D <: IterableDomain[A],
 ActiveDomain <: IterableDomain[A], Active <: DomainMap[A,B,ActiveDomain], +Repr]
extends DomainMapLike[A,B,D,Repr] {
self =>

  /** Value for all members of the domain that are not in this.active. */
  def default : B;

  /** A DomainMap containing those elements of this DomainMap that are not the default value. */
  def active : Active;

  /** Returns the value in active if active.domain contains key; otherwise default. */
  override def apply(key : A) =
    if (active.domain.contains(key)) active(key) else default;
}

/**
 * A DomainMap where only some of the values are active, and the rest take a
 * default value.
 *
 * @author dramage
 */
trait PartialDomainMap
[@specialized A, @specialized B, D<:IterableDomain[A],
 ActiveDomain<:IterableDomain[A], Active<:DomainMap[A,B,ActiveDomain]]
extends PartialDomainMapLike[A, B, D, ActiveDomain, Active, PartialDomainMap[A,B,D,ActiveDomain,Active]];


/**
 * Companion object for PartialMap class.
 * 
 * @author dramage
 */
object PartialDomainMap {

//  implicit def canBuildFrom[@specialized A,@specialized B,@specialized O]
//  : PartialMapCanBuildFrom[PartialMap[A,B], A, B, O, MutablePartialMap[A,O]] = {
//    new PartialMapCanBuildFrom[PartialMap[A,B], A, B, O, MutablePartialMap[A,O]] {
//      def apply(from : PartialMap[A,B], default : O) =
//        new PartialMapBuilder[A,O,MutablePartialMap[A,O]](
//          MutablePartialMap[A,O](from.domain, default));
//    }
//  }

//  /**
//   * Returns a default immutable PartialMap for the given domain and default
//   * value.  The actual iterator of the given map are taken as the active iterator.
//   */
//  def apply[K,V,D<:IterableDomain[K]](inDomain : D, inDefault : V)(inMap : scala.collection.Map[K,V]) = {
//    new PartialMap[K,V,D] {
//      override def domain = inDomain;
//      override def default = inDefault;
//      // TODO: fix me
//      override lazy val activeDomain = MergeableSet(inMap.keySet);
//
//      override def activeElements = inMap.iterator;
//      override def activeKeys = inMap.keysIterator;
//      override def activeValues = inMap.valuesIterator;
//
//      override def apply(key : K) =
//        inMap.getOrElse(key, default);
//    }
//  }
//
//  /** Projection of a map based on applying function to all values. */
//  abstract class Projection[A,B,D<:IterableDomain[A],O](inner : PartialMapLike[A,B,D,_]) extends PartialMap[A,O,D] {
//    def func(value : B) : O;
//
//    override def domain = inner.domain;
//    override val default = func(inner.default)
//    override def apply(i : A) : O = func(inner(i));
//    override def activeDomain = inner.activeDomain;
//  }
//
//  /** Projection of two maps joined on shared keys according to the merge function. */
//  abstract class JoinProjection[A,B1,B2,D<:IterableDomain[A],O](pm1 : PartialMapLike[A,B1,D,_], pm2 : PartialMapLike[A,B2,D,_]) extends PartialMap[A,O,D] {
//    if (pm1.domain != pm2.domain) throw new JoinException("Incompatible domains");
//
//    def merge(val1 : B1, val2 : B2) : O;
//
//    override def domain = pm1.domain;
//    override def default = merge(pm1.default, pm2.default);
//    override def apply(i : A) = merge(pm1(i),pm2(i));
//    override def activeDomain = pm1.activeDomain ++ pm2.activeDomain;
//  }
//
//  class JoinException(msg : String) extends RuntimeException(msg);
}


//
//trait VectorLike[+Repr]
//extends MutablePartialMapLike[Int,Double,IndexDomain,Repr]
//with DomainSeqLike[Double,Repr];
//
//trait Vector
//extends MutablePartialMap[Int,Double,IndexDomain]
//with  VectorLike[Vector];
//
//trait VectorCanBuildFrom[-From,+To<:Vector]
//extends DomainMapCanBuildFrom[From,Int,Double,IndexDomain,To] {
//  override def apply(from : From, domain : IndexDomain, default : Double)
//  : DomainMapBuilder[Int,Double,IndexDomain,To] =
//    apply(from, domain.size);
//
//  def apply(from : From, size : Int)
//  : DomainMapBuilder[Int,Double,IndexDomain,To];
//}
//
//object Vector {
//  def apply(size : Int) =
//    new Impl(new Array[Double](size));
//
//  implicit def canMapValues : VectorCanBuildFrom[Vector,Vector]
//  = new VectorCanBuildFrom[Vector,Vector] {
//    override def apply(from : Vector, size : Int) =
//      DomainMapBuilder(Vector(size));
//  }
//
//  class Impl(values : Array[Double]) extends Vector {
//    override def domain = IndexDomain(values.size);
//
//    override def default = 0.0;
//    override def default_=(value : Double) {}
//    override def apply(i : Int) =
//      values(i);
//    override def update(i : Int, value : Double) =
//      values(i) = value;
//    override def activeDomain = MergeableSet(domain);
//  }
//}
