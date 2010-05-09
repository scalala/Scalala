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
package scalala.collection.domain;

import scala.collection.SetProxy;

/**
 * Representation of a domain for a DomainFunction, PartialMap, etc.
 * This definition of Domain is based on the mathematical definition of a
 * Set, i.e. some collection of elements that may or may not be bounded
 * and may or may not be traversable, and those elements may or may not
 * have an ordering.
 *
 * @author dramage
 */
trait Domain[@specialized A] extends (A => Boolean) {

  /** Returns true if this domain can be iterated. */
  def isIterable : Boolean;

  /** Returns true if this domain is finite. */
  def isFinite : Boolean;

  /** Returns true if an ordering is defined for the domain. */
  def isOrdered : Boolean;

  /** Returns true if this set is empty. */
  def isEmpty : Boolean;

  /** Calls contains(key). */
  override def apply(key : A) = contains(key);

  /** Returns true if the given element is part of the set. */
  def contains(key : A) : Boolean;
}

/**
 * Domain of all instance of type A.
 *
 * @author dramage
 */
trait TypeDomain[@specialized A] extends Domain[A] {
  override def isIterable = false;
  override def isFinite = false;
  override def isOrdered = false;
  override def isEmpty = false;
  override def contains(key : A) = true;
}

/** Builder for a domain over the given types */
trait CanBuildTypeDomain[@specialized A, +D] {
  def domain : D;
}

/** Constructors for CanBuildTypeDomain instances. */
object CanBuildTypeDomain {
  implicit val canBuildDoubleDomain = new CanBuildTypeDomain[Double,RealDomain] {
    override def domain = new RealDomain();
  }

  implicit def canBuildGenericDomain[@specialized A] = new CanBuildTypeDomain[A,TypeDomain[A]] {
    override def domain = new TypeDomain[A] {}
  }
}


/**
 * A domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomain[@specialized A] extends Domain[A] with Iterable[A] {
  final override def isIterable = true;
}

/**
 * A domain on elements for which an ordering is defined.
 *
 * @author dramage
 */
trait OrderedDomain[@specialized A] extends Domain[A] {
  final override def isOrdered = true;

  /** Returns an ordering on the values of this domain. */
  def ordering : Ordering[A];

  /** Returns true if this domain is bounded from below. */
  def isLowerBounded : Boolean;

  /** Returns true if this domain is bounded from above. */
  def isUpperBounded : Boolean;

  /** The domain is bounded if it is both upper and lower bounded */
  def isBounded =
    isLowerBounded && isUpperBounded;
}

/**
 * The domain of real numbers, bounded by min and max, both inclusive.
 * By default, min is Double.NegativeInfinity and max is
 * Double.PositiveInfinity.
 *
 * @author dramage
 */
class RealDomain(_min : Double, _max : Double)
extends TypeDomain[Double] with OrderedDomain[Double] {

  def this() = this(Double.NegativeInfinity, Double.PositiveInfinity);

  /** Minimum of the domain (inclusive) */
  def min : Double = _min;

  /** Maximum of the domain (inclusive) */
  def max : Double = _max;

  final override def ordering =
    Ordering.Double;

  final override def contains(key : Double) =
    key >= min && key <= max;

  final override def isIterable = false;

  final override def isFinite = false;
  
  final override def isLowerBounded =
    !min.isNegInfinity;

  final override def isUpperBounded =
    !max.isPosInfinity;

  final override def isEmpty =
    min > max;

  override def toString() =
    "RealDomain("+_min+","+_max+")";
}

/**
 * The domain of integers as represented by Int, bounded by min and max,
 * both inclusive.
 *
 * @author dramage
 */
trait IntDomain extends OrderedDomain[Int] with IterableDomain[Int] {

  /** Minimum of the range (inclusive) */
  def min : Int;

  /** Maximum of the range (inclusive) */
  def max : Int;

  override def isFinite =
    true;

  override def ordering =
    Ordering.Int;

  override def contains(key : Int) =
    key >= min && key <= max;

  override def isLowerBounded =
    true;

  override def isUpperBounded =
    true;

  override def iterator =
    Range.inclusive(min, max).iterator;

  override def isEmpty =
    min > max;
}

/**
 * The domain of indices: ints starting from 0 up to a bounded size.
 *
 * @author dramage
 */
case class IndexDomain(override val size : Int) extends IntDomain {
  final override def min = 0;
  final override def max = size - 1;
}


/**
 * The domain of elements from a specific set.
 *
 * @author dramage
 */
case class SetDomain[@specialized A](set : scala.collection.Set[A]) extends SetProxy[A] with IterableDomain[A] {
  override def self = set;
  override def isFinite = true;
  override def isOrdered = false;

  override def apply(key : A) : Boolean =
    self.apply(key);

  override def contains(key : A) : Boolean =
    self.contains(key);

  override def - (value : A) = throw new UnsupportedOperationException();
  override def + (value : A) = throw new UnsupportedOperationException();
}

object SetDomain {
  def apply[@specialized A](values : A*) =
    new SetDomain(values.toSet);
}

//trait MappedDomain[@specialized A1, D1<:IterableDomain[A1], @specialized A2]
//extends IterableDomain[A2] {
//  def underlying : D1;
//
//  def lookup(key : A2) : A1;
//
//  override def contains(key : A2) : Boolean =
//    underlying(lookup(key));
//}

/**
 * All pairs of elements from D1 and D2, both iterable domains.
 *
 * @author dramage
 */
class Product2Domain
[@specialized A1, @specialized A2, D1 <: IterableDomain[A1], D2 <: IterableDomain[A2]]
(override val _1 : D1, override val _2 : D2)
extends Product2[D1,D2] with IterableDomain[(A1,A2)] {
  def transpose =
    new Product2Domain[A2,A1,D2,D1](_2,_1);

  override def iterator =
    for (k1 <- _1.iterator; k2 <- _2.iterator) yield (k1,k2);

  override def contains(tup : (A1,A2)) =
    _1.contains(tup._1) && _2.contains(tup._2);

  override def isFinite = _1.isFinite && _2.isFinite;

  override def isOrdered = _1.isOrdered && _2.isOrdered;

  override def toString =
    "ProductDomain("+_1.toString+","+_2.toString+")";
}

/**
 * A Product2Domain indexed by rows and columns.
 *
 * @author dramage
 */
case class TableDomain(numRows : Int, numCols : Int)
extends Product2Domain[Int,Int,IndexDomain,IndexDomain](
  IndexDomain(numRows), IndexDomain(numCols)) {

  override def transpose : TableDomain =
    TableDomain(numCols, numRows);
}

/**
 * An exception thrown when encountering an invalid domain.
 *
 * @author dramage
 */
class DomainException(msg : String) extends RuntimeException(msg) {
  def this() = this(null);
}
