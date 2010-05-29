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

import scala.collection.IterableLike;
import scala.collection.{SetProxy,SetProxyLike};

/**
 * Implementation trait for domains of a DomainFunction, representing
 * a restriction on values of type A.
 *
 * @author dramage
 */
trait DomainLike[@specialized(scala.Int, scala.Long, scala.Double) A, +This<:DomainLike[A,This]]
extends (A => Boolean) {

  type Self = This;
  
  /** Returns a shallow copy of this domain. For immutable domains, may return this. */
  def copy : Self;

  /** Calls contains(key). */
  final override def apply(key : A) = contains(key);

  /** Returns true if the given element is part of the set. */
  def contains(key : A) : Boolean;
}

/**
 * Domains of a DomainFunction, representing a restriction on values of type A.
 *
 * @author dramage
 */
trait Domain[@specialized(scala.Int, scala.Long, scala.Double) A]
extends DomainLike[A, Domain[A]];

//extends (A => Boolean) {
//
//  /** Returns true if this domain can be iterated. */
//  def isIterable : Boolean;
//
//  /** Returns true if this domain is finite. */
//  def isFinite : Boolean;
//
//  /** Returns true if an ordering is defined for the domain. */
//  def isOrdered : Boolean;
//
//  /** Returns true if this set is empty. */
//  def isEmpty : Boolean;
//}

///**
// * Domain of all instance of type A.
// *
// * @author dramage
// */
//trait TypeDomain[@specialized A] extends Domain[A] {
//  override def isIterable = false;
//  override def isFinite = false;
//  override def isOrdered = false;
//  override def isEmpty = false;
//  override def contains(key : A) = true;
//}
//
///** Builder for a domain over the given types */
//trait CanBuildTypeDomain[@specialized A, +D] {
//  def domain : D;
//}
//
///** Constructors for CanBuildTypeDomain instances. */
//object CanBuildTypeDomain {
//  implicit val canBuildDoubleDomain = new CanBuildTypeDomain[Double,RealDomain] {
//    override def domain = new RealDomain();
//  }
//
//  implicit def canBuildGenericDomain[@specialized A] = new CanBuildTypeDomain[A,TypeDomain[A]] {
//    override def domain = new TypeDomain[A] {}
//  }
//}


/**
 * Implementation trait for a domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomainLike[@specialized(scala.Int, scala.Long) A, +This<:IterableDomainLike[A,This]]
extends DomainLike[A,This] with IterableLike[A,This];

/**
 * A domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomain[@specialized(scala.Int, scala.Long) A]
extends Domain[A] with Iterable[A]
with IterableDomainLike[A,IterableDomain[A]];

///**
// * A domain on elements for which an ordering is defined.
// *
// * @author dramage
// */
//trait OrderedDomain[@specialized A] extends Domain[A] {
//  final override def isOrdered = true;
//
//  /** Returns an ordering on the values of this domain. */
//  def ordering : Ordering[A];
//
//  /** Returns true if this domain is bounded from below. */
//  def isLowerBounded : Boolean;
//
//  /** Returns true if this domain is bounded from above. */
//  def isUpperBounded : Boolean;
//
//  /** The domain is bounded if it is both upper and lower bounded */
//  def isBounded =
//    isLowerBounded && isUpperBounded;
//}

///**
// * The domain of real numbers, bounded by min and max, both inclusive.
// * By default, min is Double.NegativeInfinity and max is
// * Double.PositiveInfinity.
// *
// * @author dramage
// */
//class RealDomain(_min : Double, _max : Double)
//extends TypeDomain[Double] with OrderedDomain[Double] {
//
//  def this() = this(Double.NegativeInfinity, Double.PositiveInfinity);
//
//  /** Minimum of the domain (inclusive) */
//  def min : Double = _min;
//
//  /** Maximum of the domain (inclusive) */
//  def max : Double = _max;
//
//  final override def ordering =
//    Ordering.Double;
//
//  final override def contains(key : Double) =
//    key >= min && key <= max;
//
//  final override def isIterable = false;
//
//  final override def isFinite = false;
//
//  final override def isLowerBounded =
//    !min.isNegInfinity;
//
//  final override def isUpperBounded =
//    !max.isPosInfinity;
//
//  final override def isEmpty =
//    min > max;
//
//  override def toString() =
//    "RealDomain("+_min+","+_max+")";
//}

///**
// * The domain of integers as represented by Int, bounded by min and max,
// * both inclusive.
// *
// * @author dramage
// */
//trait IntDomain extends OrderedDomain[Int] with IterableDomain[Int] {
//
//  /** Minimum of the range (inclusive) */
//  def min : Int;
//
//  /** Maximum of the range (inclusive) */
//  def max : Int;
//
//  override def isFinite =
//    true;
//
//  override def ordering =
//    Ordering.Int;
//
//  override def contains(key : Int) =
//    key >= min && key <= max;
//
//  override def isLowerBounded =
//    true;
//
//  override def isUpperBounded =
//    true;
//
//  override def iterator =
//    Range.inclusive(min, max).iterator;
//
//  override def isEmpty =
//    min > max;
//}

/**
 * The domain of elements from a specific set.
 *
 * @author dramage
 */
class SetDomain[@specialized(scala.Int, scala.Long) A](set : scala.collection.Set[A])
extends SetProxy[A] with IterableDomain[A] with IterableDomainLike[A,SetDomain[A]] {
  
  override def self = set;
//  override def isFinite = true;
//  override def isOrdered = false;

//  override def apply(key : A) : Boolean =
//    self.apply(key);

  override def copy = new SetDomain(Set() ++ set);

  override def contains(key : A) : Boolean =
    self.contains(key);

  override def - (value : A) = throw new UnsupportedOperationException();
  override def + (value : A) = throw new UnsupportedOperationException();
}

object SetDomain {
  def apply[@specialized(scala.Int, scala.Long) A](values : A*) =
    new SetDomain(values.toSet);
}

/**
 * The domain of indices: ints starting from 0 up to a bounded size.
 *
 * TODO: rename to SeqDomain.
 *
 * @author dramage
 */
case class IndexDomain(override val size : Int)
extends IterableDomain[Int] with IterableDomainLike[Int,IndexDomain] {
  override def copy = this;

  override def iterator =
    Range.inclusive(0, size - 1).iterator;
}

trait Product2DomainLike
[@specialized(scala.Int, scala.Long) A1,
 @specialized(scala.Int, scala.Long) A2,
 D1 <: IterableDomainLike[A1,D1],
 D2 <: IterableDomainLike[A2,D2],
 +Transpose <: Product2DomainLike[A2,A1,D2,D1,This,Transpose],
 +This <: Product2DomainLike[A1,A2,D1,D2,Transpose,This]]
extends IterableDomainLike[(A1,A2),This] {
  /** Row-space domain. */
  def _1 : D1;

  /** Col-space domain. */
  def _2 : D2;

  /** Returns the transpose of this domain. */
  def transpose : Transpose;

  /** Iterators all elements of this domain. */
  override def iterator =
    for (k1 <- _1.iterator; k2 <- _2.iterator) yield (k1,k2);

  /** Returns true if tup._1 is in the row space and tup._2 is in the col space. */
  override def contains(tup : (A1,A2)) =
    _1.contains(tup._1) && _2.contains(tup._2);
}

/**
 * All pairs of elements from D1 and D2, both iterable domains.
 *
 * @author dramage
 */
class Product2Domain
[@specialized(scala.Int, scala.Long) A1,
 @specialized(scala.Int, scala.Long) A2,
 D1 <: IterableDomainLike[A1,D1], D2 <: IterableDomainLike[A2,D2]]
(override val _1 : D1, override val _2 : D2)
extends Product2[D1,D2] with IterableDomain[(A1,A2)]
with Product2DomainLike[A1,A2,D1,D2,Product2Domain[A2,A1,D2,D1],Product2Domain[A1,A2,D1,D2]] {

  def copy =
    new Product2Domain[A1,A2,D1,D2](_1.copy,_2.copy);

  def transpose =
    new Product2Domain[A2,A1,D2,D1](_2,_1);

//  override def isFinite = _1.isFinite && _2.isFinite;
//
//  override def isOrdered = _1.isOrdered && _2.isOrdered;

  override def toString =
    "Product2Domain("+_1.toString+","+_2.toString+")";
}

/**
 * An immutable Product2Domain indexed by rows and columns.
 *
 * @author dramage
 */
case class TableDomain(numRows : Int, numCols : Int)
extends Product2Domain[Int,Int,IndexDomain,IndexDomain](IndexDomain(numRows), IndexDomain(numCols))
with Product2DomainLike[Int,Int,IndexDomain,IndexDomain,TableDomain,TableDomain] {

  override def copy = this;

  override def transpose =
    TableDomain(numCols, numRows);

  override def toString =
    "TableDomain("+numRows+","+numCols+")";
}

/**
 * An exception thrown when encountering an invalid domain.
 *
 * @author dramage
 */
class DomainException(msg : String) extends RuntimeException(msg) {
  def this() = this(null);
}
