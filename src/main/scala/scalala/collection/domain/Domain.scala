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
import scala.collection.mutable.Builder;
import scala.collection.generic._;

/**
 * Implementation trait for domains of a DomainFunction, representing
 * a restriction on values of type A.
 *
 * @author dramage
 */
trait DomainLike[@specialized(Int,Long,Float,Double) A, +This<:DomainLike[A,This]]
extends (A => Boolean) {

  /** Returns a shallow copy of this domain. For immutable domains, may return this. */
  def copy : This;

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
trait Domain[@specialized(Int,Long,Float,Double) A]
extends DomainLike[A, Domain[A]];


/**
 * Implementation trait for a domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomainLike[@specialized(Int,Long) A, +This<:IterableDomainLike[A,This]]
extends DomainLike[A,This] with IterableLike[A,This]

/**
 * A domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomain[@specialized(Int,Long) A]
extends Domain[A] with Iterable[A]
with GenericTraversableTemplate[A,IterableDomain]
with IterableDomainLike[A,IterableDomain[A]] {
  override def companion : GenericCompanion[IterableDomain] = IterableDomain;
}

object IterableDomain extends TraversableFactory[IterableDomain] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IterableDomain[A]] =
    new GenericCanBuildFrom[A]

  override def newBuilder[A]: Builder[A, IterableDomain[A]] =
    SetDomain.newBuilder;
}

//trait MutableDomainLike[@specialized(Int,Long) A, +This<:MutableDomainLike[A,This]]
//extends IterableDomainLike[A,This] {
//  /** Remove all elements from the domain. */
//  def clear();
//
//  /** Add an element to the domain. */
//  def += (item : A);
//
//  /** Remove an element from the domain. */
//  def -= (item : A);
//}
//
//trait MutableDomain[@specialized(Int,Long) A]
//extends IterableDomain[A] with MutableDomainLike[A, MutableDomain[A]];

/**
 * The domain of elements from a specific set.
 *
 * @author dramage
 */
class SetDomain[@specialized(Int,Long) A](set : scala.collection.Set[A])
extends IterableDomain[A]
with GenericTraversableTemplate[A,SetDomain]
with IterableDomainLike[A,SetDomain[A]] {

  override def size =
    set.size;

  override def companion : GenericCompanion[SetDomain] = SetDomain;

  override def copy = new SetDomain(Set() ++ set);

  override def iterator =
    set.iterator;

  override def contains(key : A) : Boolean =
    set.contains(key);
}

object SetDomain extends TraversableFactory[SetDomain] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SetDomain[A]] =
    new GenericCanBuildFrom[A];

  override def newBuilder[A]: Builder[A, SetDomain[A]] =
    scala.collection.immutable.Set.newBuilder[A].mapResult(s => new SetDomain[A](s));
}

/**
 * The domain of indices: ints starting from 0 up to a bounded size.
 *
 * @author dramage
 */
case class IndexDomain(override val size : Int)
extends IterableDomain[Int] with DomainLike[Int,IndexDomain] {
  override def contains(key : Int) =
    key >= 0 && key < size;

  override def copy = this;

  override def iterator =
    Iterator.range(0, size);
}

trait Product2DomainLike
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 D1 <: IterableDomain[A1] with DomainLike[A1,D1],
 D2 <: IterableDomain[A2] with DomainLike[A2,D2],
 +Transpose <: Product2DomainLike[A2,A1,D2,D1,This,Transpose],
 +This <: Product2DomainLike[A1,A2,D1,D2,Transpose,This]]
extends IterableDomain[(A1,A2)] with DomainLike[(A1,A2),This] {
  /** Row-space domain. */
  def _1 : D1;

  /** Col-space domain. */
  def _2 : D2;

  /** Returns the transpose of this domain. */
  def transpose : Transpose;

  /** Iterators all elements of this domain. */
  override def iterator =
    for (k1 <- _1.iterator; k2 <- _2.iterator) yield (k1,k2);

  /** Returns true if a1 is in the row space and a2 is in the col space. */
  def contains(a1 : A1, a2 : A2) =
    _1.contains(a1) && _2.contains(a2);

  /** Defers to contains(tup._1, tup._2). */
  final override def contains(tup : (A1,A2)) =
    contains(tup._1, tup._2);
}

/**
 * All pairs of elements from D1 and D2, both iterable domains.
 *
 * @author dramage
 */
class Product2Domain
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 D1 <: IterableDomain[A1] with DomainLike[A1,D1],
 D2 <: IterableDomain[A2] with DomainLike[A2,D2]]
(override val _1 : D1, override val _2 : D2)
extends Product2[D1,D2]
with Product2DomainLike[A1,A2,D1,D2,Product2Domain[A2,A1,D2,D1],Product2Domain[A1,A2,D1,D2]] {

  def copy =
    new Product2Domain[A1,A2,D1,D2](_1.copy,_2.copy);

  def transpose =
    new Product2Domain[A2,A1,D2,D1](_2,_1);

  override def toString =
    "Product2Domain("+_1.toString+","+_2.toString+")";
}

/**
 * An immutable Product2Domain indexed by rows and columns.
 *
 * @author dramage
 */
case class TableDomain(numRows : Int, numCols : Int)
extends Product2[IndexDomain,IndexDomain]
with Product2DomainLike[Int,Int,IndexDomain,IndexDomain,TableDomain,TableDomain] {

  override val _1 : IndexDomain = IndexDomain(numRows);
  
  override val _2 : IndexDomain = IndexDomain(numCols);

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
