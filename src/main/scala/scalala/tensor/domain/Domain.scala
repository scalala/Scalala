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
package scalala.tensor.domain;

import scala.collection.IterableLike;
import scala.collection.mutable.Builder;
import scala.collection.generic._;

/**
 * Implementation trait for domains of a DomainFunction, representing
 * a restriction on values of type A.
 *
 * @author dramage
 */
trait DomainLike[@specialized(Int,Long,Float,Double) A, +This<:Domain[A]]
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
 * Implementation trait for a domain that can be traversed.  Default
 * equality check returns true if this eq other or if this and other
 * traverse the same elements in the same order.
 *
 * @author dramage
 */
trait IterableDomainLike[@specialized(Int,Long) A, +This<:IterableDomain[A]]
extends DomainLike[A,This] {
  /** Applies the given function to every element of the domain. */
  def foreach[O](f : A => O);

  /** Iterates over the elements of the domain. */
  def iterator : Iterator[A];

  /** Constructs the union of this and the other domain. */
  def union(that : IterableDomain[A]) : IterableDomain[A];

  /** Number of elements in the domain. */
  def size : Int;

  def toIndexedSeq =
    iterator.toIndexedSeq;

  def toArray(implicit mf : Manifest[A]) =
    iterator.toArray;

  def filter(fn : A => Boolean) : List[A] =
    iterator.filter(fn).toList;

  def max(implicit ord : Ordering[A]) =
    iterator.max(ord);

  def min(implicit ord : Ordering[A]) =
    iterator.min(ord);

  override def equals(other : Any) = other match {
    case that : IterableDomain[_] =>
      (this eq that) || (this.iterator zip that.iterator).forall(tup => tup._1 == tup._2);
    case _ =>
      false;
  }
}

/**
 * A domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomain[@specialized(Int,Long) A]
extends Domain[A] with IterableDomainLike[A,IterableDomain[A]];

/**
 * A domain that explicitly has only one element, i.e. A is not a tuple.
 *
 * @author dramage
 */
trait Product1DomainLike[@specialized(Int,Long) A, +This<:Product1Domain[A]]
extends IterableDomainLike[A,This];

/**
 * A domain that explicitly has only one element, i.e. A is not a tuple.
 *
 * @author dramage
 */
trait Product1Domain[@specialized(Int,Long) A]
extends IterableDomain[A] with Product1DomainLike[A,Product1Domain[A]];

/**
 * The domain of elements from a specific set.
 *
 * @author dramage
 */
case class SetDomain[@specialized(Int,Long) A](set : scala.collection.Set[A])
extends Product1Domain[A] with Product1DomainLike[A,SetDomain[A]] {

  override def size =
    set.size;

  override def foreach[O](fn : A=>O) =
    set.foreach(fn);

  override def copy = new SetDomain(Set() ++ set);

  override def iterator =
    set.iterator;

  override def union(that : IterableDomain[A]) = {
    val set = scala.collection.mutable.LinkedHashSet[A]();
    for (v <- this) set += v;
    for (v <- that) set += v;
    SetDomain(set);
  }

  override def contains(key : A) : Boolean =
    set.contains(key);

  override def equals(other : Any) = other match {
    case SetDomain(s) => this.set == s;
    case that : Domain[_] => super.equals(that);
    case _ => false;
  }
}

/**
 * The domain of indices: ints starting from 0 up to a bounded size.
 *
 * @author dramage
 */
case class IndexDomain(override val size : Int)
extends Product1Domain[Int] with Product1DomainLike[Int,IndexDomain] {
  override def foreach[O](fn : Int=>O) = {
    var i = 0;
    while (i < size) {
      fn(i);
      i += 1;
    }
  }

  override def contains(key : Int) =
    key >= 0 && key < size;

  override def copy = this;

  override def union(other : IterableDomain[Int]) = other match {
    case that : IndexDomain => IndexDomain(this.size max that.size);
    case that : SetDomain[_] => that union this;
    case _ => throw new IllegalArgumentException("Unexpected domain type in union.");
  }

  override def iterator =
    Iterator.range(0, size);

  override def equals(other : Any) = other match {
    case IndexDomain(s) => this.size == s;
    case that : Domain[_] => super.equals(that);
    case _ => false;
  }
}

/**
 * Implementation trait for a domain containing all pairs from
 * two underlying domains.
 *
 * @author dramage
 */
trait Product2DomainLike
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 +D1 <: IterableDomain[A1] with DomainLike[A1,D1],
 +D2 <: IterableDomain[A2] with DomainLike[A2,D2],
 +Transpose <: Product2DomainLike[A2,A1,D2,D1,This,Transpose],
 +This <: Product2DomainLike[A1,A2,D1,D2,Transpose,This]]
extends IterableDomain[(A1,A2)] with DomainLike[(A1,A2),This] {
  /** Row-space domain. */
  def _1 : D1;

  /** Col-space domain. */
  def _2 : D2;

  /** Returns the transpose of this domain. */
  def transpose : Transpose;

  override def foreach[O](fn : ((A1,A2)) => O) =
    for (k1 <- _1.iterator; k2 <- _2.iterator) fn((k1,k2));

  /** Iterators all elements of this domain. */
  override def iterator =
    for (k1 <- _1.iterator; k2 <- _2.iterator) yield (k1,k2);

  /** Returns true if a1 is in the row space and a2 is in the col space. */
  def contains(a1 : A1, a2 : A2) =
    _1.contains(a1) && _2.contains(a2);

  /** Defers to contains(tup._1, tup._2). */
  final override def contains(tup : (A1,A2)) =
    contains(tup._1, tup._2);

  override def equals(other : Any) = other match {
    case that : Product2Domain[_,_] =>
      this._1 == that._1 && this._2 == that._2;
    case base : Domain[_] =>
      super.equals(base);
    case _ => false;
  }
}

/**
 * All pairs of elements from D1 and D2, both iterable domains.
 *
 * @author dramage
 */
trait Product2Domain
[@specialized(Int,Long) A1, @specialized(Int,Long) A2]
extends Product2[IterableDomain[A1],IterableDomain[A2]] with IterableDomain[(A1,A2)]
with Product2DomainLike[A1,A2,IterableDomain[A1],IterableDomain[A2],Product2Domain[A2,A1],Product2Domain[A1,A2]] {

  def copy =
    Product2Domain[A1,A2](_1.copy,_2.copy);

  def transpose =
    Product2Domain[A2,A1](_2,_1);

  override def size =
    _1.size * _2.size;

  override def union(other : IterableDomain[(A1,A2)]) = other match {
    case that : Product2Domain[A1,A2] => Product2Domain(this._1 union that._1, this._2 union that._2);
    case that : SetDomain[_] => that union this;
    case _ => throw new IllegalArgumentException("Unexpected domain type in union.");
  }

  override def canEqual(that : Any) =
    that.isInstanceOf[Product2Domain[_,_]];

  override def toString =
    "Product2Domain("+_1.toString+","+_2.toString+")";
}

object Product2Domain {
  def apply[A1,A2](d1 : IterableDomain[A1], d2 : IterableDomain[A2])
  : Product2Domain[A1,A2] = new Impl(d1,d2);

  class Impl[@specialized(Int,Long) A1, @specialized(Int,Long) A2]
  (override val _1 : IterableDomain[A1], override val _2 : IterableDomain[A2])
  extends Product2Domain[A1,A2];
}

/**
 * An immutable Product2Domain indexed by rows and columns.
 *
 * @author dramage
 */
case class TableDomain(numRows : Int, numCols : Int)
extends Product2[IndexDomain,IndexDomain] with Product2Domain[Int,Int]
with Product2DomainLike[Int,Int,IndexDomain,IndexDomain,TableDomain,TableDomain] {

  override val _1 : IndexDomain = IndexDomain(numRows);
  
  override val _2 : IndexDomain = IndexDomain(numCols);

  override def copy = this;

  override def transpose =
    TableDomain(numCols, numRows);

  override def foreach[O](fn : (((Int,Int))=>O)) = {
    var i = 0;
    while (i < numRows) {
      var j = 0;
      while (j < numCols) {
        fn((i,j));
        j += 1;
      }
      i += 1;
    }
  }

  override def union(other : IterableDomain[(Int,Int)]) = other match {
    case that : TableDomain => TableDomain(this.numRows max that.numRows, this.numCols max that.numCols);
    case _ => super.union(other);
  }

  override def toString =
    "TableDomain("+numRows+","+numCols+")";

  override def equals(other : Any) = other match {
    case TableDomain(nr,nc) => this.numRows == nr && this.numCols == nc;
    case that : Domain[_] => super.equals(that);
    case _ => false;
  }
}

/**
 * A domain indexed by sequences of underyling key type K.
 *
 * @author dramage
 */
case class ProductNDomain[@specialized(Int) K](components : Seq[IterableDomain[K]])
extends IterableDomain[Seq[K]] with IterableDomainLike[Seq[K],ProductNDomain[K]] {
  override def copy =
    ProductNDomain(components.map(_.copy));

  override def size =
    components.map(_.size).reduceLeft(_ * _);

  override def union(other : IterableDomain[Seq[K]]) = other match {
    case that : ProductNDomain[K] => {
      require(this.components.size == that.components.size, "Can only take the union of product domains of the same size");
      ProductNDomain((this.components zip that.components) map (tup => tup._1 union tup._2));
    }
    case that : SetDomain[_] => that union this;
    case _ => throw new IllegalArgumentException("Unexpected domain type in union.");
  }

  override def foreach[O](fn : (Seq[K] => O)) = {
    def unroll(key : List[K], remaining : Seq[IterableDomain[K]]) {
      require(remaining.length > 0);
      if (remaining.length == 1) {
        remaining.head.foreach(e => fn(key :+ e));
      } else {
        remaining.head.foreach(e => unroll(key :+ e, remaining.tail));
      }
    }
    unroll(List.empty[K], components);
  }

  /** Iterators all elements of this domain. */
  override def iterator = {
    def unroll(remaining : Seq[IterableDomain[K]]) : Iterator[Seq[K]] = {
      require(remaining.length > 0);
      if (remaining.length == 1) {
        remaining.head.iterator.map(k => List(k));
      } else {
        for (k <- remaining.head.iterator;
             rest <- unroll(remaining.tail))
        yield List(k) ++ rest;
      }
    }

    unroll(components);
  }

  /** Returns true if a1 is in the row space and a2 is in the col space. */
  def contains(k : Seq[K]) =
    (components.length == k.length) && (components zip k).forall(tup => tup._1 contains tup._2);

  override def equals(other : Any) = other match {
    case that : ProductNDomain[_] =>
      this.components == that.components
    case base : Domain[_] =>
      super.equals(base);
    case _ => false;
  }
}

/**
 * An exception thrown when encountering an invalid domain.
 *
 * @author dramage
 */
class DomainException(msg : String) extends RuntimeException(msg) {
  def this() = this(null);
}
