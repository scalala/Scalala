/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.collection.domain;

/**
 * An exception thrown when encountering an invalid domain.
 * @author dramage
 */
class DomainException(msg : String) extends RuntimeException(msg) {
  def this() = this("");
}

/**
 * The domain of a PartialMap; acts like an immutable set.
 * @author dramage
 */
sealed abstract case class Domain[I]() extends Iterable[I] {
  /** Returns true if this domain is finite (and therefore iterable). */
  def isFinite : Boolean;
  
  /** Returns true if this domain contains the requested element, false otherwise. */
  def contains(element : I) : Boolean;
}

/**
 * A finite Domain with equality based on iterating all elements equality.
 * @author dramage
 */
trait FiniteDomain[I] extends Domain[I] {
  override def isFinite = true;

  /** Returns the number of elements in this domain. */
  def size : Int;
  
  protected def canEqual(other : Any) : Boolean = other match {
    case that : FiniteDomain[_] => true;
    case _ => false;
  }
  
  override def equals(other : Any) : Boolean = other match {
    case that: FiniteDomain[_] =>
      (this eq that) ||
      ((that canEqual this) &&
       (this.isFinite && !that.elements.exists(o => !this.contains(o.asInstanceOf[I]))));
    case _ => false;
  }
  
  override def hashCode : Int = 
    elements.foldLeft(1)((hash,kv) => 41 * hash + kv.hashCode);
}

/**
 * A Domain with infinitely many elements which cannot be iterated.
 * @author dramage
 */
trait InfiniteDomain[I] extends Domain[I] {
  override def isFinite = false;
  override def elements =
    throw new DomainException("Cannot iterate over infinite domain");
}

/**
 * The domain of a 1D PartialMap.
 * @author dramage
 */
abstract case class Domain1[A]() extends Domain[A] {
  def cross[B](other : Domain1[B]) : Domain2[A,B] = new Domain2(this, other);
}

/**
 * The domain of a 2D PartialMap.
 * @author dramage
 */
case class Domain2[A,B](_1 : Domain1[A], _2 : Domain1[B]) extends Domain[(A,B)] {
  def transpose : Domain2[B,A] = new Domain2(_2, _1);
  
  override def contains(element : (A,B)) =
    _1.contains(element._1) && _2.contains(element._2);
  
  override def isFinite : Boolean =
    _1.isFinite && _2.isFinite;
  
  override def elements =
    for (a <- _1.elements; b <- _2.elements) yield (a,b);
}

/**
 * An infinite domain consisting of all instances of a given type.
 * @author dramage
 */
class TypeDomain[I<:AnyRef](val manifest : scala.reflect.Manifest[I]) extends Domain1[I] with InfiniteDomain[I] {
  override def contains(element : I) = true;
  override def equals(other : Any) = other match {
    case that : TypeDomain[_] => ((that eq this) || (that.manifest.toString == this.manifest.toString));
    case _ => false;
  }
  override def hashCode = manifest.toString.hashCode;
}

/**
 * A domain consisting of the members of the given set.
 * @author dramage
 */
case class SetDomain[I](val set : scala.collection.Set[I]) extends Domain1[I] with FiniteDomain[I] {
  override def contains(element : I) = set.contains(element);
  override def elements = set.elements;
  override def size = set.size;
  override def equals(other : Any) = other match {
    case that : SetDomain[_] => (this eq that) || (this.set == that.set);
    case _ => super.equals(other);
  }
}

/**
 * A range of integers over from start (inclusive) to end (exclusive).
 * @author dramage
 */
case class IntSpanDomain(start : Int, end : Int) extends Domain1[Int] with FiniteDomain[Int] {
  override def contains(i : Int) = i >= start && i < end;
  override def elements = (start until end).elements;
  override def size = end - start;
  override def equals(other : Any) = other match {
    case IntSpanDomain(s2,e2) => (start == s2 && end == e2)
    case _ => super.equals(other);
  }
}
