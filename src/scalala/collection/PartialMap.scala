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
package scalala.collection;

import scalala.collection.domain.Domain;

/**
 * A PartialMap is an immutable map-like object (keys of type A,
 * values of type B) that decouples the domain of possible keys
 * (domain) with those that have (possibly) non-default values
 * (activeDomain).
 * 
 * @author dramage
 */
trait PartialMap[A,B] extends PartialFunction[A,B] with Iterable[(A,B)] {
  /**
   * Returns the complete domain of the function.  This set may be open,
   * so explicit iteration is not recommended.  Instead, see activeDomain.
   */
  def domain : Domain[A];
  
  /**
   * Returns the default value for elements in the domain.  Undefined
   * behavior for elements outside of the domain.
   */
  def default : B;
  
  /** Returns the subset of domain for which a value may be non-default. */
  def activeDomain : Set[A];
  
  //
  // default implementations
  //
  
  /** By default, PartialMaps are defined over their entire domain. */
  override def isDefinedAt(k : A) = domain.contains(k);
  
  /** Iterates (key,value) pairs, possibly skipping tuples with default values. */
  def activeElements : Iterator[(A,B)] = new Iterator[(A,B)] {
    val iter = activeDomain.elements;
    override def hasNext = iter.hasNext;
    override def next = { val k = iter.next;  (k, PartialMap.this(k)); }
  }
  
  /** Iterates keys, possibly skipping keys with default values. */
  def activeKeys : Iterator[A] = new Iterator[A] {
    val iter = activeElements;
    override def hasNext = iter.hasNext;
    override def next = iter.next._1;
  } 

  /** Iterates values, possibly skipping default values. */
  def activeValues : Iterator[B] = new Iterator[B] {
    val iter = activeElements;
    override def hasNext = iter.hasNext;
    override def next = iter.next._2;
  }
  
  /**
   * Iterates over all elements in the domain, whether or not the function
   * overrides the default value at that position.
   */
  override def elements : Iterator[(A,B)] = new Iterator[(A,B)] {
    val iter = domain.elements;
    override def hasNext = iter.hasNext;
    override def next = { val k = iter.next; (k, PartialMap.this(k)); }
  }

  /** @return the keys for which all possibly non-default values are defined. */
  def keys : Iterator[A] = new Iterator[A] {
    val iter = elements;
    override def hasNext = iter.hasNext;
    override def next = iter.next._1;
  }
  
  /** @return the values*/
  def values : Iterator[B] = new Iterator[B] {
    val iter = elements;
    override def hasNext = iter.hasNext;
    override def next = iter.next._2;
  }
  
  //
  // Accessors and projections
  //
  
  /** A new joint view of this map and the other map according to the given merge function. */
  def join[B2,O](that : PartialMap[A,B2])(f : ((B,B2) => O)) : PartialMap.JoinProjection[A,B,B2,O] = {
    new PartialMap.JoinProjection[A,B,B2,O](this,that) {
      override def merge(val1 : B, val2 : B2) = f(val1,val2);
    }
  }
  
  /** A view of this object mapped with the given value function. */
  def map[O](f : (B => O)) : PartialMap[A,O] = {
    new PartialMap.Projection[A,B,O](this) {
      override def func(b : B) = f(b);
    }
  }
  
  /** Getter for an iterator over keys. */
  def apply(keys : Iterator[A]) : Iterator[B] = keys.map(apply);
  
  /** Getter for a collection of keys. */
  def apply(keys : Seq[A]) : Seq[B] = keys.map(apply);
  
  /** Returns the keys for which the value returns true. */
  def find(f : (B => Boolean)) : Iterator[A] = {
    if (f(default)) {
      for ((k,v) <- elements if f(v)) yield k;
    } else {
      for ((k,v) <- activeElements if f(v)) yield k;
    }
  }
  
  /**
   * Default implementation iterates the full domain in order, checking
   * that each function returns the same value.
   */
  override def equals(other : Any) : Boolean = other match {
    case that: PartialMap[_,_] =>
      (this eq that) ||
      (
       (that canEqual this) &&
       (this.domain == that.domain) &&
       { val joint = (this join that.asInstanceOf[PartialMap[A,B]])(_ == _);
         (joint.default == true && !joint.activeValues.contains(false));
       }
      )
    case _ => false;
  }
  
  /** From recipe in "Programming in Scala" section 28.4. */
  protected def canEqual(other : Any) : Boolean = other match {
    case that : PartialMap[_,_] => true;
    case _ => false;
  }
  
  override def hashCode() =
    elements.foldLeft(1)((hash,kv) => 41 * hash + kv.hashCode);
  
  override def toString = 
    getClass.getName+"["+default+"]\n ("+activeElements.mkString("\n  ")+")";
}

/**
 * Companion object for PartialMap class.
 * 
 * @author dramage
 */
object PartialMap {
  /** Projection of a map based on applying function to all values. */
  abstract class Projection[A,B,O](inner : PartialMap[A,B]) extends PartialMap[A,O] {
    def func(value : B) : O;
    
    override def domain = inner.domain;
    override val default = func(inner.default)
    override def apply(i : A) : O = func(inner(i));
    override def activeDomain = inner.activeDomain;
  }
  
  /** Projection of two maps joined on shared keys according to the merge function. */
  abstract class JoinProjection[A,B1,B2,O](pm1 : PartialMap[A,B1], pm2 : PartialMap[A,B2]) extends PartialMap[A,O] {
    if (pm1.domain != pm2.domain) throw new JoinException("Incompatible domains");
    
    def merge(val1 : B1, val2 : B2) : O;
    
    override def domain = pm1.domain;
    override val default = merge(pm1.default, pm2.default);
    override def apply(i : A) = merge(pm1(i),pm2(i));
    override val activeDomain = pm1.activeDomain ++ pm2.activeDomain;
  }
  
  class JoinException(msg : String) extends RuntimeException(msg);
}
