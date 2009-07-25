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
   * Returns the complete domain of the function.  For some types of
   * maps (e.g. those converted from scala.collection.Map), domain may
   * equal activeDomain.
   */
  def domain : MergeableSet[A];
  
  /**
   * Returns the default value for iterator in the domain.  Undefined
   * behavior for iterator outside of the domain.
   */
  def default : B;
  
  /** Returns the subset of domain for which a value may be non-default. */
  def activeDomain : MergeableSet[A];
  
  //
  // default implementations
  //
  
  /** By default, PartialMaps are defined over their entire domain. */
  override def isDefinedAt(k : A) = domain.contains(k);
  
  /** Iterates (key,value) pairs, possibly skipping tuples with default values. */
  def activeElements : Iterator[(A,B)] = new Iterator[(A,B)] {
    val iter = activeDomain.iterator;
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
   * Iterates over all iterator in the domain, whether or not the function
   * overrides the default value at that position.
   */
  override def iterator : Iterator[(A,B)] = new Iterator[(A,B)] {
    val iter = domain.iterator;
    override def hasNext = iter.hasNext;
    override def next = { val k = iter.next; (k, PartialMap.this(k)); }
  }

  /** @return the keys for which all possibly non-default values are defined. */
  def keysIterator : Iterator[A] = new Iterator[A] {
    val iter = iterator;
    override def hasNext = iter.hasNext;
    override def next = iter.next._1;
  }
  
  /** @return the values*/
  def valuesIterator : Iterator[B] = new Iterator[B] {
    val iter = iterator;
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
  def apply(keys : Iterable[A]) : Iterable[B] = keys.map(apply);
  
  /** Getter for a collection of keys. */
  def apply(keys : Seq[A]) : Seq[B] = keys.map(apply);
  
  /** A set is both an Iterable and a (A=>Boolean), so it is special-cased. */
  def apply(keys : scala.collection.Set[A]) : Iterable[B] = keys.map(apply);

  /** Getter for all values for which the given key function returns true. */
  def apply(f : (A => Boolean)) : Iterator[B] =
    for ((k,v) <- iterator; if f(k)) yield v;
  
  /** Returns the keys for which the value returns true. */
  def find(f : (B => Boolean)) : Iterator[A] = {
    if (f(default)) {
      for ((k,v) <- iterator if f(v)) yield k;
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
         ((joint.default == true || joint.activeDomain.size == joint.domain.size)
          && !joint.activeValues.contains(false));
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
    iterator.foldLeft(1)((hash,kv) => 41 * hash + kv.hashCode);
  
  override def toString = 
    getClass.getName+"[domain="+domain+" active="+activeDomain.size+" default="+default+"]\n ("+activeElements.mkString("\n  ")+")";
}

/**
 * Companion object for PartialMap class.
 * 
 * @author dramage
 */
object PartialMap {
  /**
   * Returns a default immutable PartialMap for the given domain and default
   * value.  The actual iterator of the given map are taken as the active iterator.
   */
  def apply[K,V](inDomain : MergeableSet[K], inDefault : V)(inMap : scala.collection.Map[K,V]) = {
    new PartialMap[K,V] {
      override def domain = inDomain;
      override def default = inDefault;
      override lazy val activeDomain = MergeableSet(inMap.keySet);
      
      override def activeElements = inMap.iterator;
      override def activeKeys = inMap.keysIterator;
      override def activeValues = inMap.valuesIterator;
      
      override def apply(key : K) = {
        inMap.get(key) match {
          case Some(value) => value;
          case None => default;
        }
      }
    }
  }
  
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
    override def default = merge(pm1.default, pm2.default);
    override def apply(i : A) = merge(pm1(i),pm2(i));
    override def activeDomain = pm1.activeDomain ++ pm2.activeDomain;
  }
  
  class JoinException(msg : String) extends RuntimeException(msg);
}

/**
 * An exception thrown when encountering an invalid domain.
 * @author dramage
 */
class DomainException(msg : String) extends RuntimeException(msg) {
  def this() = this("");
}
