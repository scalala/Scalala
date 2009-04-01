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
 * An extension of a PartialMap that supports value updates.
 * 
 * @author dramage
 */
trait MutablePartialMap[A,B] extends PartialMap[A,B] {
  /** Update the default value. */
  def default_=(d : B) : Unit;
  
  /**
   * Update an individual value.  The given key must be in the
   * map's domain, but need not be in its activeDomain.
   */
  def update(key : A, value : B) : Unit;
  
  /** Batch update of keys and values. */
  def update(keys : Seq[A], values : Seq[B]) : Unit =
    update(keys.elements, values.elements);
  
  /** Batch update of keys and values. */
  def update(keys : Iterator[A], values : Iterator[B]) : Unit = {
    for ((key,value) <- (keys zip values)) update(key,value);
    if (keys.hasNext || values.hasNext) {
      throw new MutablePartialMap.UpdateException(
        "Keys and values had different numbers of elements");
    }
  }
  
  /** Batch update of keys with single value. */
  def update(keys : Collection[A], value : B) : Unit =
    update(keys.elements, value);
  
  /** Batch update of keys with single value. */
  def update(keys : Iterator[A], value : B) : Unit =
    for (key <- keys) update(key,value);
  
  /** Batch update of keys based on a function applied to the value. */
  def update(keys : Collection[A], f : Function1[B,B]) : Unit =
    update(keys.elements, f);
  
  /** Batch update of keys based on a function applied to the value. */
  def update(keys : Iterator[A], f : Function1[B,B]) : Unit =
    for (key <- keys) update(key,f(apply(key)));
  
  /** Batch update of keys based on a function applied to the key and value. */
  def update(keys : Collection[A], f : Function2[A,B,B]) : Unit =
    update(keys.elements, f);
  
  /** Batch update of keys based on a function applied to the key and value. */
  def update(keys : Iterator[A], f : Function2[A,B,B]) : Unit =
    for (key <- keys) update(key,f(key,apply(key)));
}

object MutablePartialMap {
  class UpdateException(msg : String) extends RuntimeException(msg);
}
