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
package counter;

import collection._;
import collection.domain._;
import collection.generic._;
import collection.numeric._;

/**
 * Implementation trait for Counters, which are mutable numeric domain maps
 * where the domain semantics are to automatically grow to accomodate new
 * requests, as opposed to throwing an exception on out of domain tokens.
 */
trait CounterLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B,
 +This <: Counter[A,B]]
extends MutableNumericDomainMapLike[A,B,SetDomain[A],This]
{
  def default : B;

  /** All keys of type A are good to go. */
  override protected def checkKey(key : A) : Unit = { }

  /** All domains of type Domain[A] are good to go. */
  override protected def checkDomain(domain : Domain[A]) : Unit = { }

  def size = domain.size;
}

trait Counter
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B]
extends MutableNumericDomainMap[A,B] with CounterLike[A,B,Counter[A,B]];

object Counter {
  def count[@specialized X](values : Iterator[X]) : Counter[X,Int] = {
    val rv = MapCounter[X,Int]();
    for (value <- values) {
      rv(value) += 1;
    }
    rv;
  }
}

//class OpenAddressCounter
//[@specialized(Int,Long) A:ClassManifest, @speicalized(Int,Long,Float,Double) B:scala.collection.numeric.Numeric:ClassManifest] {
//
//}

class MapCounter
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B:scalala.collection.numeric.Numeric]
(val map : scala.collection.mutable.Map[A,B] = new scala.collection.mutable.HashMap[A,B])
extends Counter[A,B] with CounterLike[A,B,MapCounter[A,B]] {
  override val numeric = implicitly[Numeric[B]];
  override def default = numeric.zero;

  override def apply(k : A) =
    map.getOrElse(k, default);

  override def update(k : A, v : B) =
    map(k) = v;

  override val domain =
    new SetDomain(map.keySet);
}

object MapCounter {
  def apply[A,B:scalala.collection.numeric.Numeric]
  (map : scala.collection.mutable.Map[A,B] = new scala.collection.mutable.HashMap[A,B]) =
    new MapCounter(map);
}
