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
package tensor;
package domain;

/**
 * A domain indexed by sequences of underyling key type K.
 *
 * @author dramage
 */
case class DomainN[@specialized(Int) K](components : Seq[IterableDomain[K]])
extends IterableDomain[Seq[K]] with IterableDomainLike[Seq[K],DomainN[K]] {

  override def size =
    components.map(_.size).reduceLeft(_ * _);

  override def union(other : IterableDomain[Seq[K]]) = other match {
    case that : DomainN[_] => {
      val casted = that.asInstanceOf[DomainN[K]];
      require(this.components.size == casted.components.size, "Can only take the union of product domains of the same size");
      DomainN((this.components zip casted.components) map (tup => tup._1 union tup._2));
    }
    case _ => super.union(other);
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
    case that : DomainN[_] =>
      this.components == that.components
    case base : Domain[_] =>
      super.equals(base);
    case _ => false;
  }
}

