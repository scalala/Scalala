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
 * The domain of indices: ints starting from 0 up to a bounded size.
 *
 * @author dramage
 */
case class IndexDomain(override val size : Int)
extends Domain1[Int] with Domain1Like[Int,IndexDomain] {
  override def foreach[O](fn : Int=>O) = {
    var i = 0;
    while (i < size) {
      fn(i);
      i += 1;
    }
  }

  override def product[B,That<:Domain1[B]](that : That) = that match {
    case IndexDomain(otherSize) => TableDomain(size,otherSize);
    case _ => super.product[B,That](that);
  }

  override def toIndexedSeq[B>:Int] =
    Range(0,size);
    
  override def contains(key : Int) =
    key >= 0 && key < size;

  override def union(other : IterableDomain[Int]) = other match {
    case that : IndexDomain => IndexDomain(this.size max that.size);
    case _ => super.union(other);
  }

  override def iterator =
    Iterator.range(0, size);

  override def toString =
    "IndexDomain("+size+")";
    
  override def equals(other : Any) = other match {
    case IndexDomain(s) => this.size == s;
    case _ => super.equals(other);
  }
}

