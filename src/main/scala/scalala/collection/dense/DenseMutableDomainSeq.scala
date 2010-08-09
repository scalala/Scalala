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
package scalala;
package collection.dense

import collection.{MutableDomainSeqLike,MutableDomainSeq};
import collection.domain.IndexDomain;

/**
 * Implementation trait for a DomainSeq backed by a dense array.
 *
 * @author dramage
 */
trait DenseMutableDomainSeqLike
[@specialized(Int,Long,Float,Double,Boolean) B, +This<:DenseMutableDomainSeq[B]]
extends DenseMutableDomainMapLike[Int,B,IndexDomain,This]
with MutableDomainSeqLike[B,This] {
  override def size = data.length;
  override val domain = IndexDomain(data.length);

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : B) =
    data(key) = value;

  /** Specialized assignment operator using System.arraycopy for speed. */
  def :=(that : DenseMutableDomainSeq[B]) {
    checkDomain(that.domain);
    System.arraycopy(that.data, 0, this.data, 0, data.length);
  }

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transform(f : (Int,B)=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(i,data(i));
      i += 1;
    }
  }
}

/**
 * A DomainSeq backed by a dense array.
 *
 * @author dramage
 */
class DenseMutableDomainSeq
[@specialized(Int,Long,Float,Double,Boolean) B](override val data : Array[B])
extends DenseMutableDomainMap[Int,B] with MutableDomainSeq[B]
with DenseMutableDomainSeqLike[B, DenseMutableDomainSeq[B]] {
  // override def copy = new DenseMutableDomainSeq(data.clone);
}
