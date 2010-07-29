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
package collection;

import domain._;
import generic._;

/**
 * Implementation trait for DomainMaps defined over an IndexDomain,
 * e.g. base type of a vector.
 *
 * @author dramage
 */
trait DomainSeqLike
[@specialized(Int,Long,Float,Double,Boolean) B, +Repr<:DomainSeq[B]]
extends DomainMapLike[Int, B, IndexDomain, Repr] {

  def size = domain.size;

  protected[this] def mkValueString(value : B) : String =
    value.toString;

  def toList =
    List.tabulate(size)(i => this(i));

  def toArray(implicit m : ClassManifest[B]) =
    Array.tabulate(size)(i => this(i));

  // TODO: improve this method to make it more Vector-like
  override def toString = {
    val rv = valuesIterator.take(10).map(mkValueString).mkString("\n");
    if (size > 10) {
      rv + "\n" + "...";
    } else {
      rv;
    }
  }
}

/**
 * A DomainMap defined over an IndexDomain, e.g. base type of a vector.
 *
 * @author dramage
 */
trait DomainSeq[@specialized(Int,Long,Float,Double,Boolean) B]
extends DomainMap[Int,B,IndexDomain] with DomainSeqLike[B,DomainSeq[B]];
