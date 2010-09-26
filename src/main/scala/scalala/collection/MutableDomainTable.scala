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
 * Implementation trait for a MutableDomainMap that is also a DomainTable.
 *
 * @author dramage
 */
trait MutableDomainTableLike
[@specialized(Int,Long,Float,Double,Boolean) B, +This<:MutableDomainTable[B]]
extends DomainTableLike[B,This]
with MutableDomainMap2Like[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,This];

/**
 * MutableDomainMap that is also a DomainTable.
 *
 * @author dramage
 */
trait MutableDomainTable
[@specialized(Int,Long,Float,Double,Boolean) B]
extends DomainTable[B]
with MutableDomainMap2[Int,Int,B]
with MutableDomainTableLike[B,MutableDomainTable[B]];

object MutableDomainTable {
  import scala.collection.mutable.IndexedSeq;

  class Impl[B](numRows : Int, numCols : Int, val data : IndexedSeq[IndexedSeq[B]]) extends MutableDomainTable[B] {
    override val domain = TableDomain(numRows, numCols);

    override def apply(i : Int, j : Int) =
      data(i)(j);

    override def update(i : Int, j : Int, v : B) =
      data(i)(j) = v;
  }

  def apply[B](numRows : Int, numCols : Int, default : B) =
    new Impl(numRows, numCols, IndexedSeq.tabulate(numRows)(r => IndexedSeq.fill(numCols)(default)));
}
