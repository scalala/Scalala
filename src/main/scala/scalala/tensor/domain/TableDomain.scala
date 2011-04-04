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
 * An immutable Domain2 indexed by rows and columns.
 *
 * @author dramage
 */
case class TableDomain(numRows : Int, numCols : Int)
extends Product2[IndexDomain,IndexDomain] with Domain2[Int,Int]
with Domain2Like[Int,Int,IndexDomain,IndexDomain,TableDomain,TableDomain] {

  override val _1 : IndexDomain = IndexDomain(numRows);
  
  override val _2 : IndexDomain = IndexDomain(numCols);

  override def transpose =
    TableDomain(numCols, numRows);

  override def foreach[O](fn : (((Int,Int))=>O)) = {
    var i = 0;
    while (i < numRows) {
      var j = 0;
      while (j < numCols) {
        fn((i,j));
        j += 1;
      }
      i += 1;
    }
  }

  override def union(other : IterableDomain[(Int,Int)]) : IterableDomain[(Int,Int)] = other match {
    case that : TableDomain =>
      TableDomain(this.numRows max that.numRows, this.numCols max that.numCols);
    case _ => super.union(other);
  }

  override def toString =
    "TableDomain("+numRows+","+numCols+")";

  override def equals(other : Any) = other match {
    case TableDomain(nr,nc) => this.numRows == nr && this.numCols == nc;
    case _ => super.equals(other);
  }
}

