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
 * Implementation trait for DomainMaps defined over a pair of IndexDomains,
 * e.g. the base type of matrices.
 *
 * @author dramage
 */
trait DomainTableLike[@specialized(Int,Long,Float,Double,Boolean) B, +Repr<:DomainTable[B]]
extends DomainMap2Like[Int, Int, B, IndexDomain, IndexDomain, TableDomain, TableDomain, Repr] {

  /** Number of rows in this table. */
  def numRows : Int = domain.numRows;

  /** Number of columsn in this table. */
  def numCols : Int = domain.numCols;

  override def checkKey(row : Int, col : Int) {
    if (row < 0 || row >= numRows || col < 0 || col >= numCols)
      throw new DomainException("Index "+(row,col)+" out of range.  Size is "+numRows+"x"+numCols);
  }

  protected[this] def mkValueString(value : B) : String =
    value.toString;

  // TODO: improve this method to make it more Matrix-like
  def toString(maxRows : Int, maxWidth : Int) : String = {
    def colWidth(col : Int) =
      (0 until (maxRows min numRows)).map(row => mkValueString(this(row,col)).length).max;

    val colWidths = new scala.collection.mutable.ArrayBuffer[Int];
    var col = 0;
    while (col < numCols && colWidths.sum < maxWidth) {
      colWidths += colWidth(col);
      col += 1;
    }

    var rv = new scala.StringBuilder;
    for (row <- 0 until (maxRows min numRows); col <- 0 until colWidths.length) {
      val cell = mkValueString(this(row,col));
      rv.append(cell);
      rv.append(" " * (colWidths(col) - cell.length + 2));
      if (col == colWidths.length - 1) {
        if (col < numCols - 1) {
          rv.append(" ...");
        }
        rv.append(System.getProperty("line.separator"));
      }
    }

    rv.toString;
  }

  override def toString : String =
    toString(maxRows = 20, maxWidth = 72);
}

/**
 * DomainMap defined over a pair of IndexDomains, e.g. the base type of matrices.
 *
 * @author dramage
 */
trait DomainTable[@specialized(Int,Long,Float,Double,Boolean) B]
extends DomainMap2[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain]
with DomainTableLike[B, DomainTable[B]];
