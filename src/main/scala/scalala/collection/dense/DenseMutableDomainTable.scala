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
package collection.dense;

import collection.{MutableDomainTableLike,MutableDomainTable};
import collection.domain.TableDomain;

/**
 * Implementation trait for a DomainTable backed by a dense array.
 *
 * @author dramage
 */
trait DenseMutableDomainTableLike
[@specialized(Int,Long,Float,Double,Boolean) B, +This<:DenseMutableDomainTable[B]]
extends DenseMutableDomainMapLike[(Int,Int),B,TableDomain,This]
with MutableDomainTableLike[B, This] {
  final def index(row : Int, col : Int) : Int = {
    checkKey(row,col);
    row + col * numRows;
  }

  final def unindex(index : Int) : (Int,Int) =
    (index % numRows, index / numRows);

  override def apply(row : Int, col : Int) =
    data(index(row,col));

  override def update(row : Int, col : Int, value : B) =
    data(index(row,col)) = value;

  /** Specialized assignment operator using System.arraycopy for speed. */
  def :=(that : DenseMutableDomainTable[B]) {
    checkDomain(that.domain);
    System.arraycopy(that.data, 0, this.data, 0, data.length);
  }

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transform(f : ((Int,Int),B)=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(unindex(i),data(i));
      i += 1;
    }
  }
}

/**
 * A DomainTable backed by a dense array.
 *
 * @author dramage
 */
class DenseMutableDomainTable[@specialized(Int,Long,Float,Double,Boolean) B]
(override val numRows : Int, override val numCols : Int, override val data : Array[B])
extends DenseMutableDomainMap[(Int,Int),B,TableDomain] with MutableDomainTable[B]
with DenseMutableDomainTableLike[B,DenseMutableDomainTable[B]] {

  override val domain = TableDomain(numRows, numCols);

  // override def copy = new DenseMutableDomainTable(numRows, numCols, data.clone);

  if (numRows * numCols != data.length)
    throw new IllegalArgumentException("data.length must equal numRows*numCols");

  def this(numRows : Int, numCols : Int)(implicit cm : ClassManifest[B]) =
    this(numRows, numCols, new Array[B](numRows * numCols));
}
