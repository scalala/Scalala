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
package dense;

import generic.Scalar;
import collection.{MutableDomainTable,MutableDomainTableLike};
import collection.domain.TableDomain;
import collection.generic.DomainMapCanMapValuesFrom;

/**
 * A DenseMatrix is backed by an array of doubles, with each column
 * stored before the next column begins.
 *
 * @author dramage
 */
class ArrayArrayMatrix[B](data : Array[Array[B]])
(implicit override val scalar : Scalar[B])
extends MutableDomainTable[B] with MutableDomainTableLike[B,ArrayArrayMatrix[B]]
with Matrix[B] with MatrixLike[B,ArrayArrayMatrix[B]] {

  override val domain = new TableDomain(data.length, data(0).length);
  require(data.forall(_.length == numCols));

  override def apply(i : Int, j : Int) =
    data(i)(j);

  override def update(i : Int, j : Int, v : B) =
    data(i)(j) = v;

}
