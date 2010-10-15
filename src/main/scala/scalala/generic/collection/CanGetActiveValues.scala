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
package generic;
package collection;

import scalala.collection.sparse.SparseArray;

/**
 * Marker for being able to get the domain (keys) of a collection.
 *
 * @author dramage
 */
trait CanGetActiveValues[-Coll, @specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V] {
  def apply(coll : Coll) : Iterator[(K,V)];
}

object CanGetActiveValues {
  class OpArray[V] extends CanGetActiveValues[Array[V],Int,V] {
    override def apply(coll : Array[V]) =
      Iterator.range(0, coll.length).map(i => (i, coll(i)));
  }

  implicit def opArray[V] = new OpArray[V];

  implicit object OpArrayI extends OpArray[Int];
  implicit object OpArrayS extends OpArray[Short];
  implicit object OpArrayL extends OpArray[Long];
  implicit object OpArrayF extends OpArray[Float];
  implicit object OpArrayD extends OpArray[Double];

  class OpSparseArray[V] extends CanGetActiveValues[SparseArray[V],Int,V] {
    override def apply(coll : SparseArray[V]) =
      coll.activeIterator;
  }

  implicit def opSparseArray[V] = new OpSparseArray[V];
  
  implicit object OpSparseArrayI extends OpSparseArray[Int];
  implicit object OpSparseArrayS extends OpSparseArray[Short];
  implicit object OpSparseArrayL extends OpSparseArray[Long];
  implicit object OpSparseArrayF extends OpSparseArray[Float];
  implicit object OpSparseArrayD extends OpSparseArray[Double];
}
