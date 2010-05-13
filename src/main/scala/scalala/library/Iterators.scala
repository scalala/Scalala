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
package library;

/**
 * Math on Iterator instances.
 *
 * @author dramage
 */
trait Iterators {
  def max(v : Iterator[Double]) : Double =
    v.reduceLeft(math.max);

  def argmax[I](v : Iterator[(I,Double)]) : I =
    v.reduceLeft((tupA,tupB) => if (tupA._2 > tupB._2) tupA else tupB)._1;

  def argmax[I,V](v : Iterator[(I,V)])(implicit cv : V=>Double) : I =
    argmax(v.map(tup => (tup._1, cv(tup._2))));

  def argmax(v : Iterator[Double]) : Int =
    argmax(v.zipWithIndex.map(tup => (tup._2,tup._1)));

  def argmax[V](v : Iterator[V])(implicit cv : V=>Double) : Int =
    argmax(v.map(cv));

  def min(v : Iterator[Double]) : Double =
    v.reduceLeft(math.min);

  def argmin[I](v : Iterator[(I,Double)]) : I =
    v.reduceLeft((tupA,tupB) => if (tupA._2 < tupB._2) tupA else tupB)._1;

  def argmin[I,V](v : Iterator[(I,V)])(implicit cv : V=>Double) : I =
    argmin(v.map(tup => (tup._1, cv(tup._2))));

  def argmin(v : Iterator[Double]) : Int =
    argmin(v.zipWithIndex.map(tup => (tup._2,tup._1)));

  def argmin[V](v : Iterator[V])(implicit cv : V=>Double) : Int =
    argmin(v.map(cv));
}
