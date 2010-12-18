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

/**
 * Builder trait for a slicing a view of a matrix.
 *
 * @author dramage
 */
trait CanSliceMatrix[-From, A1, A2, +To] {
  def apply(from : From, keys1 : Seq[A1], keys2 : Seq[A2]) : To;
}


/**
 * Builder trait for a slicing a row from a matrix.
 *
 * @author dramage
 */
trait CanSliceRow[-From, K, +To] {
  def apply(from : From, row : K) : To;
}


/**
 * Builder trait for a slicing a column from a matrix.
 *
 * @author dramage
 */
trait CanSliceCol[-From, K, +To] {
  def apply(from : From, row : K) : To;
}
