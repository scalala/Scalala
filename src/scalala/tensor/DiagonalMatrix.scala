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
package scalala.tensor

import scalala.collection.MergeableSet;

/**
 * A view of the given vector as a matrix with diagonal entries
 * wrapping the underlying vector.  Reads and writes are passed
 * through to the underlying vector.
 * 
 * @author dramage
 */
case class DiagonalMatrix(diagonal : Vector) extends Matrix {
  override def rows = diagonal.size;
  override def cols = diagonal.size;
  override def apply(row : Int, col : Int) = 
    if (row == col) diagonal(row) else 0.0;
  override def update(row : Int, col : Int, value : Double) = {
    if (row == col) {
      diagonal(row) = value;
    } else {
      throw new IndexOutOfBoundsException("Cannot set off-diagonal iterator of diagonal matrix");
    }
  }
  override def activeDomain = new MergeableSet[(Int,Int)] {
    override def size = diagonal.activeDomain.size;
    override def contains(i : (Int,Int)) = (i._1 == i._2) && diagonal.activeDomain.contains(i._1);
    override def iterator = diagonal.activeDomain.iterator.map(i => (i,i));
  }

  /**
  * Creates a tensor "like" this one, but with zeros everywhere.
  */
  def like = new DiagonalMatrix(diagonal.like);

  /**
  * Creates a vector "like" this one, but with zeros everywhere.
  */
  def matrixLike(rows:Int,cols:Int) = new DiagonalMatrix(diagonal.vectorLike(rows));

  /**
  * Creates a vector "like" this one, but with zeros everywhere.
  */
  def vectorLike(size:Int) = diagonal.vectorLike(rows);

  override def copy = DiagonalMatrix(diagonal.copy);
}
