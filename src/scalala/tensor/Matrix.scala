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
package scalala.tensor;

import scalala.collection.domain.{Domain, Domain2, IntSpanDomain};

/**
 * A standard numerical Tensor2 defined over (0,0) inclusive to 
 * (rows,cols) exclusive.
 * 
 * @author dramage
 */
trait Matrix extends Tensor2[Int,Int] {
  /** The number of rows in this matrix. */
  def rows : Int;
  
  /** The number of columns in this matrix. */
  def cols : Int;
  
  @inline final protected def check(row : Int, col : Int) = {
    if (row < 0 || row >= rows || col < 0 || col >= cols) {
      throw new Predef.IndexOutOfBoundsException(
        "Matrix out of bounds: "+(row,col)+" size was "+size)
    }
  }
  
  /** Returns (rows,cols). */
  @inline final def size = (rows, cols);
  
  private val _domain = Domain2(IntSpanDomain(0, rows), IntSpanDomain(0, cols));
  final override def domain = _domain;
  
  override def getRow(row : Int) = new Vector {
    override def size = cols;
    override def apply(i : Int) = Matrix.this.apply(row,i);
    override def update(i : Int, value : Double) = Matrix.this.update(row,i,value);
    override def activeDomain = Matrix.this.activeDomainInRow(row);
    override def create[J](domain : Domain[J]) = Matrix.this.create(domain);
  }
  
  override def getCol(col : Int) = new Vector {
    override def size = rows;
    override def apply(i : Int) = Matrix.this.apply(i,col);
    override def update(i : Int, value : Double) = Matrix.this.update(i,col,value);
    override def activeDomain = Matrix.this.activeDomainInCol(col);
    override def create[J](domain : Domain[J]) = Matrix.this.create(domain);
  }
  
  /*
  def apply(select : (Int => (Int,Int))) : Vector = {
    select(-1) match {
      case (-1,-1)  => throw new IllegalArgumentException("Index of out range");
      case (-1,col) => getCol(col);
      case (row,-1) => getRow(row);
      case _        => throw new IllegalArgumentException("Invalid index selector");
    }
  }
  */
  
}
