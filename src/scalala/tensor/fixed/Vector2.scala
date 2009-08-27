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
package scalala.tensor.fixed;

import scalala.collection.{MergeableSet, IntSpanSet, DomainException};

import scalala.tensor.Vector;
import scalala.tensor.dense._;
import scalala.tensor.operators._;

import scalala.tensor._;
import scalala.tensor.operators.TensorShapes._;
import scalala.tensor.Tensor.CreateException;

/**
* A vector with dimension 2
*
* @author dlwh
*/
final case class Vector2(var x: Double, var y: Double) extends Vector with TensorSelfOp[Int,Vector2,Shape1Col] {
  def this() = this(0.0,0.0);

  override def size = 2;

  def apply(i: Int) = i match {
    case 0 => x;
    case 1 => y;
    case _ => throw new IndexOutOfBoundsException(""+i);
  }

  def update(i: Int, v: Double) = i match {
    case 0 => x = v;
    case 1 => y = v;
    case _ => throw new IndexOutOfBoundsException(""+i);
  }

  def like = new Vector2();

  override def copy = new Vector2(x,y);

  override def dot(other: Tensor1[Int]) = other match {
    case Vector2(oX,oY) => x * oX + y * oY;
    case _ => super.dot(other);
  }

  def vectorLike(sz: Int): Vector = {
    if(sz == 2) new Vector2();
    else new DenseVector(sz);
  }

  def matrixLike(rows: Int, cols: Int) = new DenseMatrix(rows,cols);

  def activeDomain = domain;
}

object Vector2 {
  def apply():Vector2 = this(0.0,0.0);
}
