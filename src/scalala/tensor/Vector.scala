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

import scalala.collection.IntSpanSet;
import operators.TensorShapes._;
import scalala.tensor.operators.TensorSelfOp;

/**
 * A standard numerical Tensor1 defined over 0 inclusive to
 * size exclusive.
 * 
 * @author dramage
 */
trait Vector extends Tensor1[Int] with TensorSelfOp[Int,Vector,Shape1Col] {
  /**
  * Creates a vector "like" this one, but with zeros everywhere.
  */
  override def like: Vector;
  def size : Int;
  
  final override def domain : IntSpanSet = IntSpanSet(0, size);
  
  /** Returns an array copy of this tensor. */
  def toArray = Array.tabulate(size)(i => this(i));
  
  override def copy : Vector = super.copy.asInstanceOf[Vector];

  override def apply(i: Int): Double;
  override def update(i: Int,v: Double):Unit;
  
  final protected def check(i : Int) {
    if (i < 0 || i >= size) {
      throw new IndexOutOfBoundsException("Index out of range: "+i+" not in [0,"+size+")");
    }
  }

  /**
  * Creates a vector "like" this vector, with the dimensionality of the provided vector
  */
  def vectorLike(v: Vector): Vector = vectorLike(v.size);

  /**
  * Creates a vector "like" this vector, with the dimensionality provided 
  */
  def vectorLike(sz: Int): Vector;

  /**
  * Creates a matrix "like" this vector, with the dimensionality provided
  */
  def matrixLike(rows: Int, cols: Int): Matrix;
}

object Vector {
  /**
   * DenseVector literal, equivalent to
   * <pre>DenseVector(values.size)(values :_*)</pre>.
   */
  def apply(values : Double*) =
    dense.DenseVector(values.size)(values :_*);
}
