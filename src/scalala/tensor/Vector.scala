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

import scalala.collection.{IntSpanSet};

import scalala.tensor.operators._;

/**
 * A standard numerical Tensor1 defined over 0 inclusive to
 * size exclusive.
 * 
 * @author dramage
 */
trait Vector extends Tensor1[Int] {
  def size : Int;
  
  private val _domain = IntSpanSet(0, size);
  final override def domain : IntSpanSet = _domain;
  
  /** Returns an array copy of this tensor. */
  def toArray = Array.fromFunction(i => this(i))(size);
  
  override def copy : Vector = super.copy.asInstanceOf[Vector];
  
  final protected def check(i : Int) {
    if (i < 0 || i >= size) {
      throw new IndexOutOfBoundsException("Index out of range: "+i+" not in [0,"+size+")");
    }
  }
}
