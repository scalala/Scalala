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
package scalala.tensor.dense;

import scalala.collection.{MergeableSet, IntSpanSet};
import scalala.collection.domain.{Domain, IntSpanDomain, DomainException};

import scalala.tensor.Vector;
import scalala.tensor.sparse.SparseVector;

import scalala.tensor.Tensor.CreateException;

/**
 * A vector backed by a dense array of doubles.
 * 
 * @author dramage
 */
class DenseVector(data : Array[Double]) extends
  DoubleArrayData(data) with Vector with DenseTensor[Int] {
  
  def this(size : Int) = this(new Array[Double](size));
  
  override def size = data.size;
  
  override def apply(i : Int) = data(i);
  override def update(i : Int, value : Double) = data(i) = value;
  
  override def activeDomain : MergeableSet[Int] = IntSpanSet(0, size);
  
  override def copy = new DenseVector(data.toArray).asInstanceOf[DenseVector.this.type];
  
  override def dot(other : Tensor1[Int]) : Double = other match {
    case dense  : DenseVector  => dot(dense);
    case sparse : SparseVector => sparse.dot(this);
    case _ => super.dot(other);
  }
  
  def dot(that : DenseVector) = {
    if (this.size != that.size) throw new DomainException();
    var i = 0;
    var sum = 0.0;
    while (i < data.length) {
      sum += this.data(i) * that.data(i);
      i += 1;
    }
    sum;
  }
}
