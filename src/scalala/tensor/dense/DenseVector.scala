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

import scalala.tensor.Vector;
import scalala.collection.domain.{Domain, IntSpanDomain};

import scalala.tensor.Tensor.CreateException;

import scalala.tensor.spans.IntSpans._;

/**
 * A vector backed by a dense array of doubles.
 * 
 * @author dramage
 */
class DenseVector(data : Array[Double]) extends DoubleArrayData(data) with Vector {
  
  def this(size : Int) = this(new Array[Double](size));
  
  /** Cannot change default value for dense tensors. */
  override def default_=(update : Double) = {};
  
  override def size = data.size;
  
  override def apply(i : Int) = data(i);
  override def update(i : Int, value : Double) = data(i) = value;
  
  override val activeDomain : Set[Int] = 0 until size;
  
  override def create[J](domain : Domain[J]) : Tensor[J] = domain match {
    case IntSpanDomain(0,len) => new DenseVector(new Array[Double](len));
    case _ => throw new CreateException("Cannot create DenseVector with domain "+domain);
  }
  
  override def copy = new DenseVector(data.toArray).asInstanceOf[DenseVector.this.type];
  
  override def zero() = {
    this.default = 0;
    java.util.Arrays.fill(data, 0.0);
  }
}
