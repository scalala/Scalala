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
package scalala.tensor.dense

import scalala.tensor.{Tensor,Matrix};
import scalala.collection.domain.{Domain, Domain2, IntSpanDomain};

import scalala.tensor.Tensor.CreateException;

/**
 * Implementation of create zero and default for tensors backed
 * by a sense Array[Double] for data storage.
 * 
 * @author dramage
 */
trait DenseTensor[I] extends Tensor[I] {
  /** Returns the data backing this tensor. */
  def data : Array[Double];
  
  /** Creates a new dense tensor. */
  override def create[J](d : Domain[J]) : Tensor[J] = d match {
    case Domain2(IntSpanDomain(0,rows),IntSpanDomain(0,cols)) =>
      new DenseMatrix(rows, cols);
    case IntSpanDomain(0,len) =>
      new DenseVector(len).asInstanceOf[Tensor[J]];
    case _ =>
      throw new CreateException("Invalid domain for DenseMatrix construction: "+d);
  }
  
  override def zero() = {
    this.default = 0;
    java.util.Arrays.fill(data, 0.0);
  }
  
  /** Cannot change default value for dense tensors. */
  override def default_=(update : Double) = {};
  
  override def default = 0.0;
}
