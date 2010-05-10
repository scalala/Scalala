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
package scalala;
package tensor;
package sparse;

import collection.{MergeableSet,IntSpanSet,DomainException};
import Tensor.CreateException;
import dense.DenseVector;

/**
 * A SingletonBinaryVector is a sparse vector data structure that holds a single
 * integer index as its backing data structure.  The value of the vector
 * at that index is 1.0, and elsewhere is 0.0.  This vector is immutable.
 * 
 * @author mccallum
 */
class SingletonBinaryVector(domainSize : Int, val singleIndex : Int) extends Vector {
  if (domainSize < 0)
    throw new IllegalArgumentException("Invalid domain size: "+domainSize);

  override def default_=(value : Double) = {
    if (value != 0.0) {
      throw new UnsupportedOperationException("SingletonBinaryVector can only have default = 0.0.  Try SparseVector instead.");
    }
  }
  
  override def default = 0.0;
  
  override def size = domainSize;
  
  override def activeDomain = new IntSpanSet(singleIndex,singleIndex+1);

  override def activeElements = Iterator.single(Tuple2(singleIndex,1.0))
  
  override def activeKeys = Iterator.single(singleIndex)
  
  override def activeValues = Iterator.single(1.0)

  /** Zeros this vector, return */
  override def zero() =
    throw new UnsupportedOperationException("SingletonBinaryVector is immutable.  Try SparseVector instead.")
  
  override def apply(i : Int) : Double = {
    if (i == singleIndex) 1.0 else 0.0;
  }
  
  override def update(i : Int, value : Double) = 
    throw new UnsupportedOperationException("SingletonBinaryVector is immutable.  Try SparseVector instead.")
  
  override def copy = new SingletonBinaryVector(size, singleIndex);

  /**
  * Actually the same as copy here. However, this method should probably never be called.
  */
  override def like = new SingletonBinaryVector(size, singleIndex);

  /**
   Returns a SparseHashMatrix with the requested types.
  */
  override def matrixLike(rows:Int, cols:Int) = new SparseHashMatrix(rows,cols); // Seems like a reasonable default;

  /**
   Returns a SparseHashVector with the requested types.
  */
  override def vectorLike(size:Int) = new SparseHashVector(size);
  
  /** Uses optimized implementations. */
  override def dot(other : Tensor1[Int]) : Double = {
    // ensure(other);
    other(singleIndex)
  }
  
  /** Returns a SparseVector representation of this vector. */
  def toSparseVector() : SparseVector = {
    val data = new Array[Double](1);
    val index = new Array[Int](1);
    data(0) = 1.0
    index(0) = singleIndex
    val rv = new SparseVector(size);
    rv.use(index,data,1);
    rv;
  }
}

