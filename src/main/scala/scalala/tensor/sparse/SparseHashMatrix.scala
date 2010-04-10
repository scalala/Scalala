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
package scalala.tensor.sparse

import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;

import scalala.tensor.{Matrix,Vector};
import scalala.collection.{MergeableSet,IntSpanSet,DomainException};

import scalala.tensor.Tensor.CreateException;
import scalala.collection.DomainException;
import scalala.tensor.dense.DenseVector;
import scalala.tensor.operators.TensorShapes._;
import scalala.tensor.operators.TensorSelfOp;

/**
 * Sparse matrix backed by an open address hash table with expected
 * constant time access and updates.  This class wraps FastUtil's
 * Int2DoubleOpenHashMap.
 */
@serializable
@SerialVersionUID(1)
class SparseHashMatrix(override val rows: Int,
  override val cols: Int, protected val hashmap : Int2DoubleOpenHashMap)
  extends Matrix with TensorSelfOp[(Int,Int),SparseHashMatrix,Shape2] {
  
  /** Primary constructor, based only on size of the map with a new backing. */
  def this(rows : Int, cols: Int) =
    this(rows, cols, new Int2DoubleOpenHashMap());
  
  override def default : Double =
    hashmap.defaultReturnValue();
  
  override def default_=(value : Double) =
    hashmap.defaultReturnValue(value);

  override def apply(row : Int, col : Int) : Double =
    hashmap.get(index(row,col));
  
  override def update(row : Int, col : Int, value : Double) =
    hashmap.put(index(row,col),value);
  
  @inline final def index(row : Int, col : Int) : Int = {
    check(row,col);
    row + col * rows;
  }
  
  @inline final def decode(rowcol : Int) : (Int,Int) = {
    val row = rowcol % rows;
    val col = rowcol / rows;
    check(row,col);
    (row,col);
  }

  override def activeDomain = new MergeableSet[(Int,Int)] {
    override def size = hashmap.size;
    override def contains(key : (Int,Int)) =
      hashmap.containsKey(index(key._1,key._2));
    override def iterator = activeKeys;
  }
  
  override def activeElements = new Iterator[((Int,Int),Double)] {
    val iter = hashmap.int2DoubleEntrySet.fastIterator();
    override def hasNext = iter.hasNext;
    override def next = {
      val e = iter.next;
      val rowcol = decode(e.getIntKey);
      ((rowcol), e.getDoubleValue);
    }
  }
  
  override def activeKeys = new Iterator[(Int,Int)] {
    val iter = hashmap.keySet.iterator;
    override def hasNext = iter.hasNext;
    override def next = {
      val e = iter.next;
      val rowcol = decode(e.intValue);
      rowcol;
    }
  }
  
  override def activeValues = new Iterator[Double] {
    val iter = hashmap.values.iterator;
    override def hasNext = iter.hasNext;
    override def next = iter.nextDouble;
  }
  
  override def zero = {
    default = 0;
    hashmap.clear();
  }
  

  /**
  * Creates a vector "like" this one, but with zeros everywhere.
  */
  def like = new SparseHashMatrix(rows,cols);

  /**
  * Creates a vector "like" this one, but with zeros everywhere.
  */
  def vectorLike(size:Int) = new SparseHashVector(size);

  /**
  * Creates a matrix "like" this one, but with zeros everywhere.
  */
  def matrixLike(rows:Int, cols:Int) = new SparseHashMatrix(rows,cols);

  override def copy : SparseHashMatrix =
    new SparseHashMatrix(rows, cols, hashmap.clone.asInstanceOf[Int2DoubleOpenHashMap]);
}

object SparseHashMatrix {
  def apply(rows : Int, cols: Int)(default : Double) = {
    val sv = new SparseHashMatrix(rows,cols);
    sv.default= default;
    sv;
  }
}
