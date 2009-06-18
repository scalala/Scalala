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

import scalala.tensor.Vector;
import scalala.collection.{MergeableSet,IntSpanSet,DomainException};

import scalala.tensor.Tensor.CreateException;
import scalala.collection.DomainException;
import scalala.tensor.dense.DenseVector;

/**
 * Sparse vector backed by an open address hash table with expected
 * constant time access and updates.  This class wraps FastUtil's
 * Int2DoubleOpenHashMap.
 */
@serializable
@SerialVersionUID(1)
class SparseHashVector(override val size : Int, protected val hashmap : Int2DoubleOpenHashMap) extends Vector {
  
  /** Primary constructor, based only on size of the map with a new backing. */
  def this(size : Int) =
    this(size, new Int2DoubleOpenHashMap());
  
  override def default : Double =
    hashmap.defaultReturnValue();
  
  override def default_=(value : Double) =
    hashmap.defaultReturnValue(value);
  
  /** Returns the value associated with the given key. */
  override def apply(key : Int) = {
    check(key);
    hashmap.get(key);
  }
  
  override def update(key : Int, value : Double) = {
    check(key);
    hashmap.put(key, value);
  }
  
  override def activeDomain = new MergeableSet[Int] {
    override def size = hashmap.size;
    override def contains(key : Int) = hashmap.containsKey(key);
    override def elements = activeKeys;
  }
  
  override def activeElements = new Iterator[(Int,Double)] {
    val iter = hashmap.int2DoubleEntrySet.fastIterator();
    override def hasNext = iter.hasNext;
    override def next = {
      val e = iter.next;
      (e.getIntKey, e.getDoubleValue);
    }
  }
  
  override def activeKeys = new Iterator[Int] {
    val iter = hashmap.keySet.iterator;
    override def hasNext = iter.hasNext;
    override def next = iter.nextInt;
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
  
  override def copy : SparseHashVector =
    new SparseHashVector(size, hashmap.clone.asInstanceOf[Int2DoubleOpenHashMap]);
  
  override def create[J](domain : MergeableSet[J]) : Tensor[J] = domain match {
    case IntSpanSet(0,len) => new SparseHashVector(size);
    case _ => throw new CreateException("Cannot create sparse with domain "+domain);
  }
  
  //
  // specialized dot product implementations
  //
  
  /**
   * Provides specialized dot product implementations for singletons
   * and dense vectors.
   */
  override def dot(other : Tensor1[Int]) : Double = other match {
    case singleton : SingletonBinaryVector => this.apply(singleton.singleIndex);
    case dense : DenseVector => dot(dense);
    case _ => super.dot(other);
  }
  
  def dot(that : DenseVector) : Double = {
    ensure(that);
    
    if (this.default == 0) {
      val iter = hashmap.int2DoubleEntrySet.fastIterator();
      var sum = 0.0;
      while (iter.hasNext) {
        val e = iter.next;
        sum += e.getDoubleValue * that.data(e.getIntKey);
      }
      return sum;
    } else {
      return super.dot(that);
    }
  }
}

object SparseHashVector {
  def apply(size : Int)(default : Double) = {
    val sv = new SparseHashVector(size);
    sv += default;
    sv;
  }
}

trait SparseHashVectorTest extends scalala.library.Library with scalala.library.Random with scalala.ScalalaTest {
  test("SparseHashVector:General") {
    val sparse = new SparseVector(10);
    val dense  = new scalala.tensor.dense.DenseVector(10);
    
    val values = List((2,2),(4,4),(3,3),(0,-1),(5,5),(1,1),(9,9),(7,7),(8,8),(3,3));
    
    for ((index,value) <- values) {
      sparse(index) = value;
      dense(index) = value;
      assertEquals(dense, sparse);
    }
  }
  
  test("SparseHashVector:Dot") {
    val x = new SparseVector(10);
    val y = new SparseVector(10);
    val d = rand(10);
    
    def densedot(a : Vector, b : Vector) =
      new DenseVector(a.toArray) dot new DenseVector (b.toArray);

    def checks() = {
      assertEquals(densedot(x,y), x dot y);
      assertEquals(densedot(y,x), y dot x);
      assertEquals(densedot(x,d), x dot d);
      assertEquals(densedot(d,x), d dot x);
      assertEquals(densedot(y,d), y dot d);
      assertEquals(densedot(d,y), d dot y);
    }
    
    x(2) = 3;
    y(2) = 4;
    checks();
    
    x(7) = 2;
    y += 1;
    checks();
    
    y -= 1;
    y(7) = .5;
    checks();
    
    x += 1;
    y(8) = 2;
    checks();
    
    y += 1;
    checks();
  }
}
