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

import scalala.tensor.Vector;
import scalala.collection.{MergeableSet,IntSpanSet,DomainException};

import scalala.tensor.Tensor.CreateException;
import scalala.tensor.dense.DenseVector;

/**
 * A sparse vector implementation based on an array of indeces and
 * an array of values.  Inserting a new value takes on the order
 * of the number of non-zeros.  Getting a value takes on the order
 * of the log of the number of non-default values, with special
 * constant time shortcuts for getting the previously accessed
 * element or its successor.  Note that this class is not threadsafe.
 * 
 * @author dramage
 */
class SparseVector(domainSize : Int, initialNonzeros : Int) extends Vector {
  if (domainSize < 0)
    throw new IllegalArgumentException("Invalid domain size: "+domainSize);
  
  /** Data array will be reassigned as the sparse vector grows. */
  var data : Array[Double] = new Array[Double](initialNonzeros);
  
  /** Index will be reassigned as the sparse vector grows. */
  var index : Array[Int] = new Array[Int](initialNonzeros);
  
  /** How many elements of data,index are used. */
  var used : Int = 0;
  
  /** The previous index and offset found by apply or update. */
  var lastOffset = -1;
  var lastIndex = -1;

  /** Constructs a new SparseVector with initially 0 allocation. */
  def this(size : Int) =
    this(size, 0);

  /** Use the given index and data arrays, of which the first inUsed are valid. */
  def use(inIndex : Array[Int], inData : Array[Double], inUsed : Int) = {
    if (inIndex.size != inData.size)
      throw new IllegalArgumentException("Index and data sizes do not match");
    if (inIndex.contains((x:Int) => x < 0 || x > size))
      throw new IllegalArgumentException("Index array contains out-of-range index");
    if (inIndex == null || inData == null)
      throw new IllegalArgumentException("Index and data must be non-null");
    if (inIndex.size < inUsed)
      throw new IllegalArgumentException("Used is greater than provided array");
    for (i <- 1 until used; if (inIndex(i-1) > inIndex(i))) {
      throw new IllegalArgumentException("Input index is not sorted at "+i);
    }
    for (i <- 0 until used; if (inIndex(i) < 0)) {
      throw new IllegalArgumentException("Input index is less than 0 at "+i);
    }
    
    data = inData;
    index = inIndex;
    used = inUsed;
    lastOffset = -1;
    lastIndex = -1;
  }
  
  override def size = domainSize;
  
  override def activeDomain = new MergeableSet[Int] {
    override def size = used;
    override def contains(i : Int) = findOffset(i) >= 0;
    override def elements = {
      if (used == index.length) index.elements;
      else index.elements.take(used);
    }
  }

  override def activeElements = new Iterator[(Int,Double)] {
    var offset = 0;
    override def hasNext = offset < used;
    override def next = {
      val rv = (index(offset),data(offset));
      offset += 1;
      rv;
    }
  }
  
  override def activeKeys = index.take(used).elements;
  
  override def activeValues = data.take(used).elements;

  /** Zeros this vector, return */
  override def zero() = {
    use(new Array[Int](initialNonzeros),
        new Array[Double](initialNonzeros), 0);
  }
  
  /** Records that the given index was found at this.index(offset). */
  protected final def found(index : Int, offset : Int) : Int = {
    lastOffset = offset;
    lastIndex = index;
    return offset;
  }
  
  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with -(rv+1).
   */
  def findOffset(i : Int) : Int = {
    if (i < 0)
      throw new IndexOutOfBoundsException("index is negative (" + index + ")");
    if (i >= size)
      throw new IndexOutOfBoundsException("index >= size (" + index + " >= " + size + ")");
    
    if (i == lastIndex) {
      // previous element; don't need to update lastOffset
      return lastOffset;
    } else if (used == 0) {
      // empty list; do nothing
      return -1;
    } else {
      // regular binary search
      var begin = 0;
      var end = used - 1;
      
      // narrow the search if we have a previous reference
      if (lastIndex >= 0 && lastOffset >= 0) {
        if (i < lastIndex) {
          // in range preceding last request
          end = lastOffset;
        } else {
          // in range following last request
          begin = lastOffset;
          
          if (begin + 1 <= end && index(begin + 1) == i) {
            // special case: successor of last request
            return found(i, begin + 1);
          }
        }
      }

      // this assert is for debug only
      //assert(begin >= 0 && end >= begin,
      //       "Invalid range: "+begin+" to "+end);
      
      var mid = (end + begin) >> 1;
      
      while (begin <= end) {
        mid = (end + begin) >> 1;
        if (index(mid) < i)
          begin = mid + 1;
        else if (index(mid) > i)
          end = mid - 1;
        else
          return found(i, mid);
      }
      
      // no match found, return insertion point
      if (i <= index(mid))
        return -(mid)-1;     // Insert here (before mid) 
      else 
        return -(mid + 1)-1; // Insert after mid
    }
  }
  
  override def apply(i : Int) : Double = {
    val offset = findOffset(i);
    if (offset >= 0) data(offset) else default;
  }
  
  /**
   * Sets the given value at the given index if the value is not
   * equal to the current default.  The data and
   * index arrays will be grown to support the insertion if
   * necessary.  The growth schedule doubles the amount
   * of allocated memory at each allocation request up until
   * the sparse vector contains 1024 elements, at which point
   * the growth is additive: an additional n * 1024 spaces will
   * be allocated for n in 1,2,4,8,16.  The largest amount of
   * space added to this vector will be an additional 16*1024*(8+4) = 
   * 196608 bytes, although more space is needed temporarily
   * while moving to the new arrays.
   */
  override def update(i : Int, value : Double) = {
    val offset = findOffset(i);
    if (offset >= 0) {
      // found at offset
      data(offset) = value;
    } else if (value != default) {
      // need to insert at position -(offset+1)
      val insertPos = -(offset+1);
      
      used += 1;
      
      var newIndex = index;
      var newData = data;
      
      if (used > data.length) {
        val newLength = {
          if (data.length < 8) { 8 }
          else if (data.length > 16*1024) { data.length + 16*1024 }
          else if (data.length > 8*1024)  { data.length +  8*1024 }
          else if (data.length > 4*1024)  { data.length +  4*1024 }
          else if (data.length > 2*1024)  { data.length +  2*1024 }
          else if (data.length > 1*1024)  { data.length +  1*1024 }
          else { data.length * 2 }
        }
        
        // copy existing data into new arrays
        newIndex = new Array[Int](newLength);
        newData  = new Array[Double](newLength);
        System.arraycopy(index, 0, newIndex, 0, insertPos);
        System.arraycopy(data, 0, newData, 0, insertPos);
      }
    
      // make room for insertion
      System.arraycopy(index, insertPos, newIndex, insertPos + 1, used - insertPos - 1);
      System.arraycopy(data,  insertPos, newData,  insertPos + 1, used - insertPos - 1);
      
      // assign new value
      newIndex(insertPos) = i;
      newData(insertPos) = value;
      
      // record the insertion point
      found(i,insertPos);
      
      // update pointers
      index = newIndex;
      data = newData;
    }
  }
  
  /** Compacts the vector by removing all stored default values. */
  def compact() {
    val _default = default;
      
    val nz = { // number of non-zeros
      var _nz = 0;
      var i = 0;
      while (i < used) {
        if (data(i) != _default) {
          _nz += 1;
        }
        i += 1;
      }
      _nz;
    }
    
    val newData  = new Array[Double](nz);
    val newIndex = new Array[Int](nz);
    
    var i = 0;
    var o = 0;
    while (i < used) {
      if (data(i) != _default) {
        newData(o) = data(i);
        newIndex(o) = index(i);
        o += 1;
      }
      i += 1;
    }
    
    use(newIndex, newData, nz);
  }
  
  override def copy = {
    val rv = new SparseVector(size, 0);
    rv.use(index.toArray, data.toArray, used);
    rv.default = this.default;
    rv;
  }
  
  override def create[J](domain : MergeableSet[J]) : Tensor[J] = SparseVector.create(domain);
  
  /** Uses optimized implementations. */
  override def dot(other : Tensor1[Int]) : Double = other match {
    case singleton : SingletonBinaryVector => this.apply(singleton.singleIndex);
    case sparse : SparseVector => dot(sparse);
    case binary : SparseBinaryVector => binary.dot(this);
    case dense  : DenseVector => dot(dense);
    case _ => super.dot(other);
  }
  
  /** Optimized implementation for SpraseVector dot DenseVector. */
  def dot(that : DenseVector) : Double = {
    ensure(that);
    
    val thisDefault = this.default;
    var sum = 0.0;
    if (thisDefault == 0) {
      var o = 0;
      while (o < this.used) {
        sum += (this.data(o) * that.data(this.index(o)));
        o += 1;
      }
    } else {
      var o1 = 0;
      var i2 = 0;
      while (o1 < this.used) {
        val i1 = this.index(o1);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(i2));
          o1 += 1;
          i2 += 1;
        } else { // i1 < i2
          sum += (thisDefault * that.data(i2));
          i2 += 1;
        }
      }
      // consume remander of that
      while (i2 < that.data.length) {
        sum += (thisDefault * that.data(i2));
        i2 += 1;
      }
    }
    return sum;
  }
  
  /** Optimized implementation for SpraseVector dot SparseVector. */
  def dot(that : SparseVector) : Double = {
    ensure(that);
    
    var o1 = 0;    // offset into this.data, this.index
    var o2 = 0;    // offset into that.data, that.index
    var sum = 0.0; // the dot product
    
    val thisDefault = this.default;
    val thatDefault = that.default;
    
    if (thisDefault == 0 && thatDefault == 0) {
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
        } else if (i1 < i2) {
          o1 += 1;
        } else { // i2 > i1
          o2 += 1;
        }
      }
    } else if (thisDefault == 0) { // && thatDefault != 0
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
        } else if (i1 < i2) {
          sum += (thatDefault * this.data(o1));
          o1 += 1;
        } else { // i2 > i1
          o2 += 1;
        }
      }
      // consume remainder of this
      while (o1 < this.used) {
        sum += (thatDefault * this.data(o1));
        o1 += 1;
      }
    } else if (thatDefault == 0) { // thisDefault != 0
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
        } else if (i1 < i2) {
          o1 += 1;
        } else { // i2 > i1
          sum += (thisDefault * that.data(o2));
          o2 += 1;
        }
      }
      // consume remainder of that
      while (o2 < that.used) {
        sum += (thisDefault * that.data(o2));
        o2 += 1;
      }
    } else { // thisDefault != 0 && thatDefault != 0
      var counted = 0;
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
          counted += 1;
        } else if (i1 < i2) {
          sum += (thatDefault * this.data(o1));
          o1 += 1;
          counted += 1;
        } else { // i2 > i1
          sum += (thisDefault * that.data(o2));
          o2 += 1;
          counted += 1;
        }
      }
      // consume remainder of this
      while (o1 < this.used) {
        sum += (thatDefault * this.data(o1));
        o1 += 1;
        counted += 1;
      }
      // consume remainder of that
      while (o2 < that.used) {
        sum += (thisDefault * that.data(o2));
        o2 += 1;
        counted += 1;
      }
      // add in missing product total
      sum += ((size - counted) * (thisDefault * thatDefault));
    }
    
    return sum;
  }
}

trait SparseVectorTest extends scalala.library.Library with scalala.library.Random with scalala.ScalalaTest {
  test("SparseVector:General") {
    val sparse = new SparseVector(10);
    val dense  = new scalala.tensor.dense.DenseVector(10);
    
    val values = List((2,2),(4,4),(3,3),(0,-1),(5,5),(1,1),(9,9),(7,7),(8,8),(3,3));
    
    for ((index,value) <- values) {
      sparse(index) = value;
      dense(index) = value;
      assertEquals(dense, sparse);
    }
  }
  
  test("SparseVector:Dot") {
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

object SparseVector {
  /** Creates a general sparse tensor for the requested domain. */
  def create[J](domain : MergeableSet[J]) : Tensor[J] = domain match {
    case IntSpanSet(0,len) => new SparseVector(len);
    case _ => throw new CreateException("Cannot create sparse with domain "+domain);
  }
}
