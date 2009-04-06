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
import scalala.collection.MergeableSet;
import scalala.collection.domain.{Domain, IntSpanDomain};

import scalala.tensor.Tensor.CreateException;

/**
 * A sparse vector implementation based on an array of indeces and
 * an array of values.  Inserting a new value takes on the order
 * of the number of non-zeros.  Getting a value takes on the order
 * of the log of the number of non-zeros, with special constant
 * time shortcuts for getting the previously accessed element or
 * its successor.
 * 
 * @author dramage
 */
class SparseVector(domainSize : Int, nonzeros : Int) extends Vector {
  if (domainSize < 0)
    throw new IllegalArgumentException("Invalid domain size: "+domainSize);

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
    for (i <- 1 until used) {
      if (inIndex(i-1) > inIndex(i)) {
        throw new IllegalArgumentException("Input index is not sorted at "+i);
      }
    }
    for (i <- 0 until used) {
      if (inIndex(i) < 0) {
        throw new IllegalArgumentException("Input index is less than 0 at "+i);
      }
    }
    
    data = inData;
    index = inIndex;
    used = inUsed;
    lastOffset = -1;
    lastIndex = -1;
  }
  
  /** Data array will be reassigned as the sparse vector grows. */
  var data : Array[Double] = new Array[Double](nonzeros);
  
  /** Index will be reassigned as the sparse vector grows. */
  var index : Array[Int] = new Array[Int](nonzeros);
  
  /** How many elements of data,index are used. */
  var used : Int = 0;
  
  /** The previous index and offset found by apply or update. */
  var lastOffset = -1;
  var lastIndex = -1;

  // constructors
  
  /** Constructs a new SparseVector with initially 0 allocation. */
  def this(size : Int) =
    this(size, 0);
  
  override def size = domainSize;
  
  override def activeDomain = new MergeableSet[Int] {
    override def size = used;
    override def contains(i : Int) = findOffset(i) >= 0;
    override def elements = index.take(used).elements;
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
    use(new Array[Int](nonzeros), new Array[Double](nonzeros), 0);
  }
  
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

      //System.err.println("-- "+(lastIndex,lastOffset,i,begin,end))
      assert(begin >= 0 && end >= begin, "Invalid range: "+begin+" to "+end);
      
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
          else if (data.length > 1024) { data.length + 1024 }
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
  
  override def copy = {
    val rv = new SparseVector(size, 0);
    rv.use(index.toArray, data.toArray, used);
    rv.default = this.default;
    rv;
  }
  
  override def create[J](domain : Domain[J]) : Tensor[J] = domain match {
    case IntSpanDomain(0,len) => new SparseVector(size);
    case _ => throw new CreateException("Cannot create sparse with domain "+domain);
  }
}
