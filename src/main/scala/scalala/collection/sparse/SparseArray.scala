/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

package scalala.collection.sparse;

import scala.collection.generic._;
import scala.reflect.ClassManifest;
import scala.collection.mutable._;

/**
 * <p>Nearly-drop-in replacement for Array[T](length) that does not store
 * default array values.  Internally, the SparseArray data structure
 * stores an array of indexes in packed, sorted order, and a
 * corresponding packed array of values.  Updates are linear in
 * the number of non-zero array elements, while accesses are
 * logarithmic.  This class is not threadsafe for simultaneous
 * reads and writes, but more than one thread can read simultaneously
 * if no threads are writing.</p>
 *
 * <p>Note that the DefaultArrayValue must be a constant reference -- no
 * clever tricks like creating new instances of type T are supported
 * by the nature of the way this map works.  Otherwise, iterating
 * over the size of the array would cause the SparseArray to become
 * (inefficiently) dense.</p>
 *
 * @param length The virtual length of the array.
 * @param index The indices of the array, in packed sorted order.
 * @param data The data of the array, in corresponding positions to index.
 * @param used The number of used elements in index and data.
 * @param initialActiveLength The initial length of the sparse data structures
 *   when creating new instances.
 *
 * @author dlwh, dramage
 */
final class SparseArray[@specialized T]
(val length : Int, protected var index : Array[Int], protected var data : Array[T], protected var used : Int, initialActiveLength : Int)
(implicit m : ClassManifest[T], df : DefaultArrayValue[T]) {

  def this(length : Int, initialActiveLength : Int = 3)(implicit m : ClassManifest[T], d : DefaultArrayValue[T]) =
    this(length, new Array[Int](initialActiveLength), new Array[T](initialActiveLength), 0, initialActiveLength)(m, d);

  { // check rep invariants
    require(length >= 0, "Length must be non-negative");
    require(used >= 0 && used <= length, "Used must be <= length and >= 0");
    if (used > 0) {
      require(index(0) >= 0, "Indexes must be ordered and non-negative");
      var i = 1;
      while (i < used) {
        require(index(i-1) < index(i) && index(i) >= 0, "Indexes must be ordered and non-negative");
        i +=1;
      }
    }
  }

  /** Default value. */
  val default = df.value;

  /** Iterator over used indexes and values */
  def iterator = Iterator.range(0, used).map(i => (index(i),data(i)));

  /** Used indexes. */
  def indexIterator = index.iterator.take(used);

  /** Used values. */
  def valueIterator = data.iterator.take(used);

  /** Key value pairs of non-default entries. */
  def activeIterator = indexIterator zip valueIterator;

  /** A copy of the keys in this array. */
  def indexArray : Array[Int] = index.take(used);

  /** A copy of the values in this array. */
  def valueArray : Array[T] = data.take(used);

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with ~rv.
   */
  private def findOffset(i : Int) : Int = {
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException("Index "+i+" out of bounds [0,"+used+")");

    if (used == 0) {
      // empty list; do nothing
      return -1;
    } else if (i > index(used-1)) {
      // special case for end of list - this is a big win for growing sparse arrays
      return ~used;
    } else {
      // regular binary search from begin to end (inclusive)
      var begin = 0;
      var end = used - 1;

      // Simple optimization: position i can't be after offset i.
      if(end > i)
        end = i;

      var mid = (end + begin) >> 1;
      while (begin <= end) {
        mid = (end + begin) >> 1;
        if (index(mid) < i)
          begin = mid + 1;
        else if (index(mid) > i)
          end = mid - 1;
        else
          return mid;
      }

      // no match found, return insertion point
      if (i <= index(mid))
        return ~mid;       // Insert here (before mid)
      else
        return ~(mid + 1); // Insert after mid
    }
  }

  def apply(i : Int) : T = {
    val offset = findOffset(i);
    if (offset >= 0) data(offset) else default;
  }

  def get(i: Int) : Option[T] = {
    val offset = findOffset(i);
    if (offset >= 0) Some(data(offset)) else None;
  }

  def getOrElse(i : Int, value : T) : T = {
    val offset = findOffset(i);
    if (offset >= 0) data(offset) else value;
  }

  /** Returns the size of the array. */
  def activeLength = used;

  /** Returns the index of the item stored at the given offset. */
  def indexAt(offset : Int) : Int = {
    if (offset >= used) throw new ArrayIndexOutOfBoundsException();
    index(offset);
  }

  /** Returns the value of the item stored at the given offset. */
  def valueAt(offset : Int) : T = {
    if (offset >= used) throw new ArrayIndexOutOfBoundsException();
    data(offset);
  }

  /**
   * Sets the given value at the given index if the value is not
   * equal to the current default.  The data and
   * index arrays will be grown to support the insertion if
   * necessary.  The growth schedule doubles the amount
   * of allocated memory at each allocation request up until
   * the sparse array contains 1024 values, at which point
   * the growth is additive: an additional n * 1024 spaces will
   * be allocated for n in 1,2,4,8,16.  The largest amount of
   * space added to this vector will be an additional 16*1024*(sizeof(T)+4),
   * which is 196608 bytes at a time for a SparseVector[Double],
   * although more space is needed temporarily while moving to the
   * new arrays.
   */
  def update(i : Int, value : T) = {
    val offset = findOffset(i);
    if (offset >= 0) {
      // found at offset
      data(offset) = value;
    } else if (value != default) {
      // need to insert at ~offset
      val insertPos = ~offset;

      used += 1;

      if (used > data.length) {
        // need to grow array
        val newLength = {
          if      (data.length < 0x0400) { data.length * 2 }
          else if (data.length < 0x0800) { data.length + 0x0400 }
          else if (data.length < 0x1000) { data.length + 0x0800 }
          else if (data.length < 0x2000) { data.length + 0x1000 }
          else if (data.length < 0x4000) { data.length + 0x2000 }
          else { data.length + 0x4000 };
        };

        // copy existing data into new arrays
        val newIndex = new Array[Int](newLength);
        val newData  = new Array[T](newLength);
        System.arraycopy(index, 0, newIndex, 0, insertPos);
        System.arraycopy(data, 0, newData, 0, insertPos);

        System.arraycopy(index, insertPos, newIndex, insertPos + 1, used - insertPos - 1);
        System.arraycopy(data,  insertPos, newData,  insertPos + 1, used - insertPos - 1);

        // update pointers
        index = newIndex;
        data = newData;
      } else if (used - insertPos > 1) {
        // need to make room for new element mid-array
        System.arraycopy(index, insertPos, index, insertPos + 1, used - insertPos - 1);
        System.arraycopy(data,  insertPos, data,  insertPos + 1, used - insertPos - 1);
      }

      // assign new value
      index(insertPos) = i;
      data(insertPos) = value;
    }
  }

  /** Clears this array, resetting to the initial size. */
  def clear() {
    use(new Array[Int](initialActiveLength), new Array[T](initialActiveLength), 0);
  }

  /** Pre-allocate space in this array for all active indexes in other. */
  def reserve[O](other : SparseArray[O]) {
    val rv = new SparseArray[T](this.length, this.used + other.used);
    var i = 0;
    var j = 0;
    while (j < other.used) {
      val indexI = index(i);
      val indexJ = index(j);
      while (i < used && indexI < indexJ) {
        rv(indexI) = data(i);
        i += 1;
      }
      rv(indexJ) = this(indexJ);
      j += 1;
    }
    while (i < used) {
      rv(index(i)) = data(i);
      i += 1;
    }
    use(rv.index, rv.data, rv.used);
  }

  /** Compacts the array by removing all stored default values. */
  def compact() {
    val nz = { // number of non-zeros
      var _nz = 0;
      var i = 0;
      while (i < used) {
        if (data(i) != default) {
          _nz += 1;
        }
        i += 1;
      }
      _nz;
    }

    val newData  = new Array[T](nz);
    val newIndex = new Array[Int](nz);

    var i = 0;
    var o = 0;
    while (i < used) {
      if (data(i) != default) {
        newData(o) = data(i);
        newIndex(o) = index(i);
        o += 1;
      }
      i += 1;
    }

    use(newIndex, newData, nz);
  }


  /** Use the given index and data arrays, of which the first inUsed are valid. */
  private def use(inIndex : Array[Int], inData : Array[T], inUsed : Int) = {
    // no need to rep-check since method is private and all callers satisfy
    // these invariants.

    data = inData;
    index = inIndex;
    used = inUsed;
  }

  private def checkrep() {
    if (index == null || data == null)
      throw new IllegalArgumentException("Index and data must be non-null");
    if (index.length != data.length)
      throw new IllegalArgumentException("Index and data sizes do not match");
    if (index.length < used)
      throw new IllegalArgumentException("Used is greater than provided array");
    if (index(0) < 0 || index(0) >= used)
      throw new IllegalArgumentException("use inIndex out of range contains illegal offset @ 0");
    var i = 1;
    while (i < used) {
      if (index(i) < 0 || index(i) >= used || index(i) < index(i-1))
        throw new IllegalArgumentException("use inIndex out of range contains illegal offset @ "+i);
      i += 1;
    }
  }

  /**
   * Maps all values.  If f(this.default) is not equal to the new default
   * value, the result will be dense (and much less efficient than just
   * storing an Array).
   */
  def map[B:ClassManifest:DefaultArrayValue](f : T=>B) : SparseArray[B] = {
    if (default == null || f(default) == implicitly[DefaultArrayValue[B]].value) {
      val newIndex = new Array[Int](used);
      val newData = new Array[B](used);
      var i = 0;
      while (i < used) {
        newIndex(i) = index(i);
        newData(i) = f(data(i));
        i += 1;
      }
      new SparseArray[B](length, newIndex, newData, used, initialActiveLength);
    } else {
      val mappedDefault = f(default);
      val newIndex = Array.range(0, length);
      val newData = new Array[B](length);
      var i = 0;
      var o = 0;
      while (i < used) {
        while (o < index(i)) {
          newData(o) = mappedDefault;
          o += 1;
        }
        newData(o) = f(data(i));
        o += 1;
        i += 1;
      }
      while (o < length) {
        newData(o) = mappedDefault;
        o += 1;
      }
      new SparseArray[B](length, newIndex, newData, length, initialActiveLength);
    }
  }

  /**
   * Tranforms all elements this array by applying the given function. If
   * f(default) == default, then only updates non-default values.  Otherwise,
   * the map essentially becomes dense -- not so efficient an operation!
   */
  def transform(f : T=>T) = {
    val newDefault = f(default);
    if (newDefault == default) {
      var i = 0;
      while (i < used) {
        data(i) = f(data(i));
        i += 1;
      }
    } else {
      val newIndex = Array.range(0, length);
      val newData = new Array[T](length);
      var i = 0;
      var o = 0;
      while (i < used) {
        while (o < index(i)) {
          newData(o) = newDefault;
          o += 1;
        }
        newData(o) = f(data(i));
        o += 1;
        i += 1;
      }
      while (o < length) {
        newData(o) = newDefault;
        o += 1;
      }
      use(newIndex, newData, length);
    }
  }

  /** Applies the given function to each non-default element. */
  def foreachActive[U](f: ((Int,T) => U)) {
    var i = 0;
    while(i < used) {
      f(index(i), data(i));
      i += 1;
    }
  }

  /** Applies the given function to each non-default element. */
  def foreachActive[U](f: T => U) {
    var i = 0;
    while(i < used) {
      f(data(i));
      i += 1;
    }
  }

  def toArray =
    Array.tabulate(length)(apply);

  def toList =
    List.tabulate(length)(apply);

  def toIndexedSeq =
    List.tabulate(length)(apply);

  def toMap =
    (indexIterator zip valueIterator).toMap;

  override def hashCode = {
    var rv = 0;
    var i = 0;
    while (i < used) {
      if (data(i) != default) {
        rv += 17*rv + data(i).hashCode*7 + index(i);
      }
      i += 1;
    }
    rv;
  }

  override def equals(other : Any) : Boolean = other match {
    case that : SparseArray[_] =>
      var thisI = 0;
      var thatI = 0;
      while (thisI < this.used && thatI < that.used) {
        if (this.index(thisI) < that.index(thatI)) {
          if (this.data(thisI) != that.default) return false;
          thisI += 1;
        } else if (that.index(thatI) < this.index(thisI)) {
          if (that.data(thatI) != this.default) return false;
          thatI += 1;
        } else if (this.index(thisI) == that.index(thatI)) {
          if (this.data(thisI) != that.data(thatI)) return false;
          thisI += 1;
          thatI += 1;
        }
      }
      while (thisI < this.used) {
        if (this.data(thisI) != that.default) return false;
        thisI += 1;
      }
      while (thatI < that.used) {
        if (that.data(thatI) != this.default) return false;
        thatI += 1;
      }
      true;
    case _ => false;
  }
}

object SparseArray {
  def apply[@specialized T:ClassManifest:DefaultArrayValue](values : T*) = {
    val rv = new SparseArray[T](values.length);
    var i = 0;
    for (v <- values) {
      rv(i) = v;
      i += 1;
    }
    rv.compact;
    rv;
  }

  /**
   * Creates a SparseArray filled with the given value.  The value function
   * is called once initially to test if the returned value is equal to the
   * DefaultArrayValue - if so, an empty SparseArray with initialActiveLength
   * non-zero entries is returned.  Otherwise, an inefficient "dense"
   * SparseArray is returned.
   *
   * @author dramage
   */
  def fill[@specialized T:ClassManifest:DefaultArrayValue](length : Int, initialActiveLength : Int = 3)(value : =>T) : SparseArray[T] = {
    if (value != implicitly[DefaultArrayValue[T]].value) {
      val rv = new SparseArray[T](length = length, initialActiveLength = length);
      var i = 0;
      while (i < length) {
        rv(i) = value;
        i += 1;
      }
      rv;
    } else {
      new SparseArray[T](length = length, initialActiveLength = length);
    }
  }

  def create[@specialized T:ClassManifest:DefaultArrayValue](length : Int)(values : (Int,T)*) = {
    val rv = new SparseArray[T](length = length, initialActiveLength = values.length);
    for ((k,v) <- values) {
      rv(k) = v;
    }
    rv;
  }

  def tabulate[@specialized T:ClassManifest:DefaultArrayValue](length : Int, initialActiveLength : Int = 3)(fn : (Int => T)) = {
    val rv = new SparseArray[T](length = length, initialActiveLength = initialActiveLength);
    var i = 0;
    while (i < length) {
      val v = fn(i);
      if (v != rv.default) {
        rv(i) = v;
      }
      i += 1;
    }
    rv.compact;
    rv;
  }
}


/**
 * Default value of type T as used by SparseArray.
 * 
 * Note that this is not the same as a general default value for T,
 * say as used by DomainMap, because this value must be null for
 * all references types in order for SparseArray to work as expected.
 * 
 * @author dramage
 */
sealed trait DefaultArrayValue[@specialized T] {
  def value : T;
}

object DefaultArrayValue {
  implicit object IntDefaultArrayValue extends DefaultArrayValue[Int] {
    override def value = 0;
  }

  implicit object ShortDefaultArrayValue extends DefaultArrayValue[Short] {
    override def value = 0.toShort;
  }

  implicit object LongDefaultArrayValue extends DefaultArrayValue[Long] {
    override def value = 0l;
  }

  implicit object ByteDefaultArrayValue extends DefaultArrayValue[Byte] {
    override def value = 0.toByte;
  }

  implicit object CharDefaultArrayValue extends DefaultArrayValue[Char] {
    override def value = 0.toChar;
  }

  implicit object FloatDefaultArrayValue extends DefaultArrayValue[Float] {
    override def value = 0.0f;
  }

  implicit object DoubleDefaultArrayValue extends DefaultArrayValue[Double] {
    override def value = 0.0;
  }

  implicit object BooleanDefaultArrayValue extends DefaultArrayValue[Boolean] {
    override def value = false;
  }

  val refDefault = new DefaultArrayValue[AnyRef] {
    override def value : AnyRef = null;
  }

  implicit def ObjectDefaultArrayValue[T<:AnyRef] =
    refDefault.asInstanceOf[DefaultArrayValue[T]];
}
