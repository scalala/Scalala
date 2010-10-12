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

package scalala;
package tensor;
package counters;

import scala.collection.JavaConversions._;

import Scalala._;
import sparse._;
import collection._;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;

/**
* The basic interface for Counters, which are sort of likes maps from T to Int.
*
* An IntCounter is a PartialMap[T,Int], for scalala.
*
* Normally you will want to use object Counters's for creating Counters.
*
* @author dlwh
*/
trait BaseIntCounter[T] extends MutablePartialMap[T,Int] with TrackedIntStatistics[T] { outer =>
  /**
  * A counter should be backed by a IntValuedMap, of any sort.
  * (We use IntValuedMap to get around excessive boxing.)
  * Default value should defer to BaseIntCounter.default
  */
  protected val counts = new Object2IntOpenHashMap[T];
  counts.defaultReturnValue(default);

  val activeDomain : MergeableSet[T] = new MergeableSet[T] {
    def contains(t:T) = counts.containsKey(t);
    def iterator = counts.keySet.iterator;
  }

  val domain : MergeableSet[T] = activeDomain;

  /**
  * Sets the count for t to be v, and calls updateStatistics.
  */
  def update(t: T, v: Int) {
    val oldV = counts.getInt(t);
    counts.put(t, v);
    updateStatistics(t,oldV,v);
  }

  def default = 0;
  def default_=(default: Int) = {
    throw new UnsupportedOperationException("Cannot set default of IntCounter");
  }

  /**
  * This should create a default counter that is a copy of this.
  */
  def copy: BaseIntCounter[T];

  /**
  * Returns the number of keys with stored values.
  */
  override def size = counts.size;

  /**
  * Returns Some(v) if this.contains(t), else None
  */
  def get(t: T) = if (counts containsKey(t)) Some(counts.getInt(t)) else None;

  /**
  * Returns true if we store a (likely) non-default value for t
  */
  def contains(t:T) = counts containsKey t;

  /**
  * Returns the value associated with t, or default if none is.
  */
  def apply(t: T) = counts.getInt(t);

  /**
  * Equivalent to this(t) += inc. it may be faster (though at the moment it isn't)
  */
  def incrementCount(t:T, inc: Int) {
    val oldV = counts.getInt(t);
    counts.put(t, inc + oldV);
    updateStatistics(t,oldV,inc + oldV);
  }

  /**
  * Adds each element of the iterable to the counter.
  */
  def ++=(kv: Iterable[(T,Int)]) = kv.foreach(+=);
  /**
  * Adds each element of the iterable to the counter.
  */
  def ++=(kv: Iterator[(T,Int)]) = kv.foreach(+=);

  /**
  * Equivalent to this(kv._1) += kv._2. (i.e., it increments the count for kv._1 by kv._2)
  */
  def +=(kv: (T,Int)) { this(kv._1) += kv._2 }

  def +=(c: BaseIntCounter[T]) { 
    for( (k,v) <- c) {
      this.incrementCount(k,v);
    }
  }

  protected [this] def maxkv : (T,Int) = {
    var maxK = counts.keySet.iterator.next;
    var maxV = Int.MinValue;
    val iter = counts.object2IntEntrySet.fastIterator;
    while (iter.hasNext) {
      val entry = iter.next;
      if (entry.getIntValue > maxV) {
        maxK = entry.getKey;
        maxV = entry.getIntValue
      }
    }
    (maxK,maxV);
  }

  protected [this] def minkv : (T,Int) = {
    var minK = counts.keySet.iterator.next;
    var minV = Int.MaxValue;
    val iter = counts.object2IntEntrySet.fastIterator;
    while (iter.hasNext) {
      val entry = iter.next;
      if (entry.getIntValue < minV) {
        minK = entry.getKey;
        minV = entry.getIntValue
      }
    }
    (minK,minV);
  }

  /**
   * Return the T with the largest count
   */
  def argmax: T = maxkv._1;

  /**
   * Return the T with the smallest count
   */
  def argmin: T = minkv._1;

  /**
   * Return the largest count
   */
  def max: Int = maxkv._2;

  /**
   * Return the smallest count
   */
  def min: Int = minkv._2;

  /** Returns the top k values in this counter */
  def maxk(k : Int) : Iterable[T] =
    TopK(k, this.keysIterator, this.apply _);

  /**
  * For each k,v in the map, it sets this(k) = f(k,v)
  */
  def transform(f: (T,Int)=>Int) {
    for( (k,v) <- activeElements) {
      update(k,f(k,v));
    }
  }

  def clear() {
    resetStatistics();
    counts.clear(); 
  }

  def /=(div: Int) = {
    val c = this
    for( (k,v) <- this) {
      c(k) = v/div;
    }
    this
  }


  def *=(mul: Int) = {
    val c = this
    for( (k,v) <- this) {
      c(k) = v*mul;
    }
    this
  }


  def map[U](f: ((T,Int))=>U):Iterable[U] = {
    activeElements.map{ kv => f(kv)} toSeq; 
  }

  def map[U](f: (T,Int)=>U):Iterable[U] = {
    activeElements.map{ case (k,v) => f(k,v)} toSeq; 
  }
}
