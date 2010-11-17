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
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;

/**
* The basic interface for Counters, which are sort of likes maps from T to Double.
*
* Normally you will want to use object Counters for creating Counters.
*
* @author dlwh
*/
@serializable
@SerialVersionUID(1)
trait BaseDoubleCounter[T] extends Tensor1[T] with TrackedStatistics[T] { outer =>
  /**
  * A counter should be backed by a DoubleValuedMap, of any sort.
  * (We use DoubleValuedMap to get around excessive boxing.)
  * Default value should defer to BaseDoubleCounter.default
  */
  protected val counts = new Object2DoubleOpenHashMap[T]();

  override def default_=(d: Double) = counts.defaultReturnValue(d);
  override def default = counts.defaultReturnValue;

  val activeDomain : MergeableSet[T] = new MergeableSet[T] {
    def contains(t:T) = counts.containsKey(t);
    def iterator = counts.keySet.iterator;
    
  }
  val domain : MergeableSet[T] = activeDomain;

  override def valuesIterator:Iterator[Double] = new Iterator[Double] {
    val iter = counts.values.doubleIterator;
    override def hasNext = iter.hasNext;
    override def next = iter.nextDouble;
  }

  override def foreach[A](f: ((T,Double))=>A) {
    val iter = counts.object2DoubleEntrySet.fastIterator;
    while(iter.hasNext) {
      val n = iter.next;
      f((n.getKey,n.getDoubleValue));
    }
  }

  def foreach[A](f: (T,Double)=>A) {
    val iter = counts.object2DoubleEntrySet.fastIterator;
    while(iter.hasNext) {
      val n = iter.next;
      f(n.getKey,n.getDoubleValue);
    }
  }

  /**
  * Sets the count for t to be v, and calls updateStatistics.
  */
  def update(t: T, v: Double) {
    val oldV = counts.getDouble(t);
    counts.put(t, v);
    updateStatistics(t,oldV,v);
  }

  /**
  * Returns the number of keys with stored values.
  */
  override def size = counts.size;

  /**
  * Returns Some(v) if this.contains(t), else None
  */
  def get(t: T) = if (counts containsKey(t)) Some(counts.getDouble(t)) else None;

  /**
  * Returns true if we store a (likely) non-default value for t
  */
  def contains(t:T) = counts containsKey t;

  /**
   * Returns the value associated with t, or default if none is.
   */
  override def apply(t: T) = counts.getDouble(t);

  /**
  * Equivalent to this(t) += inc. it may be faster (though at the moment it isn't)
  */
  def incrementCount(t:T, inc: Double) {
    val oldV = counts.getDouble(t);
    counts.put(t, inc + oldV);
    updateStatistics(t,oldV,inc + oldV);
  }

  /**
  * Adds each element of the iterable to the counter.
  */
  def ++=(kv: Iterable[(T,Double)]) = kv.foreach(+=);
  /**
  * Adds each element of the iterable to the counter.
  */
  def ++=(kv: Iterator[(T,Double)]) = kv.foreach(+=);

  /**
  * Equivalent to this(kv._1) += kv._2. (i.e., it increments the count for kv._1 by kv._2)
  */
  def +=(kv: (T,Double)) { this(kv._1) += kv._2 }

  protected [this] def maxkv : (T,Double) = {
    var maxK = counts.keySet.iterator.next;
    var maxV = Double.NegativeInfinity;
    val iter = counts.object2DoubleEntrySet.fastIterator;
    while (iter.hasNext) {
      val entry = iter.next;
      if (entry.getDoubleValue > maxV) {
        maxK = entry.getKey;
        maxV = entry.getDoubleValue;
      }
    }
    (maxK,maxV);
  }

  protected [this] def minkv : (T,Double) = {
    var minK = counts.keySet.iterator.next;
    var minV = Double.PositiveInfinity;
    val iter = counts.object2DoubleEntrySet.fastIterator;
    while (iter.hasNext) {
      val entry = iter.next;
      if (entry.getDoubleValue < minV) {
        minK = entry.getKey;
        minV = entry.getDoubleValue;
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
  def max: Double = maxkv._2;

  /**
   * Return the smallest count
   */
  def min: Double = minkv._2;

  /** Returns the top k values in this counter */
  def maxk(k : Int) : Iterable[T] =
    TopK(k, this.keysIterator, this.apply _);

  protected[counters] override def ensure(otherDomain: PartialMap[T,Double]) {
    // no need to do anything here.
  }

  /**
  * For each k,v in the map, it sets this(k) = f(k,v)
  */
  def transform(f: (T,Double)=>Double) {
    for( (k,v) <- activeElements) {
      update(k,f(k,v));
    }
  }

  def map[U](f: ((T,Double))=>U):Iterable[U] = {
    activeElements.map{ kv => f(kv)} toSeq; 
  }

  def map[U](f: (T,Double)=>U):Iterable[U] = {
    activeElements.map{ case (k,v) => f(k,v)} toSeq; 
  }
}
