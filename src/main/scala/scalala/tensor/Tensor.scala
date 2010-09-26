/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package tensor;

import domain._;
import generic.collection._;
import generic.tensor._;

import mutable.TensorBuilder;

/**
 * A Tensor is a DomainFunction backed by an IterableDomain, i.e. it is
 * a function for which its domain can be enumerated, so that we can support
 * map-like operations of forach, mapValues, etc.
 *
 * @author dramage
 */
trait TensorLike
[@specialized(Int, Long) A,
 @specialized(Int, Long, Float, Double, Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D],
 +Repr<:Tensor[A,B]]
extends DomainFunction[A, B, D] with operators.NumericOps[Repr]
{
self =>

  protected type Self = Repr;

  def repr : Repr = this.asInstanceOf[Repr];

  /** Provides information about the underlying scalar value type B. */
  implicit val scalar : Scalar[B];

  /** Constructs a builder on the same domain as this tensor but for type C. */
  def newBuilder[C:Scalar] : TensorBuilder[A,C,Tensor[A,C]] =
  new TensorBuilder[A,C,Tensor[A,C]] {
    val rv = mutable.Tensor[A,C](self.domain);
    def update(k : A, v : C) = rv(k) = v;
    def result = rv;
  }

//  //
//  // for-comprehensions.
//  //
//  // Note:
//  // 1. flatMap doesn't make sense here because the value types are
//  //    always scalar, so you can't do { for ((k,v) <- tensor; value <- v) yield ... }.
//  //    Although I guess you might want to iterate over they key types, k,
//  //    but I'm not supporting that.
//  //
//  // 2. filter doesn't make sense because removing some elements from
//  //    the domain is not well defined.
//  //
//
//  /**
//   * Applies the given function to each element of the domain and
//   * its corresponding value (boxed version used by "for").
//   */
//  def foreach[U](f: ((A,B)) => U) : Unit =
//    this.foreach((k,v) => f((k,v)));
//
//  /**
//   * For-comprehension support for "for ((k,v) <- x) yield ..."
//   */
//  def map[O, That](f: ((A,B)) => O)
//  (implicit map: CanMapValues[Repr, B, O, That]): That =
//    this.map((k,v) => f((k,v)));

  //
  // Collection contents
  //

  /**
   * Applies the given function to each element of the domain and
   * its corresponding value.
   */
  def foreach[U](fn: (A,B) => U) : Unit =
    for (k <- domain) fn(k, apply(k));

  /**
   * Applies the given function to each value in the map (one for
   * each element of the domain, including zeros).
   */
  def foreachValue[U](fn : (B=>U)) =
    this.foreach((k,v) => fn(v));

  /**
   * Applies the given function to all elements of the domain
   * that have a non-zero values (and possibly some that have zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def foreachNonZero[U](fn : ((A,B)=>U)) : Boolean = {
    this.foreach(fn);
    true;
  }

  /**
   * Applies the given function to every non-zero value in the map
   * (and possibly some zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def foreachNonZeroValue[U](fn : (B=>U)) = {
    this.foreachValue(fn);
    true;
  }

  /** Iterates over all elements in the domain and the corresponding value. */
  def iterator : Iterator[(A,B)] =
    domain.iterator.map(k => (k,this(k)));

  /** Iterates over the values in the map. */
  def valuesIterator : Iterator[B] =
    domain.iterator.map(this);

  /** Returns the keys for which the given predicate is true. */
  def find(p : B => Boolean) : Iterable[A] =
    domain.filter(this andThen p);
  
  /**
   * Constructs a view of this map on which calls to mapValues are
   * chained together and lazily evaluated.
   */
  def view[That](implicit bf : CanView[Repr,That]) : That =
    bf.apply(repr);

  /**
   * Returns a slice sequence of this Tensor containing only those
   * values matching the given filter criteria.
   */
  def filter[That](p: B => Boolean)
  (implicit bf : CanSliceVector[Repr, A, That]) : That =
    apply(find(p));

  /** Creates a new map containing a transformed copy of this map. */
  def map[O,That](f : (A,B) => O)
  (implicit bf : CanMapKeyValuePairs[Repr, A, B, O, That]) : That =
    bf.apply(repr, f);

  /** Creates a new map containing a transformed copy of this map. */
  def mapValues[O,That](f : B => O)
  (implicit bf : CanMapValues[Repr, B, O, That]) : That =
    bf.apply(repr, f);

  /**
   * Creates a new Tensor over the same domain using the given value
   * function to create each return value in the map.
   */
  def join[V2,RV,That](m : Tensor[A,V2])(fn : (A,B,V2) => RV)
  (implicit bf : CanJoin[Repr, Tensor[A,V2], A, B, V2, RV, That]) : That =
    bf.apply(repr, m, fn);

  //
  // Slice construction
  //

  /** The value at the given key.  Takes precedence over apply(keys : A*). */
  def apply(key : A) : B;

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : A*)
  (implicit bf : CanSliceVector[Repr, A, That]) : That =
    bf(repr, keys);

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : Traversable[A])
  (implicit bf : CanSliceVector[Repr, A, That]) : That =
    bf(repr, keys.toIndexedSeq);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : (I,A)*)
  (implicit bf : CanSliceTensor[Repr, A, I, That]) : That =
    apply[I,That](keys.toMap);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : Iterable[(I,A)])
  (implicit bf : CanSliceTensor[Repr, A, I, That]) : That =
    apply[I,That](keys.toMap);

  def apply[I,That](keys : scala.collection.Map[I,A])
  (implicit bf : CanSliceTensor[Repr, A, I, That]) : That =
    bf(repr, keys);

  //
  // Sorting
  //

  /**
   * Returns the elements of this.domain ordered by their values in this map.
   * Currently this method is not particularly efficient, as it creates several
   * in-memory arrays the size of the domain.
   */
  def argsort(implicit cm : ClassManifest[A], ord : Ordering[B]) : Array[A] =
    domain.toArray.sortWith((i:A, j:A) => ord.lt(this(i), this(j)));

  /**
   * Returns a sorted view of the current map.  Equivalent to calling
   * <code>x(x.argsort)</code>.  Changes to the sorted view are
   * written-through to the underlying map.
   */
  def sorted[That]
  (implicit bf : CanSliceVector[Repr, A, That],
   cm : ClassManifest[A], ord : Ordering[B]) : That =
    this(this.argsort);


  //
  // Collection level queries
  //

  /** Returns a key associated with the largest value in the map. */
  def argmax : A = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .max");
    }
    var max = valuesIterator.next;
    var arg = domain.iterator.next
    foreach((k,v) => if (scalar.>(v, max)) { max = v; arg = k; });
    arg;
  }

  /** Returns a key associated with the smallest value in the map. */
  def argmin : A = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .min");
    }
    var min = valuesIterator.next;
    var arg = domain.iterator.next
    foreach((k,v) => if (scalar.<(v,min)) { min = v; arg = k; });
    arg;
  }

  /** Returns the max of the values in this map. */
  def max : B = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .max");
    }
    var max = valuesIterator.next;
    foreachValue(v => { if (scalar.>(v,max)) max = v; });
    return max;
  }

  /** Returns the min of the values in this map. */
  def min : B = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .min");
    }
    var min = valuesIterator.next;
    foreachValue(v => { if (scalar.<(v,min)) min = v; })
    return min;
  }

  /** Returns the sum of the values in this map. */
  def sum : B = {
    var sum = scalar.zero;
    foreachValue(v => sum = scalar.+(sum,v));
    return sum;
  }


  //
  // Conversions
  //

  /** Returns an ordering over the domain based on the values in this map. */
  def asOrdering(implicit ord : Ordering[B]) : Ordering[A] = new Ordering[A] {
    override def compare(a : A, b : A) = ord.compare(self(a), self(b));
  }

  /** Returns an unmodifiable Map-like view of this Tensor. */
  def asMap : scala.collection.Map[A,B] = new scala.collection.Map[A,B] {
    override def keysIterator = domain.iterator;
    override def valuesIterator = self.valuesIterator;
    override def contains(key : A) = self.isDefinedAt(key);
    override def apply(key : A) = self.apply(key);
    override def iterator = self.iterator;
    override def get(key : A) =
      if (self.isDefinedAt(key)) Some(self.apply(key)) else None;
    override def - (key : A) =
      throw new UnsupportedOperationException("asMap view of Tensor is unmodifiable: use toMap");
    override def + [B1>:B](kv : (A,B1)): scala.collection.Map[A,B1] =
      throw new UnsupportedOperationException("asMap view of Tensor is unmodifiable: use toMap");
  }

  /** Creates a new copy of this Tensor as a scala map. */
  def toMap : Map[A,B] =
    this.iterator.toMap;

  // TODO: provide better toString
  override def toString = {
    val iter = iterator;
    val rv = iter.take(10).mkString("\n");
    if (iter.hasNext) {
      rv + "...\n";
    } else {
      rv;
    }
  }

  //
  // Equality
  //

  /**
   * Default implementation iterates the full domain in order, checking
   * that each function returns the same value.
   */
  override def equals(other : Any) : Boolean = other match {
    case that: Tensor[A,B] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.domain == that.domain) &&
      (domain.iterator.forall((k:A) => this(k) == that(k)))
    case _ => false;
  }

  /** From recipe in "Programming in Scala" section 28.4. */
  def canEqual(other : Any) : Boolean = other match {
    case that : Tensor[_,_] => true;
    case _ => false;
  }

  override def hashCode() =
    domain.hashCode + valuesIterator.foldLeft(1)((hash,v) => 41 * hash + v.hashCode);
}

trait Tensor
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends TensorLike[A, B, IterableDomain[A], Tensor[A, B]];

object Tensor {

  class Impl[A,B](values : Map[A,B])(implicit override val scalar : Scalar[B])
  extends Tensor[A,B] {
    override val domain : SetDomain[A] = new SetDomain(values.keySet);
    override def apply(k : A) = values(k);
  }

  def apply[A,B:Scalar](values : (A,B)*) : Tensor[A,B] =
    new Impl(values.toMap);

  implicit def canView[A, B:Scalar] =
  new CanView[Tensor[A,B],TensorView.IdentityView[A,B,Tensor[A,B]]] {
    override def apply(from : Tensor[A,B]) =
      new TensorView.IdentityViewImpl[A,B,Tensor[A,B]](from);
  }

  implicit def canMapValues[A, B, O:Scalar] =
  new CanMapValues[Tensor[A,B],B,O,Tensor[A,O]] {
    override def apply(from : Tensor[A,B], fn : (B=>O)) = {
      val builder = from.newBuilder[O];
      from.foreach((k,v) => builder(k) = fn(v));
      builder.result;
    }
  }

  implicit def canMapKeyValuePairs[A, B, O:Scalar] =
  new CanMapKeyValuePairs[Tensor[A,B],A,B,O,Tensor[A,O]] {
    override def apply(from : Tensor[A,B], fn : ((A,B)=>O)) = {
      val builder = from.newBuilder[O];
      from.foreach((k,v) => builder(k) = fn(k,v));
      builder.result;
    }
  }

  implicit def canJoin[K, V1, V2, RV:Scalar] =
  new CanJoin[Tensor[K,V1], Tensor[K,V2], K, V1, V2, RV, Tensor[K,RV]] {
    override def apply(a : Tensor[K,V1], b : Tensor[K,V2], fn : (K,V1,V2)=>RV) = {
      if (a.domain != b.domain) {
        throw new DomainException("Mismatched domains on join");
      }
      val builder = a.newBuilder[RV];
      for (k <- a.domain) { builder(k) = fn(k,a(k),b(k)); }
      builder.result;
    }
  }

  implicit def canSliceTensor[A1, A2, B:Scalar] =
  new CanSliceTensor[Tensor[A1,B], A1, A2, Tensor[A2,B]] {
    override def apply(from : Tensor[A1,B], keymap : scala.collection.Map[A2,A1]) =
      new TensorSlice.FromKeyMap[A1, A2, B, Tensor[A1,B]](from, keymap);
  }

  implicit def canSliceVector[A, B:Scalar] =
  new CanSliceVector[Tensor[A,B], A, Vector[B]] {
    override def apply(from : Tensor[A,B], keys : Seq[A]) =
      new VectorSlice.FromKeySeq[A,B,Tensor[A,B]](from, keys);
  }
}

///*
// * Distributed as part of Scalala, a linear algebra library.
// *
// * Copyright (C) 2008- Daniel Ramage
// *
// * This library is free software; you can redistribute it and/or
// * modify it under the terms of the GNU Lesser General Public
// * License as published by the Free Software Foundation; either
// * version 2.1 of the License, or (at your option) any later version.
// *
// * This library is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// * Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public
// * License along with this library; if not, write to the Free Software
// * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
// */
//package scalala;
//package tensor;
//
//import scalala.generic._;
//import scalala.collection._;
//import scalala.collection.domain._;
//
///**
// * A Tensor is a Double-valued MutableDomainMap.
// *
// * @author dramage
// */
//trait TensorLike
//[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B,
// +D<:IterableDomain[A] with DomainLike[A,D],
// +This<:Tensor[A,B]]
//extends MutableDomainMapLike[A,B,D,This] with operators.NumericOps[This] {
//
////
////  //
////  // Scalar updates.
////  //
////
////  /** Multiplies each element by the given scale factor. */
////  def *= (s : Double) =
////    if (s != 1) transformValues(_ * s);
////
////  /** Divides each element by the given scale factor. */
////  def /= (s : Double) =
////    if (s != 1) transformValues(_ / s);
////
////  /** Increments element by the given scalar. */
////  def += (s : Double) =
////    if (s != 0) transformValues(_ + s);
////
////  /** Decrements each element by the given scalar. */
////  def -= (s : Double) =
////    if (s != 0) transformValues(_ - s);
////
////  /** Raises each element to the the given power. */
////  def :^= (s : Double) =
////    if (s != 1) transformValues(v => math.pow(v, s));
////
////  /** Each element becomes itself modulo the given scalar. */
////  def %= (s : Double) =
////    if (s != 0 && s != 1) transformValues(_ % s);
////
////  //
////  // Updates from another DomainMap.
////  //
////
////  /** Multiplies each value in this map by the corresponding value in the other map. */
////  def :*= (t : DomainMap[A,Double,D]) {
////    checkDomain(t.domain);
////    transform((k,v) => v * t(k));
////  }
////
////  /** Divides each value in this map by the corresponding value in the other map. */
////  def :/= (t : DomainMap[A,Double,D]) {
////    checkDomain(t.domain);
////    transform((k,v) => v / t(k));
////  }
////
////  /** Increments each value in this map by the corresponding value in the other map. */
////  def :+= (t : DomainMap[A,Double,D]) {
////    checkDomain(t.domain);
////    transform((k,v) => v + t(k));
////  }
////
////  /** Decrements each value in this map by the corresponding value in the other map. */
////  def :-= (t : DomainMap[A,Double,D]) {
////    checkDomain(t.domain);
////    transform((k,v) => v - t(k));
////  }
////
////  /** Raises each value in this map by the corresponding value in the other map. */
////  def :^= (t : DomainMap[A,Double,D]) {
////    checkDomain(t.domain);
////    transform((k,v) => math.pow(v, t(k)));
////  }
////
////  /** Modulos each value in this map by the corresponding value in the other map. */
////  def :%= (t : DomainMap[A,Double,D]) {
////    checkDomain(t.domain);
////    transform((k,v) => v % t(k));
////  }
////
////  /** += with another PartialMap is a fixed alias for :+= */
////  final def += (t : DomainMap[A,Double,D]) = this.:+=(t);
////
////  /** -= with another PartialMap is a fixed alias for :-= */
////  final def -= (t : DomainMap[A,Double,D]) = this.:-=(t);
////
////  /** Approximate equality at a given tolerance level. */
////  def =~= (tolerance : Double)(that : DomainMap[A,Double,D]) : Boolean = {
////    (this eq that) ||
////    (that canEqual this) &&
////    (this.domain == that.domain) &&
////    (domain.iterator.forall((k:A) => math.abs(this(k) - that(k)) < tolerance));
////  }
////
////  /** Approximate equality using the default Tensor.TOLERANCE value. */
////  def =~= (that : DomainMap[A,Double,D]) : Boolean =
////    this.=~=(Tensor.TOLERANCE)(that);
////
////  /** Returns !(this.=~=(tolerance)(that)) */
////  def !~= (tolerance : Double)(that : DomainMap[A,Double,D]) : Boolean =
////    ! this.=~=(tolerance)(that);
////
////  /** Returns !(this.=~=(that)) */
////  def !~= (that : DomainMap[A,Double,D]) : Boolean =
////    ! this.=~=(that);
//}
//
///**
// * A Tensor is a Double-valued MutableDomainMap.
// *
// * @author dramage
// */
//trait Tensor[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B]
//extends DomainMap[A,B]
//with MutableDomainMap[A,B] with TensorLike[A,B,IterableDomain[A],Tensor[A,B]];
//
//object Tensor {
//
//  type T[A,B] = Tensor[A,B];
//
////  class OpAdd[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B, @specialized(Int,Long,Float,Double) R]
////  (implicit op : CanAdd[A,B,R], scalar : Scalar[R])
////  extends CanAdd[T[A,B],T[A,C],T[A,R]] {
////    def apply(a : T[A,B], b : T[A,C]) = {
////      a.
////    }
////  }
//
//
//}
////  /** Default tolerance value for element-wise equality. */
////  val TOLERANCE = 1e-8;
//
////  /** A Tensor slice of a numeric MutableDomainMap. */
////  trait SliceLike
////  [@specialized(Int,Long) A1, D1<:IterableDomain[A1] with DomainLike[A1,D1],
////   @specialized(Int,Long) A2, D2<:IterableDomain[A2] with DomainLike[A2,D2],
////   +Coll <: MutableDomainMap[A1, Double, D1],
////   +This <: Slice[A1, D1, A2, D2, Coll]]
////  extends MutableDomainMapSliceLike[A1,D1,A2,D2,Double,Coll,This]
////  with Tensor1Like[A2,D2,This];
////
////  /** A Tensor slice of a numeric MutableDomainMap. */
////  trait Slice
////  [@specialized A1, D1<:IterableDomain[A1] with DomainLike[A1,D1],
////   @specialized A2, D2<:IterableDomain[A2] with DomainLike[A2,D2],
////   +Coll <: MutableDomainMap[A1, Double, D1]]
////  extends MutableDomainMapSlice[A1,D1,A2,D2,Double,Coll]
////  with Tensor1[A2,D2]
////  with SliceLike[A1,D1,A2,D2,Coll,Slice[A1,D1,A2,D2,Coll]];
//
////  implicit def canMapValues[@specialized A,D<:IterableDomain[A]] =
////  new DomainMapCanBuildFrom[DomainMap[A,B,D], A, O, D, MutableDomainMap[A,O,D]] {
////    override def apply(from : DomainMap[A,B,D], domain : D) =
////      DomainMapBuilder(MutableDomainMap[A,O,D](domain, default.value));
////  }
////
////  implicit def canSliceFrom[@specialized A1, @specialized A2, D<:IterableDomain[A1], @specialized B] =
////  new DomainMapCanSliceFrom[DomainMap[A1,B,D], A1, D, A2, B, DomainMap[A2,B,SetDomain[A2]]] {
////    override def apply(from : DomainMap[A1,B,D], keymap : scala.collection.Map[A2,A1]) =
////      new DomainMapSlice.FromKeyMap[A1, D, A2, B, DomainMap[A1,B,D]](from, keymap);
////  }
////
////  implicit def canSliceSeqFrom[@specialized A, D<:IterableDomain[A], @specialized B] =
////  new DomainMapCanSliceSeqFrom[DomainMap[A,B,D], A, D, B, DomainSeq[B]] {
////    override def apply(from : DomainMap[A,B,D], keys : Seq[A]) =
////      new DomainMapSliceSeq.FromKeySeq[A,D,B,DomainMap[A,B,D]](from, keys);
////  }
