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

import scalar.Scalar;

import domain._;
import generic.{CanAdd,CanMul,CanSub,CanDiv,CanMod,CanPow};
import generic.collection._;

import mutable.TensorBuilder;

/**
 * A Tensor is a map from keys A (with a domain) to numeric scalar values B.
 * More specific operations are available on tensors indexed by a single key
 * (Tensor1, Vector) or pair of keys (Tensor2, Matrix).
 *
 * @author dramage
 */
trait TensorLike
[@specialized(Int, Long) A,
 @specialized(Int, Long, Float, Double, Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D],
 +This<:Tensor[A,B]]
extends DomainFunction[A, B, D]
with operators.NumericOps[This] {
self =>

  protected type Self = This;

  def repr : This = this.asInstanceOf[This];

  /** Provides information about the underlying scalar value type B. */
  implicit val scalar : Scalar[B];

  /**
   * Returns a builder for constructing new instances like this one,
   * on the given domain.
   */
  def newBuilder[NK,NV:Scalar](domain : IterableDomain[NK])
  : TensorBuilder[NK,NV,Tensor[NK,NV]] = domain match {
    case that : IndexDomain =>
      mutable.Vector(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : Product1Domain[_] =>
      mutable.Tensor1(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : TableDomain =>
      mutable.Matrix(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : Product2Domain[_,_] =>
      mutable.Tensor2(that)(implicitly[Scalar[NV]]).asBuilder;
      // TODO: add this in when we have a mutable.TensorN
//    case that : ProductNDomain[_] =>
//      mutable.TensorN(that)(implicitly[Scalar[NV]]).asBuilder;
    case _ =>
      mutable.Tensor[NK,NV](domain).asBuilder;
  }

  //
  // for-comprehensions.
  //
  // Note:
  // 1. flatMap doesn't make sense here because the value types are
  //    always scalar, so you can't do { for ((k,v) <- tensor; value <- v) yield ... }.
  //    Although I guess you might want to iterate over they key types, k,
  //    but I'm not supporting that.
  //
  // 2. filter doesn't make sense because removing some elements from
  //    the domain is not well defined.
  //

  /**
   * For-comprension support for "for ((k,v) <- x) ...".
   * Defers to un-tupled alternate foreach.
   */
  def foreach[U](f: ((A,B)) => U) : Unit =
    this.foreach((k,v) => f((k,v)));

  /**
   * For-comprehension support for "for ((k,v) <- x) yield ..."
   * Defers to un-tupled map.
   */
  def map[TT>:This, O, That](f: ((A,B)) => O)
  (implicit map: CanMapKeyValuePairs[TT, A, B, O, That]): That =
    this.map[TT,O,That]((k : A, v : B) => f((k,v)));

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

  /** Creates a new map containing a transformed copy of this map. */
  def map[TT>:This,O,That](f : (A,B) => O)
  (implicit bf : CanMapKeyValuePairs[TT, A, B, O, That]) : That =
    bf.map(repr.asInstanceOf[TT], f);

  /** Maps all non-zero key-value pairs values. */
  def mapNonZero[TT>:This,O,That](f : (A,B) => O)
  (implicit bf : CanMapKeyValuePairs[TT, A, B, O, That]) : That =
    bf.mapNonZero(repr.asInstanceOf[TT], f);

  /** Creates a new map containing a transformed copy of this map. */
  def mapValues[TT>:This,O,That](f : B => O)
  (implicit bf : CanMapValues[TT, B, O, That]) : That =
    bf.map(repr.asInstanceOf[TT], f);

  def mapNonZeroValues[TT>:This,O,That](f : B => O)
  (implicit bf : CanMapValues[TT, B, O, That]) : That =
    bf.mapNonZero(repr.asInstanceOf[TT], f);

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
  def view[That](implicit bf : CanView[This,That]) : That =
    bf.apply(repr);


  /**
   * Creates a new Tensor over the same domain using the given value
   * function to create each return value in the map.
   */
  def join[TT>:This,V2,RV,That](tensor : Tensor[A,V2])(fn : (B,V2) => RV)
  (implicit bf : CanJoinValues[TT, Tensor[A,V2], B, V2, RV, That]) : That =
    bf.joinAll(repr.asInstanceOf[TT], tensor, fn);

  /**
   * Creates a new Tensor over the same domain using the given value
   * function to create each return value in the map where keys in
   * both this and m are non-zero.
   */
  def joinBothNonZero[TT>:This,V2,RV,That](tensor : Tensor[A,V2])(fn : (B,V2) => RV)
  (implicit bf : CanJoinValues[TT, Tensor[A,V2], B, V2, RV, That]) : That =
    bf.joinBothNonZero(repr.asInstanceOf[TT], tensor, fn);

  /**
   * Creates a new Tensor over the same domain using the given value
   * function to create each return value in the map where keys in
   * either this or m are non-zero.
   */
  def joinEitherNonZero[TT>:This,V2,RV,That](tensor : Tensor[A,V2])(fn : (B,V2) => RV)
  (implicit bf : CanJoinValues[TT, Tensor[A,V2], B, V2, RV, That]) : That =
    bf.joinEitherNonZero(repr.asInstanceOf[TT], tensor, fn);

  //
  // Slice construction
  //

  /** The value at the given key.  Takes precedence over apply(keys : A*). */
  def apply(key : A) : B;

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : A*)
  (implicit bf : CanSliceVector[This, A, That]) : That =
    bf(repr, keys);

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : Traversable[A])
  (implicit bf : CanSliceVector[This, A, That]) : That =
    bf(repr, keys.toIndexedSeq);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : (I,A)*)
  (implicit bf : CanSliceTensor[This, A, I, That]) : That =
    apply(keys.toMap);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : Iterable[(I,A)])
  (implicit bf : CanSliceTensor[This, A, I, That]) : That =
    apply(keys.toMap);

  def apply[I,That](keys : scala.collection.Map[I,A])
  (implicit bf : CanSliceTensor[This, A, I, That]) : That =
    bf(repr, keys);

  //
  // Sorting
  //

  /**
   * Returns the elements of this.domain ordered by their values in this map.
   * Currently this method is not particularly efficient, as it creates several
   * in-memory arrays the size of the domain.
   */
  def argsort(implicit cm : Manifest[A], ord : Ordering[B]) : Array[A] =
    domain.toArray(cm).sortWith((i:A, j:A) => ord.lt(this(i), this(j)));

  /**
   * Returns a sorted view of the current map.  Equivalent to calling
   * <code>x(x.argsort)</code>.  Changes to the sorted view are
   * written-through to the underlying map.
   */
  def sorted[That](implicit bf : CanSliceVector[This, A, That], cm : Manifest[A], ord : Ordering[B]) : That =
    this.apply(this.argsort);


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
      (this.foreachNonZero((k,v) => that(k) == v));
    case _ => false;
  }

  /** From recipe in "Programming in Scala" section 28.4. */
  protected def canEqual(other : Any) : Boolean = other match {
    case that : Tensor[_,_] => true;
    case _ => false;
  }

  override def hashCode() =
    domain.hashCode + valuesIterator.foldLeft(1)((hash,v) => 41 * hash + v.hashCode);
}

/**
 * A Tensor is a map from keys A (with a domain) to numeric scalar values B.
 * More specific operations are available on tensors indexed by a single key
 * (Tensor1, Vector) or pair of keys (Tensor2, Matrix).
 *
 * @author dramage
 */
trait Tensor
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends TensorLike[A, B, IterableDomain[A], Tensor[A, B]];

object Tensor extends TensorCompanion[Tensor] {

  // private type Bound[K,V] = Tensor[K,V];

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

/**
 * Default instances of all capability traits for tensors bounded by
 * Bound.  These implementations use map and join to construct new
 * return values.
 *
 * @author dramage
 */
trait TensorCompanion[Bound[K,V] <: Tensor[K,V]] {

  implicit def canMapValues[K, V, RV:Scalar]
  : CanMapValues[Bound[K,V],V,RV,Bound[K,RV]]
  = new CanMapValues[Bound[K,V],V,RV,Bound[K,RV]] {
    override def map(from : Bound[K,V], fn : (V=>RV)) = {
      val builder = from.newBuilder[K,RV](from.domain);
      from.foreach((k,v) => builder(k) = fn(v));
      builder.result.asInstanceOf[Bound[K,RV]];
    }
    override def mapNonZero(from : Bound[K,V], fn : (V=>RV)) = {
      val builder = from.newBuilder[K,RV](from.domain);
      from.foreachNonZero((k,v) => builder(k) = fn(v));
      builder.result.asInstanceOf[Bound[K,RV]];
    }
  }

  implicit def canMapKeyValuePairs[K, V, RV:Scalar]
  : CanMapKeyValuePairs[Bound[K,V],K,V,RV,Bound[K,RV]]
  = new CanMapKeyValuePairs[Bound[K,V],K,V,RV,Bound[K,RV]] {
    override def map(from : Bound[K,V], fn : ((K,V)=>RV)) = {
      val builder = from.newBuilder[K,RV](from.domain);
      from.foreach((k,v) => builder(k) = fn(k,v));
      builder.result.asInstanceOf[Bound[K,RV]];
    }
    override def mapNonZero(from : Bound[K,V], fn : ((K,V)=>RV)) = {
      val builder = from.newBuilder[K,RV](from.domain);
      from.foreachNonZero((k,v) => builder(k) = fn(k,v));
      builder.result.asInstanceOf[Bound[K,RV]];
    }
  }

  implicit def canJoin[K, V1, V2, RV:Scalar]
  : CanJoinValues[Bound[K,V1], Tensor[K,V2], V1, V2, RV, Bound[K,RV]] =
  new CanJoinValues[Bound[K,V1], Tensor[K,V2], V1, V2, RV, Bound[K,RV]] {
    override def joinAll(a : Bound[K,V1], b : Tensor[K,V2], fn : (V1,V2)=>RV) = {
      a.checkDomain(b.domain);
      val builder = a.newBuilder[K,RV](a.domain union b.domain);
      a.foreach((k,aV) => builder(k) = fn(aV,b(k)));
      builder.result.asInstanceOf[Bound[K,RV]];
    }

    override def joinEitherNonZero(a : Bound[K,V1], b : Tensor[K,V2], fn : (V1,V2)=>RV) = {
      a.checkDomain(b.domain);
      val builder = a.newBuilder[K,RV](a.domain union b.domain);
      a.foreachNonZero((k,aV) => builder(k) = fn(aV,b(k)));
      b.foreachNonZero((k,bV) => builder(k) = fn(a(k),bV));
      builder.result.asInstanceOf[Bound[K,RV]];
    }

    override def joinBothNonZero(a : Bound[K,V1], b : Tensor[K,V2], fn : (V1,V2)=>RV) = {
      a.checkDomain(b.domain);
      val builder = a.newBuilder[K,RV](a.domain union b.domain);
      a.foreachNonZero((k,aV) => builder(k) = fn(aV,b(k)));
      builder.result.asInstanceOf[Bound[K,RV]];
    }
  }

  //
  // Tensor-scalar
  //

  implicit def canAddScalar[K,V,O,RV,That](implicit op : CanAdd[V,O,RV], bf : CanMapValues[Bound[K,V],V,RV,That], so : Scalar[O])
  : CanAdd[Bound[K,V],O,That] = new CanAdd[Bound[K,V],O,That] {
    override def apply(a : Bound[K,V], b : O) = {
      a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapValues(v => op(v, b))(bf);
    }
  }

  implicit def canSubScalar[K,V,O,RV,That](implicit op : CanSub[V,O,RV], bf : CanMapValues[Bound[K,V],V,RV,That], so : Scalar[O])
  : CanSub[Bound[K,V],O,That] = new CanSub[Bound[K,V],O,That] {
    override def apply(a : Bound[K,V], b : O) = {
      a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapValues(v => op(v, b))(bf);
    }
  }

  implicit def canMulScalar[K,V,O,RV,That](implicit op : CanMul[V,O,RV], bf : CanMapValues[Bound[K,V],V,RV,That], sb : Scalar[O])
  : CanMul[Bound[K,V],O,That] = new CanMul[Bound[K,V],O,That] {
    override def apply(a : Bound[K,V], b : O) = {
      if (sb.isNaN(b)) {
        a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapValues(v => op(v, b))(bf);
      } else {
        a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapNonZeroValues(v => op(v, b))(bf);
      }
    }
  }

  implicit def canDivScalar[K,V,O,RV,That](implicit op : CanDiv[V,O,RV], bf : CanMapValues[Bound[K,V],V,RV,That], sb : Scalar[O])
  : CanDiv[Bound[K,V],O,That] = new CanDiv[Bound[K,V],O,That] {
    override def apply(a : Bound[K,V], b : O) = {
      if (b == sb.zero || sb.isNaN(b)) {
        a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapValues(v => op(v, b));
      } else {
        a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapNonZeroValues(v => op(v, b));
      }
    }
  }

  implicit def canPowScalar[K,V,O,RV,That](implicit op : CanPow[V,O,RV], bf : CanMapValues[Bound[K,V],V,RV,That], so : Scalar[O])
  : CanPow[Bound[K,V],O,That] = new CanPow[Bound[K,V],O,That] {
    override def apply(a : Bound[K,V], b : O) = {
      a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapValues(v => op(v, b))(bf);
    }
  }

  implicit def canModScalar[K,V,O,RV,That](implicit op : CanMod[V,O,RV], bf : CanMapValues[Bound[K,V],V,RV,That], so : Scalar[O])
  : CanMod[Bound[K,V],O,That] = new CanMod[Bound[K,V],O,That] {
    override def apply(a : Bound[K,V], b : O) = {
      a.asInstanceOf[TensorLike[K,V,_,Bound[K,V]]].mapValues(v => op(v, b))(bf);
    }
  }


  //
  // Tensor-Tensor
  //

  implicit def canAddBound[K,V,O,RV,That](implicit op : CanAdd[V,O,RV], bf : CanJoinValues[Bound[K,V],Bound[K,O],V,O,RV,That], so : Scalar[O])
  : CanAdd[Bound[K,V],Bound[K,O],That] = new CanAdd[Bound[K,V],Bound[K,O],That] {
    override def apply(a : Bound[K,V], b : Bound[K,O]) = {
      val aLike = a.asInstanceOf[Tensor[K,V]];
      val bLike = b.asInstanceOf[Tensor[K,O]];
      val bfLike = bf.asInstanceOf[CanJoinValues[Tensor[K,V],Tensor[K,O],V,O,RV,That]];
      (a joinEitherNonZero b)(op)(bfLike);
    }
  }

  implicit def canSubBound[K,V,O,RV,That](implicit op : CanSub[V,O,RV], bf : CanJoinValues[Bound[K,V],Bound[K,O],V,O,RV,That], so : Scalar[O])
  : CanSub[Bound[K,V],Bound[K,O],That] = new CanSub[Bound[K,V],Bound[K,O],That] {
    override def apply(a : Bound[K,V], b : Bound[K,O]) = {
      val aLike = a.asInstanceOf[Tensor[K,V]];
      val bLike = b.asInstanceOf[Tensor[K,O]];
      val bfLike = bf.asInstanceOf[CanJoinValues[Tensor[K,V],Tensor[K,O],V,O,RV,That]];
      (a joinEitherNonZero b)(op)(bfLike);
    }
  }

  // TODO: this could be faster via joinBothNonZero if we knew there were no NaN in b
  implicit def canMulBound[K,V,O,RV,That](implicit op : CanMul[V,O,RV], bf : CanJoinValues[Bound[K,V],Bound[K,O],V,O,RV,That], so : Scalar[O])
  : CanMul[Bound[K,V],Bound[K,O],That] = new CanMul[Bound[K,V],Bound[K,O],That] {
    override def apply(a : Bound[K,V], b : Bound[K,O]) = {
      val aLike = a.asInstanceOf[Tensor[K,V]];
      val bLike = b.asInstanceOf[Tensor[K,O]];
      val bfLike = bf.asInstanceOf[CanJoinValues[Tensor[K,V],Tensor[K,O],V,O,RV,That]];
      (a joinEitherNonZero b)(op)(bfLike);
    }
  }

  implicit def canDivBound[K,V,O,RV,That](implicit op : CanDiv[V,O,RV], bf : CanJoinValues[Bound[K,V],Bound[K,O],V,O,RV,That], so : Scalar[O])
  : CanDiv[Bound[K,V],Bound[K,O],That] = new CanDiv[Bound[K,V],Bound[K,O],That] {
    override def apply(a : Bound[K,V], b : Bound[K,O]) = {
      val aLike = a.asInstanceOf[Tensor[K,V]];
      val bLike = b.asInstanceOf[Tensor[K,O]];
      val bfLike = bf.asInstanceOf[CanJoinValues[Tensor[K,V],Tensor[K,O],V,O,RV,That]];
      (a join b)(op)(bfLike);
    }
  }

  implicit def canPowBound[K,V,O,RV,That](implicit op : CanPow[V,O,RV], bf : CanJoinValues[Bound[K,V],Bound[K,O],V,O,RV,That], so : Scalar[O])
  : CanPow[Bound[K,V],Bound[K,O],That] = new CanPow[Bound[K,V],Bound[K,O],That] {
    override def apply(a : Bound[K,V], b : Bound[K,O]) = {
      val aLike = a.asInstanceOf[Tensor[K,V]];
      val bLike = b.asInstanceOf[Tensor[K,O]];
      val bfLike = bf.asInstanceOf[CanJoinValues[Tensor[K,V],Tensor[K,O],V,O,RV,That]];
      (a join b)(op)(bfLike);
    }
  }

  implicit def canModBound[K,V,O,RV,That](implicit op : CanMod[V,O,RV], bf : CanJoinValues[Bound[K,V],Bound[K,O],V,O,RV,That], so : Scalar[O])
  : CanMod[Bound[K,V],Bound[K,O],That] = new CanMod[Bound[K,V],Bound[K,O],That] {
    override def apply(a : Bound[K,V], b : Bound[K,O]) = {
      val aLike = a.asInstanceOf[Tensor[K,V]];
      val bLike = b.asInstanceOf[Tensor[K,O]];
      val bfLike = bf.asInstanceOf[CanJoinValues[Tensor[K,V],Tensor[K,O],V,O,RV,That]];
      (a join b)(op)(bfLike);
    }
  }
}
