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

import domain.{IterableDomain,Domain1,Domain2,IndexDomain,TableDomain,SetDomain,CanGetDomain};
import generic.collection._;

import mutable.TensorBuilder;

import scalala.operators._;
import scalala.scalar.Scalar;

/**
 * A Tensor is a map from keys K (with a domain) to numeric scalar values V.
 * More specific operations are available on tensors indexed by a single key
 * (Tensor1, Vector) or pair of keys (Tensor2, Matrix).
 *
 * Note that this trait does not support for comprehensions, although its
 * two sub-traits: TensorSeqLike and TensorMapLike do.  The difference is
 * the way in which the collection is viewed.  TensorSeqLike views the
 * collection as one containing values.  TensorMapLike views the collection
 * as one containing key value pairs.
 *
 * @author dramage
 */
trait TensorLike
[@specialized(Int, Long) K,
 @specialized(Int, Long, Float, Double, Boolean) V,
 +D<:IterableDomain[K],
 +This<:Tensor[K,V]]
extends DomainFunction[K, V, D]
with operators.NumericOps[This] {
self =>

  type Domain = D;

  protected type Self = This;

  /** Returns a pointer to this cast as an instance of This. */
  def repr : This = this.asInstanceOf[This];

  /** Provides information about the underlying scalar value type V. */
  implicit val scalar : Scalar[V];

  /**
   * Returns a builder for constructing new instances like this one,
   * on the given domain.
   */
  def newBuilder[NK,NV:Scalar](domain : IterableDomain[NK])
  : TensorBuilder[NK,NV,Tensor[NK,NV]] = domain match {
    case that : IndexDomain =>
      mutable.Vector(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : Domain1[_] =>
      mutable.Tensor1(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : TableDomain =>
      mutable.Matrix(that)(implicitly[Scalar[NV]]).asBuilder;
    case that : Domain2[_,_] =>
      mutable.Tensor2(that)(implicitly[Scalar[NV]]).asBuilder;
      // TODO: add this in when we have a mutable.TensorN
//    case that : ProductNDomain[_] =>
//      mutable.TensorN(that)(implicitly[Scalar[NV]]).asBuilder;
    case _ =>
      mutable.Tensor[NK,NV](domain).asBuilder;
  }

  //
  // Collection contents
  //

  /**
   * Applies the given function to each element of the domain and
   * its corresponding value.
   */
  def foreachPair[U](fn: (K,V) => U) : Unit =
    for (k <- domain) fn(k, apply(k));

  /**
   * Applies the given function to all elements of the domain
   * that have a non-zero values (and possibly some that have zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def foreachNonZeroPair[U](fn : ((K,V)=>U)) : Boolean = {
    this.foreachPair(fn);
    true;
  }

  /**
   * Applies the given function to each value in the map (one for
   * each element of the domain, including zeros).
   */
  def foreachValue[U](fn : (V=>U)) =
    for (k <- domain) fn(apply(k));

  /**
   * Applies the given function to every non-zero value in the map
   * (and possibly some zeros, too).
   *
   * @return true if all elements in the map were visited.
   */
  def foreachNonZeroValue[U](fn : (V=>U)) = {
    this.foreachValue(fn);
    true;
  }

  /** Returns true if and only if the given predicate is true for all elements. */
  def forall(fn : (K,V) => Boolean) : Boolean = {
    foreachPair((k,v) => if (!fn(k,v)) return false);
    return true;
  }

  /** Returns true if and only if the given predicate is true for all elements. */
  def forall(fn : V => Boolean) : Boolean = {
    foreachValue(v => if (!fn(v)) return false);
    return true;
  }

  /** Returns true if and only if the given predicate is true for all non-zero elements. */
  def forallNonZero(fn : (K,V) => Boolean) : Boolean = {
    foreachNonZeroPair((k,v) => if (!fn(k,v)) return false);
    return true;
  }
 
  /** Returns true if and only if the given predicate is true for all non-zero elements. */
  def forallNonZero(fn : V => Boolean) : Boolean = {
    foreachNonZeroValue(v => if (!fn(v)) return false);
    return true;
  }

  /** Creates a new map containing a transformed copy of this map. */
  def mapPairs[TT>:This,O,That](f : (K,V) => O)
  (implicit bf : CanMapKeyValuePairs[TT, K, V, O, That]) : That =
    bf.map(repr.asInstanceOf[TT], f);

  /** Maps all non-zero key-value pairs values. */
  def mapNonZeroPairs[TT>:This,O,That](f : (K,V) => O)
  (implicit bf : CanMapKeyValuePairs[TT, K, V, O, That]) : That =
    bf.mapNonZero(repr.asInstanceOf[TT], f);

  /** Creates a new map containing a transformed copy of this map. */
  def mapValues[TT>:This,O,That](f : V => O)
  (implicit bf : CanMapValues[TT, V, O, That]) : That =
    bf.map(repr.asInstanceOf[TT], f);

  /** Maps all non-zero values. */
  def mapNonZeroValues[TT>:This,O,That](f : V => O)
  (implicit bf : CanMapValues[TT, V, O, That]) : That =
    bf.mapNonZero(repr.asInstanceOf[TT], f);

  /** Iterates over all elements in the domain and the corresponding value. */
  def pairsIterator : Iterator[(K,V)] =
    domain.iterator.map(k => (k,this(k)));

  /** Iterates over the values in the tensor. */
  def valuesIterator : Iterator[V] =
    domain.iterator.map(this);

  /** Iterates over the keys in the tensor. */
  def keysIterator : Iterator[K] =
    domain.iterator;

  /** Returns some key for which the given predicate is true. */
  def find(p : V => Boolean) : Option[K] = {
    for (k <- domain) {
      if (p(apply(k))) return Some(k);
    }
    return None;
  }
    
  /** Returns the keys for which the given predicate is true. */
  def findAll(p : V => Boolean) : Iterable[K] =
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
  def join[TT>:This,V2,RV,That](tensor : Tensor[K,V2])(fn : (V,V2) => RV)
  (implicit bf : CanJoinValues[TT, Tensor[K,V2], V, V2, RV, That]) : That =
    bf.joinAll(repr.asInstanceOf[TT], tensor, fn);

  /**
   * Creates a new Tensor over the same domain using the given value
   * function to create each return value in the map where keys in
   * both this and m are non-zero.
   */
  def joinBothNonZero[TT>:This,V2,RV,That](tensor : Tensor[K,V2])(fn : (V,V2) => RV)
  (implicit bf : CanJoinValues[TT, Tensor[K,V2], V, V2, RV, That]) : That =
    bf.joinBothNonZero(repr.asInstanceOf[TT], tensor, fn);

  /**
   * Creates a new Tensor over the same domain using the given value
   * function to create each return value in the map where keys in
   * either this or m are non-zero.
   */
  def joinEitherNonZero[TT>:This,V2,RV,That](tensor : Tensor[K,V2])(fn : (V,V2) => RV)
  (implicit bf : CanJoinValues[TT, Tensor[K,V2], V, V2, RV, That]) : That =
    bf.joinEitherNonZero(repr.asInstanceOf[TT], tensor, fn);

  //
  // Slice construction
  //

  /** The value at the given key.  Takes precedence over apply(keys : K*). */
  def apply(key : K) : V;

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : K*)
  (implicit bf : CanSliceVector[This, K, That]) : That =
    bf(repr, keys);

  /** Creates a view backed by the "true" elements in selected. */
  def apply[That](selected : Tensor[K,Boolean])
  (implicit bf : CanSliceVector[This, K, That]) : That =
    bf(repr, domain.filter(selected).toIndexedSeq);

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : Traversable[K])
  (implicit bf : CanSliceVector[This, K, That]) : That =
    bf(repr, keys.toIndexedSeq);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : (I,K)*)
  (implicit bf : CanSliceTensor[This, K, I, That]) : That =
    apply(keys.toMap);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : Iterable[(I,K)])
  (implicit bf : CanSliceTensor[This, K, I, That]) : That =
    apply(keys.toMap);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : scala.collection.Map[I,K])
  (implicit bf : CanSliceTensor[This, K, I, That]) : That =
    bf(repr, keys);

  //
  // Sorting
  //

  /**
   * Returns the elements of this.domain ordered by their values in this map.
   * Currently this method is not particularly efficient, as it creates several
   * in-memory arrays the size of the domain.
   */
  def argsort(implicit cm : Manifest[K], ord : Ordering[V]) : Array[K] =
    domain.toArray(cm).sortWith((i:K, j:K) => ord.lt(this(i), this(j)));

  /**
   * Returns a sorted view of the current map.  Equivalent to calling
   * <code>x(x.argsort)</code>.  Changes to the sorted view are
   * written-through to the underlying map.
   */
  def sorted[That](implicit bf : CanSliceVector[This, K, That], cm : Manifest[K], ord : Ordering[V]) : That =
    this.apply(this.argsort);


  //
  // Collection level queries
  //

  /** Returns a key associated with the largest value in the map. */
  def argmax : K = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .max");
    }
    var max = valuesIterator.next;
    var arg = domain.iterator.next
    foreachPair((k,v) => if (scalar.>(v, max)) { max = v; arg = k; });
    arg;
  }

  /** Returns a key associated with the smallest value in the map. */
  def argmin : K = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .min");
    }
    var min = valuesIterator.next;
    var arg = domain.iterator.next
    foreachPair((k,v) => if (scalar.<(v,min)) { min = v; arg = k; });
    arg;
  }

  /** Returns the max of the values in this map. */
  def max : V = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .max");
    }
    var max = valuesIterator.next;
    if (foreachNonZeroValue(v => { max = scalar.max(max,v) })) {
      return max;
    } else {
      return scalar.max(max, scalar.zero);
    }
  }

  /** Returns the min of the values in this map. */
  def min : V = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .min");
    }
    var min = valuesIterator.next;
    if (foreachNonZeroValue(v => { min = scalar.min(min,v); })) {
      return min;
    } else {
      return scalar.min(min, scalar.zero)
    }
  }

  /** Returns the sum of the values in this map. */
  def sum : V = {
    var sum = scalar.zero;
    foreachNonZeroValue(v => sum = scalar.+(sum,v));
    return sum;
  }

  //
  // Conversions
  //

  /** Returns an ordering over the domain based on the values in this map. */
  def asOrdering(implicit ord : Ordering[V]) : Ordering[K] = new Ordering[K] {
    override def compare(a : K, b : K) = ord.compare(self(a), self(b));
  }

  /** Returns an unmodifiable Map-like view of this Tensor. */
  def asMap : scala.collection.Map[K,V] = new scala.collection.Map[K,V] {
    override def keysIterator = self.keysIterator;
    override def valuesIterator = self.valuesIterator;
    override def contains(key : K) = self.isDefinedAt(key);
    override def apply(key : K) = self.apply(key);
    override def iterator = self.pairsIterator;
    override def get(key : K) =
      if (self.isDefinedAt(key)) Some(self.apply(key)) else None;
    override def - (key : K) =
      throw new UnsupportedOperationException("asMap view of Tensor is unmodifiable: use toMap");
    override def + [V1>:V](kv : (K,V1)): scala.collection.Map[K,V1] =
      throw new UnsupportedOperationException("asMap view of Tensor is unmodifiable: use toMap");
  }

  /** Creates a new copy of this Tensor as a scala map. */
  def toMap : Map[K,V] =
    this.pairsIterator.toMap;

  protected[this] def mkKeyString(key : K) : String =
    key.toString;

  protected[this] def mkValueString(value : V) : String =
    value.toString;

  override def toString : String = {
    val iter = keysIterator;
    val keys = iter.take(10).toList;
    
    if (keys.isEmpty) 
      return "";
    
    val newline = System.getProperty("line.separator");
    val keyWidth = keys.iterator.map(mkKeyString).map(_.length).max+1;
    val rv = (for (key <- keys.iterator) yield {
      val ks = mkKeyString(key);
      ks + (" " * (keyWidth-ks.length)) + mkValueString(apply(key));
    }).mkString(newline);
    
    if (iter.hasNext) {
      rv + newline + "... ("+(domain.size-10) +" more)";
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
    case that: Tensor[_,_] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.domain == that.domain) &&
      ({ val casted = that.asInstanceOf[Tensor[K,V]];
         this.forall((k,v) => casted(k) == v) });
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
 * K Tensor is a map from keys K (with a domain) to numeric scalar values V.
 * More specific operations are available on tensors indexed by a single key
 * (Tensor1, Vector) or pair of keys (Tensor2, Matrix).
 *
 * @author dramage
 */
trait Tensor
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V]
extends TensorLike[K, V, IterableDomain[K], Tensor[K, V]];

object Tensor {

  class Impl[K,V](values : Map[K,V])(implicit override val scalar : Scalar[V])
  extends Tensor[K,V] {
    override val domain : SetDomain[K] = new SetDomain(values.keySet);
    override def apply(k : K) = values(k);
  }

  def apply[K,V:Scalar](values : (K,V)*) : Tensor[K,V] =
    new Impl(values.toMap);

  implicit def canView[K, V:Scalar] =
  new CanView[Tensor[K,V],TensorView.IdentityView[K,V,Tensor[K,V]]] {
    override def apply(from : Tensor[K,V]) =
      new TensorView.IdentityViewImpl[K,V,Tensor[K,V]](from);
  }

  implicit def canSliceTensor[K1, K2, V:Scalar] =
  new CanSliceTensor[Tensor[K1,V], K1, K2, Tensor[K2,V]] {
    override def apply(from : Tensor[K1,V], keymap : scala.collection.Map[K2,K1]) =
      new TensorSlice.FromKeyMap[K1, K2, V, Tensor[K1,V]](from, keymap);
  }

  implicit def canSliceVector[K, V:Scalar] =
  new CanSliceVector[Tensor[K,V], K, Vector[V]] {
    override def apply(from : Tensor[K,V], keys : Seq[K]) =
      new VectorSlice.FromKeySeq[K,V,Tensor[K,V]](from, keys);
  }

  implicit def canMapValues[K, V, RV, This, D, That]
  (implicit view : This=>Tensor[K,V], d : CanGetDomain[This,D],
   bf : CanBuildTensorFrom[This, D, K, RV, That],
   s : Scalar[RV])
  : CanMapValues[This,V,RV,That]
  = new CanMapValues[This,V,RV,That] {
    override def map(from : This, fn : (V=>RV)) = {
      val builder = bf(from, from.domain.asInstanceOf[D]);
      from.foreachPair((k,v) => builder(k) = fn(v));
      builder.result;
    }
    override def mapNonZero(from : This, fn : (V=>RV)) = {
      val builder = bf(from, from.domain.asInstanceOf[D]);
      from.foreachNonZeroPair((k,v) => builder(k) = fn(v));
      builder.result;
    }
  }

  implicit def canMapKeyValuePairs[K, V, RV, This, D, That]
  (implicit view : This=>Tensor[K,V], d : CanGetDomain[This,D],
   bf : CanBuildTensorFrom[This, D, K, RV, That],
   s : Scalar[RV])
  : CanMapKeyValuePairs[This,K,V,RV,That]
  = new CanMapKeyValuePairs[This,K,V,RV,That] {
    override def map(from : This, fn : ((K,V)=>RV)) = {
      val builder = bf(from, from.domain.asInstanceOf[D]);
      from.foreachPair((k,v) => builder(k) = fn(k,v));
      builder.result;
    }
    override def mapNonZero(from : This, fn : ((K,V)=>RV)) = {
      val builder = bf(from, from.domain.asInstanceOf[D]);
      from.foreachNonZeroPair((k,v) => builder(k) = fn(k,v));
      builder.result;
    }
  }

  implicit def canJoin[K, V1, V2, RV, This, D, That]
  (implicit view : This=>Tensor[K,V1], d : CanGetDomain[This,D],
   bf : CanBuildTensorFrom[This, D, K, RV, That],
   s : Scalar[RV])
  : CanJoinValues[This, Tensor[K,V2], V1, V2, RV, That] =
  new CanJoinValues[This, Tensor[K,V2], V1, V2, RV, That] {
    override def joinAll(a : This, b : Tensor[K,V2], fn : (V1,V2)=>RV) = {
      a.checkDomain(b.domain);
      val builder = bf(a, (a.domain union b.domain).asInstanceOf[D]);
      a.foreachPair((k,aV) => builder(k) = fn(aV,b(k)));
      builder.result;
    }

    override def joinEitherNonZero(a : This, b : Tensor[K,V2], fn : (V1,V2)=>RV) = {
      a.checkDomain(b.domain);
      val builder = bf(a, (a.domain union b.domain).asInstanceOf[D]);
      a.foreachNonZeroPair((k,aV) => builder(k) = fn(aV,b(k)));
      b.foreachNonZeroPair((k,bV) => builder(k) = fn(a(k),bV));
      builder.result;
    }

    override def joinBothNonZero(a : This, b : Tensor[K,V2], fn : (V1,V2)=>RV) = {
      a.checkDomain(b.domain);
      val builder = bf(a, (a.domain union b.domain).asInstanceOf[D]);
      a.foreachNonZeroPair((k,aV) => builder(k) = fn(aV,b(k)));
      builder.result;
    }
  }

//  implicit def opTensorUnary[K,V,RV,Op<:OpType,This,That]
//  (implicit view : This=>Tensor[K,V],
//   op : UnaryOp[V,Op,RV],
//   bf : CanMapValues[This,V,RV,That])
//  : UnaryOp[This,Op,That]
//  = new UnaryOp[This,Op,That] {
//    override def apply(from : This) =
//      bf.map(from, op);
//  }

  implicit def opTensorTensor[K,V1,V2,Op<:OpType,RV,This,That]
  (implicit view : This=>Tensor[K,V1],
   op : BinaryOp[V1,V2,Op,RV],
   bf : CanJoinValues[This,Tensor[K,V2],V1,V2,RV,That])
  : BinaryOp[This,Tensor[K,V2],Op,That]
  = new BinaryOp[This,Tensor[K,V2],Op,That] {
    override def opType = op.opType;
    override def apply(a : This, b : Tensor[K,V2]) = {
      if (op == OpMul || op == OpAdd || op == OpSub) {
        bf.joinEitherNonZero(a, b, op)
      } else {
        bf.joinAll(a, b, op);
      }
    }
  }

  implicit def opTensorScalar[K,V1,V2,Op<:OpType,RV,This,That]
  (implicit view : This=>Tensor[K,V1],
   op : BinaryOp[V1,V2,Op,RV],
   bf : CanMapValues[This,V1,RV,That],
   s : Scalar[V2])
  : BinaryOp[This,V2,Op,That]
  = new BinaryOp[This,V2,Op,That] {
    override def opType = op.opType;
    override def apply(a : This, b : V2) = {
      if (op == OpMul && !s.isNaN(b)) {
        bf.mapNonZero(a, v => op(v, b));
      } else {
        bf.map(a, v => op(v, b));
      }
    }
  }

  implicit def opScalarTensor[K,V1,V2,Op<:OpType,RV,This,That]
  (implicit view : This=>Tensor[K,V2],
   op : BinaryOp[V1,V2,Op,RV],
   bf : CanMapValues[This,V2,RV,That],
   s : Scalar[V1])
  : BinaryOp[V1,This,Op,That]
  = new BinaryOp[V1,This,Op,That] {
    override def opType = op.opType;
    override def apply(a : V1, b : This) = {
      if (op == OpMul && !s.isNaN(a)) {
        bf.mapNonZero(b, v => op(a, v));
      } else {
        bf.map(b, v => op(a, v));
      }
    }
  }
}

