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
package collection;

import domain._;
import generic._;

/**
 * A DomainMap is a DomainFunction backed by an IterableDomain, i.e. it is
 * a function for which its domain can be enumerated, so that we can support
 * map-like operations of forach, mapValues, etc.
 *
 * @author dramage
 */
trait DomainMapLike[@specialized A, @specialized B, D<:IterableDomain[A], +Repr<:DomainMap[A,B,D]]
extends DomainFunction[A, B, D] {
self =>

  protected type Self = Repr;

  def repr : Repr = this.asInstanceOf[Repr];

  /**
   * Creates a shallow copy of this domain map.  Inner value types are not copied,
   * so this method is safest when both keys and values are immutable.
   */
  def copy : Repr;

  /**
   * Applies the given function to each element of the domain and
   * its corresponding value (unboxed version).
   */
  def foreach[U](f: (A,B) => U) : Unit =
    for (k <- domain) f(k, apply(k));

//  //
//  // for-comprehensions
//  //
//
//  /**
//   * Applies the given function to each element of the domain and
//   * its corresponding value (boxed version used by "for").
//   */
//  def foreach[U](f: ((A,B)) => U) : Unit =
//    foreach((k,v) => f((k,v)));
//
//  /**
//   * For-comprehension support for "for ((k,v) <- x) yield ..."
//   */
//  def map[O, That](f: ((A,B)) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
//    val b = bf(repr);
//    for (x <- this) b += f(x);
//    b.result;
//  }
//
//  /**
//   * For-comprehension support for "for ((k,v) <- x; a <- v) yield ..."
//   */
//  def flatMap[O, TLike, That](f: ((A,B)) => TLike)
//  (implicit view: (TLike => Traversable[O]), bf: CanBuildFrom[Repr, O, That]): That = {
//    val b = bf(repr);
//    for (x <- this) b ++= view(f(x));
//    b.result;
//  }
//
//  /**
//   * For-comprehension support.
//   */
//  def filter[That](p: ((A,B)) => Boolean)(implicit bf: CanBuildFrom[Repr, (A,B), That]): That = {
//    val b = bf(repr);
//    for (x <- this) if (p(x)) b += x;
//    b.result;
//  }

  //
  // Collection contents
  //

  /**
   * Iterates over all iterator in the domain, whether or not the function
   * overrides the default value at that position.
   */
  def iterator : Iterator[(A,B)] =
    domain.iterator.map(k => (k,this(k)));

  /** @return the keys for which all possibly non-default values are defined. */
  def keysIterator : Iterator[A] =
    domain.iterator;

  /** @return the values in this map in some order. */
  def valuesIterator : Iterator[B] =
    domain.iterator.map(this);

  /** Returns the keys for which the value returns true. */
  def find(p : B => Boolean) : Iterable[A] =
    domain.filter(this andThen p);

//  def zip[I,O](other : DomainMap[A,I,D], compose : (B,I) => O = ((q:B,r:I)=>(q,r))) : DomainMap[A,O,D] = {
//    null;
//  }

//  /**
//   * Constructs a view of this map on which calls to mapValues are
//   * chained together and lazily evaluated.
//   */
//  def view : DomainMapView[A,B,D,Repr] =
//    new DomainMapView.IdentityViewImpl[A,B,D,Repr](repr);
  
  /**
   * Constructs a view of this map on which calls to mapValues are
   * chained together and lazily evaluated.
   */
  def view[That](implicit bf : DomainMapCanViewFrom[Repr,That]) : That =
    bf.apply(repr);

  /**
   * Returns a slice sequence of this DomainMap containing only those
   * values matching the given filter criteria.
   */
  def filterValues[That](p: B => Boolean)
  (implicit bf : DomainMapCanSliceSeqFrom[Repr, A, D, B, That]) : That =
    apply(find(p));

  /** Creates a new map containing a transformed view of this map. */
  def mapValues[O,That](f : B => O)
  (implicit bf : DomainMapCanMapValuesFrom[Repr, A, B, O, D, That]) : That =
    bf.apply(repr, f);

  //
  // Slice construction
  //

  /** The value at the given key.  Takes precedence over apply(keys : A*). */
  def apply(key : A) : B;

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : A*)
  (implicit bf : DomainMapCanSliceSeqFrom[Repr, A, D, B, That]) : That =
    bf(repr, keys);

  /** Creates a view backed by the given keys, returning them as a sequence. */
  def apply[That](keys : Iterable[A])
  (implicit bf : DomainMapCanSliceSeqFrom[Repr, A, D, B, That]) : That =
    bf(repr, keys.toIndexedSeq);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : (I,A)*)
  (implicit bf : DomainMapCanSliceFrom[Repr, A, D, I, B, That]) : That =
    apply[I,That](Map() ++ keys);

  /** Creates a view for the given elements with new indexes I, backed by this map. */
  def apply[I,That](keys : Iterable[(I,A)])
  (implicit bf : DomainMapCanSliceFrom[Repr, A, D, I, B, That]) : That =
    apply[I,That](Map() ++ keys);

  def apply[I,That](keys : scala.collection.Map[I,A])
  (implicit bf : DomainMapCanSliceFrom[Repr, A, D, I, B, That]) : That =
    bf(repr, keys);


  //
  // Equality
  //

  /**
   * Default implementation iterates the full domain in order, checking
   * that each function returns the same value.
   */
  override def equals(other : Any) : Boolean = other match {
    case that: DomainMap[A,B,_] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.domain == that.domain) &&
      (domain.iterator.forall((k:A) => this(k) == that(k)))
    case _ => false;
  }

  /** From recipe in "Programming in Scala" section 28.4. */
  def canEqual(other : Any) : Boolean = other match {
    case that : DomainMap[_,_,_] => true;
    case _ => false;
  }

  override def hashCode() =
    domain.hashCode + valuesIterator.foldLeft(1)((hash,v) => 41 * hash + v.hashCode);

  //
  // Sorting
  //

  // TODO: scala bug prevents this from being called.
//  /**
//   * Returns a sorted view of the current map.  Equivalent to calling
//   * <code>x(x.argsort)</code>.  Changes to the sorted view are written-through
//   * to the underlying map.
//   */
//  def sorted[That]
//  (implicit bf : DomainMapCanSliceSeqFrom[Repr, A, D, B, That], cm : ClassManifest[A], ord : Ordering[B]) : That =
//    this(this.argsort);

  /**
   * Returns the elements of this.domain ordered by their values in this map.
   * Currently this method is not particularly efficient, as it creates several
   * in-memory arrays the size of the domain.
   *
   * TODO: a faster implementation based on Sorting.quickSort using the
   * new versions of scala 2.8.0.
   */
  def argsort(implicit cm : ClassManifest[A], ord : Ordering[B]) : Array[A] = {
    domain.toArray.sortWith((i:A, j:A) => ord.lt(this(i), this(j)));
//    val index = domain.toArray;
//    scala.util.Sorting.quickSort(index)(this.asOrdering);
//    return index;
  }

  //
  // Conversions
  //

  /** Returns an ordering over the domain based on the values in this map. */
  def asOrdering(implicit ord : Ordering[B]) : Ordering[A] = {
    val map = this;
    new Ordering[A] {
      override def compare(a : A, b : A) = ord.compare(map(a), map(b));
    }
  }

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
}

trait DomainMap[@specialized A, @specialized B, D <: IterableDomain[A]]
extends DomainMapLike[A, B, D, DomainMap[A, B, D]];

//object DomainMapTypes {
//   type From[A,B] = DomainMap[A,B,IterableDomain[A]];
//   type ToMutable[A,B] = MutableDomainMap[A,B,IterableDomain[A]];
//   type ToSlice[A1,A2,B] = DomainMapSlice[A1,IterableDomain[A1],A2,SetDomain[A2],B,From[A1,B]];
//   type ToSliceSeq[A,B] = DomainMapSliceSeq[A,IterableDomain[A],B,From[A,B]];
//}
//
//object DomainMap extends DomainMapCompanion[IterableDomain, DomainMapTypes.From, DomainMapTypes.ToMutable, DomainMapTypes.ToSlice, DomainMapTypes.ToSliceSeq] {
//
//  override protected def createMutable[A,B](domain : IterableDomain[A], initial : B) =
//    MutableDomainMap[A,B,IterableDomain[A]](domain, initial);
//
//  override protected def createSlice[A1,A2,B](from : DomainMap[A1,B,IterableDomain[A1]], keymap : scala.collection.Map[A2,A1]) : DomainMapSlice[A1,IterableDomain[A1],A2,SetDomain[A2],B,DomainMap[A1,IterableDomain[A1],B]] =
//    new DomainMapSlice.FromKeyMap[A1,IterableDomain[A1],A2,B,DomainMap[A1,B,IterableDomain[A1]]](from, keymap);
//
//  override protected def createSliceSeq[A,B](from : From[A,B], keys : Seq[A]) : ToSliceSeq[A,B];
//
//}

object DomainMap {

  implicit def canViewFrom[A, B, D<:IterableDomain[A]] =
  new DomainMapCanViewFrom[DomainMap[A,B,D],DomainMapView.IdentityView[A,B,D,DomainMap[A,B,D]]] {
    override def apply(from : DomainMap[A,B,D]) =
      new DomainMapView.IdentityViewImpl[A,B,D,DomainMap[A,B,D]](from);
  }

  implicit def canMapValuesFrom[@specialized A,@specialized B,@specialized O,D<:IterableDomain[A]]
  (implicit default : DefaultValue[O]) =
  new DomainMapCanMapValuesFrom[DomainMap[A,B,D],A,B,O,D,MutableDomainMap[A,O,D]] {
    override def apply(from : DomainMap[A,B,D], fn : (B=>O)) = {
      val rv = MutableDomainMap[A,O,D](from.domain, default.value);
      from.foreach((k,v) => rv(k) = fn(v));
      rv;
    }
  }

  implicit def canSliceFrom[@specialized A1, @specialized A2, D<:IterableDomain[A1], @specialized B] =
  new DomainMapCanSliceFrom[DomainMap[A1,B,D], A1, D, A2, B, DomainMap[A2,B,SetDomain[A2]]] {
    override def apply(from : DomainMap[A1,B,D], keymap : scala.collection.Map[A2,A1]) =
      new DomainMapSlice.FromKeyMap[A1, D, A2, B, DomainMap[A1,B,D]](from, keymap);
  }

  implicit def canSliceSeqFrom[@specialized A, D<:IterableDomain[A], @specialized B] =
  new DomainMapCanSliceSeqFrom[DomainMap[A,B,D], A, D, B, DomainSeq[B]] {
    override def apply(from : DomainMap[A,B,D], keys : Seq[A]) =
      new DomainMapSliceSeq.FromKeySeq[A,D,B,DomainMap[A,B,D]](from, keys);
  }

}
