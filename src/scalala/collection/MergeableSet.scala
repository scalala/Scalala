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
package scalala.collection;

import scala.collection.Set;

/**
 * A set that can be unioned, subtracted, or intersected with another.
 * 
 * @author dramage
 */
trait MergeableSet[I] extends Set[I] {
  private lazy val _size = {
    var s = 0;
    for (e <- elements) { s += 1; }
    s;
  }
  
  /**
   * The default implementation lazily evaluates the size 
   * by counting the number of elements in the set.
   */
  override def size = _size;
  
  /** Returns the Union of this set with another. */
  def ++(that : Set[I]) : MergeableSet[I] =
    UnionSet(this, that);
  
  /** Returns this set except those elements in the other set. */
  def --(that : Set[I]) : MergeableSet[I] =
    SubtractionSet(this, that);
  
  /** Returns the intersection of this set with that one. */
  override def **(that : Set[I]) : MergeableSet[I] =
    IntersectionSet(this, that);
}

/** Default implementations based on wrapping scala objects. */
object MergeableSet {
  def apply[I](set : Set[I]) = new MergeableSet[I] {
    override def size = set.size;
    override def elements = set.elements;
    override def contains(i : I) = set.contains(i);
  }
  
  def apply[I](collection : Collection[I]) = new MergeableSet[I] {
    override def size = collection.size;
    override def elements = collection.elements;
    override def contains(i:I) = !elements.forall(_ != i);
  }
  
  def apply[I](iterable : Iterable[I]) = new MergeableSet[I] {
    override def elements = iterable.elements;
    override def contains(i:I) = !elements.forall(_ != i);
  }
}

/**
 * An empty mergeable set.
 * @author dramage
 */
case class EmptySet[I]() extends MergeableSet[I] {
  override def contains(e : I) = false;
  
  override def elements = new Iterator[I] {
    override def hasNext = false;
    override def next = throw new IllegalAccessException;
  }
  
  override def **(that : Set[I]) = this;
  
  override def --(that : Set[I]) = this;
  
  override def ++(that : Set[I]) = that match {
    case other : MergeableSet[_] => other;
    case _ => super.++(that);
  }
}

/**
 * The union of elements in the given sets.  The provided
 * elements iterator is careful not repeat items contained
 * in more than one set.
 * 
 * @author dramage
 */
case class UnionSet[I](sets : Set[I]*) extends MergeableSet[I] {
  override def contains(x : I) : Boolean =
    sets.elements.map(_.contains(x)).contains(true);
  
  override def elements =
    for (i <- (0 until sets.size).elements;
         e <- sets(i).elements;
         if (sets.take(i).forall(!_.contains(e))))
      yield e;
    
  override def ++(that : Set[I]) = that match {
    case UnionSet(those) => UnionSet((sets ++ those.asInstanceOf[Seq[Set[I]]]) : _*);
    case _ => UnionSet((sets ++ List(that)) :_*);
  }
}

/**
 * The intersection of elements in the given sets.  The provided
 * elements iterator iterates the shortest set, discarding elements
 * not found in the others.
 * 
 * @author dramage
 */
case class IntersectionSet[I](sets : Set[I]*) extends MergeableSet[I] {
  if (sets.size == 0) throw new IllegalArgumentException();
  
  lazy val smallest =
    sets.reduceLeft[Set[I]]((a : Set[I], b : Set[I]) => if (a.size < b.size) a else b);
  
  override def contains(x : I) : Boolean =
    sets.elements.forall(_.contains(x));
  
  override def elements =
    for (e <- smallest.elements; if contains(e)) yield e;
  
  override def **(that : Set[I]) = that match {
    case IntersectionSet(those) => IntersectionSet((sets ++ those.asInstanceOf[Seq[Set[I]]]) : _*);
    case _ => IntersectionSet((sets ++ List(that)) :_*);
  }
}

/**
 * The elements of included except not the elements of excluded.
 * 
 * @author dramage
 */
case class SubtractionSet[I](included : Set[I], excluded : Set[I]) extends MergeableSet[I] {
  override def contains(x : I) : Boolean =
    included.contains(x) && !excluded.contains(x);
  
  override def elements =
    for (e <- included.elements; if !excluded.contains(e)) yield e;
  
  override def --(that : Set[I]) =
    SubtractionSet(included, UnionSet(excluded, that));
}

/**
 * The cross product of two sets useful.
 * 
 * @author dramage
 */
case class ProductSet[I,J](_1 : Set[I], _2 : Set[J]) extends MergeableSet[(I,J)] {
  override def size = _1.size * _2.size;
  override def contains(e : (I,J)) = _1.contains(e._1) && _2.contains(e._2);
  override def elements = for (e1 <- _1.elements; e2 <- _2.elements) yield (e1,e2);
}

/**
 * The set of made up of continuous integers within a span.
 * 
 * @author dramage
 */
case class IntSpanSet(start : Int, end : Int) extends MergeableSet[Int] {
  override def size = end - start;
  override def contains(i : Int) = i >= start && i < end;
  override def elements = (start until end).elements;
  
  override def ++(other : Set[Int]) = other match {
    case that : IntSpanSet if (that.start <= this.end || this.start <= that.end) =>
      IntSpanSet(Math.min(this.start, that.start), Math.max(this.end, that.end));
    case _ => super.++(other);
  }
  
  override def **(other : Set[Int]) = other match {
    case that : IntSpanSet if (that.start <= this.end || this.start <= that.end) =>
      val newStart = Math.max(this.start, that.start);
      val newEnd = Math.min(this.end, that.end);
      if (newStart >= newEnd) EmptySet[Int]() else IntSpanSet(newStart, newEnd);
    case _ => super.**(other);
  }
}

/**
 * Tests of the MergeableSet architecture.
 * 
 * @uathor dramage
 */
trait MergeableSetTest {
  import scalala.ScalalaTest._;

  def _set_test() {
    assertEquals(IntSpanSet(-1,9), IntSpanSet(-1,3) ++ IntSpanSet(4,9));
    assertEquals(IntSpanSet(4,7), IntSpanSet(2,7) ** IntSpanSet(4,14));
    assertEquals(IntersectionSet(IntSpanSet(2,5),IntSpanSet(6,14)),
                 IntSpanSet(2,5) ** IntSpanSet(6,14));

    // check that we don't repeat elements in UnionSet's iterator
    assertEquals(List(0,1,2,3), UnionSet(IntSpanSet(0,3),IntSpanSet(2,4)).elements.toList);
  }
}
