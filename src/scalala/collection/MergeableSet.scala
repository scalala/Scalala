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
  
  /** Lazily computed size based on the number of elements returned by elements. */
  override def size = _size;
  
  /** Returns the Union of this set with another. */
  def ++(that : Set[I]) : UnionSet[I] =
    UnionSet(this, that);
  
  def --(that : Set[I]) : SubtractionSet[I] =
    SubtractionSet(this, that);
  
  override def **(that : Set[I]) : IntersectionSet[I] =
    IntersectionSet(this, that);
}

case class EmptySet[I]() extends MergeableSet[I] {
  override def contains(e : I) = false;
  
  override def elements = new Iterator[I] {
    override def hasNext = false;
    override def next = throw new IllegalAccessException;
  }
}

case class UnionSet[I](sets : Set[I]*) extends MergeableSet[I] {
  override def contains(x : I) : Boolean =
    sets.elements.map(_.contains(x)).contains(true);
  
  override def elements =
    for (i <- (0 until sets.size).elements;
         e <- sets(i).elements;
         if !(0 until i).map(j => sets(j).contains(e)).contains(true))
      yield e;
    
  override def ++(that : Set[I]) = that match {
    case UnionSet(those) => UnionSet((sets ++ those.asInstanceOf[Seq[Set[I]]]) : _*);
    case _ => UnionSet((sets ++ List(that)) :_*);
  }
}

case class IntersectionSet[I](sets : Set[I]*) extends MergeableSet[I] {
  lazy val smallest =
    sets.reduceLeft[Set[I]]((a : Set[I], b : Set[I]) => if (a.size < b.size) a else b);
  
  override def contains(x : I) : Boolean =
    sets.elements.forall(_.contains(x));
  
  override def elements =
    for (e <- smallest.elements; if contains(e)) yield e;
  
  override def **(that : Set[I]) =
    IntersectionSet((sets ++ List(that)) :_*);
}


case class SubtractionSet[I](included : Set[I], excluded : Set[I]) extends MergeableSet[I] {
  override def contains(x : I) : Boolean =
    included.contains(x) && !excluded.contains(x);
  
  override def elements =
    for (e <- included.elements; if !excluded.contains(e)) yield e;
  
  override def --(that : Set[I]) =
    SubtractionSet(included, UnionSet(excluded, that));
}

case class ProductSet[I,J](_1 : Set[I], _2 : Set[J]) extends MergeableSet[(I,J)] {
  override def size = _1.size * _2.size;
  override def contains(e : (I,J)) = _1.contains(e._1) && _2.contains(e._2);
  override def elements = for (e1 <- _1.elements; e2 <- _2.elements) yield (e1,e2);
}

case class IntSpanSet(start : Int, end : Int) extends MergeableSet[Int] {
  override def size = end - start;
  override def contains(i : Int) = i >= start && i < end;
  override def elements = (start until end).elements;
}
