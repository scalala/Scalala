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

import domain.IndexDomain;

import scalala.generic.collection._;

/**
 * Vectors are Tensor1s on the non-negative integers.
 *
 * @author dramage
 */
trait VectorLike[@specialized(Int,Long,Float,Double) V, +This<:Vector[V]]
extends Tensor1Like[Int,V,IndexDomain,This] { self =>

  override def foreachKey[U](fn : (Int => U)) =
    Range(0,size).foreach(fn);

  //
  // for comprehensions
  //
  
  /** Calls this.foreachValue(fn). */
  def foreach[U](fn : (V=>U)) =
    this.foreachValue(fn);
  
  /** Calls this.mapValues(fn). */
  def map[TT>:This,RV,That](fn : V => RV)(implicit bf : CanMapValues[TT, V, RV, That]) : That =
    this.mapValues[TT,RV,That](fn)(bf);

  def filter[TT>:This,That](f : V => Boolean)(implicit cf : CanBuildTensorFrom[TT,IndexDomain,Int,V,That]) =
    withFilter(f).strict;
  
  def withFilter(f : V => Boolean) =
    new Vector.Filtered[V,This](repr, f);

  /** Calls this.valuesIterator. */
  def iterator : Iterator[V] =
    this.valuesIterator;

  //
  // Views
  //

  /** Returns a view of this vector as a row. */
  def asRow : VectorRow[V] = this match {
    case r : VectorRow[_] => this.asInstanceOf[VectorRow[V]];
    case _ => new VectorRow.View(repr);
  }

  /** Returns a view of this vector as a column. */
  def asCol : VectorCol[V] = this match {
    case c : VectorCol[_] => this.asInstanceOf[VectorCol[V]];
    case _ => new VectorCol.View(repr);
  }

  /** Returns a copy of this vector's data as a list. */
  def toList =
    List.tabulate(size)(i => this(i));

  /** Returns a copy of this vector's data as a list. */
  def toArray(implicit m : ClassManifest[V]) =
    Array.tabulate(size)(i => this(i));

  override def toString = {
    val rv = valuesIterator.take(10).map(mkValueString).mkString("\n");
    if (size > 10) {
      rv + System.getProperty("line.separator") + "... ("+(size-10) +" more)";
    } else {
      rv;
    }
  }
}

/**
 * Vectors are Tensor1's on the non-negative integers.
 *
 * @author dramage
 */
trait Vector[@specialized(Int,Long,Float,Double) V]
extends Tensor1[Int,V]
with VectorLike[V,Vector[V]];


object Vector {
  class Filtered[@specialized(Int,Long,Float,Double) V, +This<:Vector[V]]
  (inner : This, filterFn : V => Boolean) {
    def size = {
      var rv = 0;
      inner.foreach(v => if (filterFn(v)) rv += 1);
      rv;
    }
    
    def withFilter(fn : V => Boolean) =
      new Filtered[V,This](inner, v => filterFn(v) && fn(v));
    
    def foreach[U](fn : V => U) = {
      for (v <- inner)
        if (filterFn(v)) fn(v);
    }
    
    def map[U,That](fn : V => U)
    (implicit bf : CanBuildTensorFrom[This,IndexDomain,Int,U,That]) = {
      val builder = bf(inner, IndexDomain(size));
      var i = 0;
      for (v <- inner) {
        if (filterFn(v)) {
          builder(i) = fn(v);
          i += 1;
        }
      }
      builder.result;
    }
    
//    def flatMap[U,That](fn : V => Traversable[U])
//    (implicit bf : CanBuildTensorFrom[This,IndexDomain,Int,U,That]) = {
//      val builder = bf(inner, IndexDomain(size));
//      var i = 0;
//      for (v <- inner) {
//        if (filterFn(v)) {
//          for (u <- fn(v)) {
//            builder(i) = u;
//            i += 1;
//          }
//        }
//      }
//      builder.result;
//    }
    
    def strict[That](implicit bf : CanBuildTensorFrom[This,IndexDomain,Int,V,That]) = {
      val builder = bf(inner, IndexDomain(size));
      var i = 0;
      for (v <- inner) {
        if (filterFn(v)) {
          builder(i) = v;
          i += 1;
        }
      }
      builder.result;
    }
  }
}

