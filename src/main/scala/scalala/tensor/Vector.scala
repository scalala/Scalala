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

import scalala.generic.collection.CanFilterValues;

/**
 * Vectors are Tensor1's on the non-negative integers.
 *
 * @author dramage
 */
trait VectorLike[@specialized(Int,Long,Float,Double) B, +This<:Vector[B]]
extends Tensor1Like[Int,B,IndexDomain,This] { self =>

  //
  // for comprehensions
  //
  
  /**
   * For-comprension support for "for (v <- x) ...".
   */
  def foreach[U](f : B => U) : Unit =
    this.foreach((k,v) => f(v));
  
  def filter[TT>:This,That](f : B => Boolean)(implicit cf : CanFilterValues[TT,B,That]) =
    cf.filter(repr, f);

  protected[this] def mkValueString(value : B) : String =
    value.toString;

  /** Returns a view of this vector as a row. */
  def asRow : VectorRow[B] = this match {
    case r : VectorRow[_] => this.asInstanceOf[VectorRow[B]];
    case _ => new VectorRow.View(repr);
  }

  /** Returns a view of this vector as a column. */
  def asCol : VectorCol[B] = this match {
    case c : VectorCol[_] => this.asInstanceOf[VectorCol[B]];
    case _ => new VectorCol.View(repr);
  }

  /** Returns a copy of this vector's data as a list. */
  def toList =
    List.tabulate(size)(i => this(i));

  /** Returns a copy of this vector's data as a list. */
  def toArray(implicit m : ClassManifest[B]) =
    Array.tabulate(size)(i => this(i));

  override def toString = {
    val rv = valuesIterator.take(10).map(mkValueString).mkString("\n");
    if (size > 10) {
      rv + "\n" + "... ("+(size-10) +" more)";
    } else {
      rv;
    }
  }

  override protected def canEqual(other : Any) : Boolean = other match {
    case that : Vector[_] => true;
    case _ => false;
  }
}

/**
 * Vectors are Tensor1's on the non-negative integers.
 *
 * @author dramage
 */
trait Vector[@specialized(Int,Long,Float,Double) B]
extends Tensor1[Int,B]
with VectorLike[B,Vector[B]];

