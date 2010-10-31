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

/**
 * Vectors are Tensor1's on the non-negative integers.
 *
 * @author dramage
 */
trait VectorLike[@specialized(Int,Long,Float,Double) B, +This<:Vector[B]]
extends Tensor1Like[Int,B,IndexDomain,This] { self =>

  def size = domain.size;

  protected[this] def mkValueString(value : B) : String =
    value.toString;

  def toList =
    List.tabulate(size)(i => this(i));

  def toArray(implicit m : ClassManifest[B]) =
    Array.tabulate(size)(i => this(i));

  // TODO: improve this method to make it more Vector-like
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

object Vector extends VectorCompanion[Vector];


trait VectorCompanion[Bound[V] <: Vector[V]] extends IndexedTensorCompanion[Int,Bound];
