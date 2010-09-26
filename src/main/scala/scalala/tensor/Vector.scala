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
import generic._;
import mutable.TensorBuilder;

trait VectorLike[@specialized(Int,Long,Float,Double) B, +This<:Vector[B]]
extends Tensor1Like[Int,B,IndexDomain,This] { self =>

  def size = domain.size;

  override def newBuilder[C:Scalar] : TensorBuilder[Int,C,Vector[C]] =
  new TensorBuilder[Int,C,Vector[C]] {
    val rv = mutable.Vector[C](self.size);
    def update(k : Int, v : C) = rv(k) = v;
    def result = rv;
  }

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
      rv + "\n" + "...";
    } else {
      rv;
    }
  }
}

trait Vector[@specialized(Int,Long,Float,Double) B]
extends Tensor1[Int,B]
with VectorLike[B,Vector[B]];

object Vector {
//  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
//  trait SliceSeqLike
//  [@specialized(Int,Long) A,
//   @specialized(Int,Long,Float,Double) B,
//   D<:IterableDomain[A] with DomainLike[A,D],
//   +Coll<:MutableDomainMap[A,B],
//   +This <: SliceSeq[A, B, Coll]]
//  extends MutableDomainMapSliceSeqLike[A,D,B,Coll,This]
//  with VectorLike[B,This];
//
//  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
//  trait SliceSeq
//  [@specialized(Int,Long) A,
//   @specialized(Int,Long,Float,Double) B,
//   +Coll <: MutableDomainMap[A,B]]
//  extends MutableDomainMapSliceSeq[A,B,Coll]
//  with Vector[B] with SliceSeqLike[A,B,IterableDomain[A],Coll,SliceSeq[A,B,Coll]];
//
//  class SliceFromKeySeq
//  [@specialized(Int,Long) A,
//   @specialized(Int,Long,Float,Double) B,
//   +Coll <: MutableDomainMap[A,B]]
//  (underlying : Coll, keys : Seq[A])
//  (implicit override val scalar : Scalar[B])
//  extends MutableDomainMapSliceSeq.FromKeySeq[A,B,Coll](underlying, keys)
//  with SliceSeq[A,B,Coll];
}
