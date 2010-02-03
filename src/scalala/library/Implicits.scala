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
package scalala.library;

import scalala.collection.{PartialMap,MergeableSet,IntSpanSet};
import scalala.tensor.dense.DenseVector;

import scala.collection.IndexedSeqLike;
import scala.reflect.Manifest;

/**
 * A trait alias for convenient access to scalala.tensor.TensorImplicits.
 * 
 * @author dramage
 */
trait Implicits {
  implicit def iRichIndexedSeq[T,Repr](seq : IndexedSeqLike[T,Repr])(implicit m : Manifest[T]) =
    RichIndexedSeqLike(seq);

  implicit def iRichDoubleArray(array : Array[Double]) =
    RichDoubleArray(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Int]) =
    iRichIndexedSeq(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Long]) =
    iRichIndexedSeq(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Byte]) =
    iRichIndexedSeq(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Short]) =
    iRichIndexedSeq(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Float]) =
    iRichIndexedSeq(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Char]) =
    iRichIndexedSeq(array);

  implicit def iRichIndexedSeqFromArray(array : Array[Boolean]) =
    iRichIndexedSeq(array);
}

case class RichIndexedSeqLike[T,Repr](seq : IndexedSeqLike[T,Repr])(implicit m : Manifest[T]) {
  def asPartialMapWithDefault(defaultValue : T) = new PartialMap[Int,T] {
    override def default : T = defaultValue;
    override def domain : MergeableSet[Int] = IntSpanSet(0, seq.length);
    override def activeDomain = domain;
    override def apply(i : Int) = seq(i);
  }

  def asPartialMap : PartialMap[Int,T] = {
    val default : T = m.toString match {
      case "Double" => 0.0.asInstanceOf[T];
      case "Int" => 0.asInstanceOf[T];
      case "Float" => 0f.asInstanceOf[T];
      case "Long" => 0l.asInstanceOf[T];
      case "Byte" => (0:Byte).asInstanceOf[T];
      case "Short" => (0:Short).asInstanceOf[T];
      case "Char" => (0:Char).asInstanceOf[T];
      case "Boolean" => (false).asInstanceOf[T];
      case _ => null.asInstanceOf[T];
    }
    asPartialMapWithDefault(default);
  }
}

/**
 * A version of Array[Double] that contains a method toVector to construct
 * a DenseVector view of the given array.
 */
case class RichDoubleArray(array : Array[Double]) {
  /**
   * Constructs a DenseVector view of the given array.  Changes to the
   * vector are propragated into the calling array.
   */
  def asVector = new DenseVector(array);
}

/**
 * An object with access to scalala.tensor.TensorImplicits.
 * 
 * @author bethard
 */
object Implicits extends Implicits { }
