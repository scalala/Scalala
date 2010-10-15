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
package generic;
package collection;

/**
 * Marker for being able to get the value of a map at a key as a double.
 *
 * @author dramage
 */
trait CanGetDouble[-Coll, @specialized(Int,Long) -K] {
  def apply(coll : Coll, key : K) : Double;
}

object CanGetDouble {
  type Op[Coll,K] = CanGetDouble[Coll,K];

  implicit object OpArrayI extends Op[Array[Int],Int]
  { override def apply(coll : Array[Int], key : Int) = coll(key); }

  implicit object OpArrayS extends Op[Array[Short],Int]
  { override def apply(coll : Array[Short], key : Int) = coll(key); }

  implicit object OpArrayL extends Op[Array[Long],Int]
  { override def apply(coll : Array[Long], key : Int) = coll(key); }

  implicit object OpArrayF extends Op[Array[Float],Int]
  { override def apply(coll : Array[Float], key : Int) = coll(key); }

  implicit object OpArrayD extends Op[Array[Double],Int]
  { override def apply(coll : Array[Double], key : Int) = coll(key); }

  implicit def opIndexedSeq[@specialized V](implicit cv : V => Double) =
    new OpIndexedSeq[V];

  class OpIndexedSeq[@specialized V](implicit cv : V => Double) extends CanGetDouble[IndexedSeq[V], Int] {
    def apply(coll : IndexedSeq[V], key : Int) = coll(key);
  }

  implicit object OpIndexedSeqI extends OpIndexedSeq[Int];
  implicit object OpIndexedSeqC extends OpIndexedSeq[Char];
  implicit object OpIndexedSeqS extends OpIndexedSeq[Short];
  implicit object OpIndexedSeqL extends OpIndexedSeq[Long];
  implicit object OpIndexedSeqF extends OpIndexedSeq[Float];
  implicit object OpIndexedSeqD extends OpIndexedSeq[Double];

  implicit def opMap[K,V](implicit cv : V => Double) =
    new OpMap[K,V];

  class OpMap[K,V](implicit cv : V => Double) extends CanGetDouble[scala.collection.Map[K,V], K] {
    def apply(coll : scala.collection.Map[K,V], key : K) = coll(key);
  }
}
