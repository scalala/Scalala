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
 * Marker for being able to get the value of a map at a key.
 *
 * @author dramage
 */
trait CanGetValue[-Coll, @specialized(Int,Long) -K, @specialized(Int,Long,Float,Double) +V] {
  def apply(coll : Coll, key : K) : V;
}

object CanGetValue {
  implicit def opArray[@specialized V] =
    new OpArray[V];

  class OpArray[@specialized V] extends CanGetValue[Array[V], Int, V] {
    def apply(coll : Array[V], key : Int) = coll(key);
  }

  implicit object OpArrayI extends OpArray[Int];
  implicit object OpArrayC extends OpArray[Char];
  implicit object OpArrayS extends OpArray[Short];
  implicit object OpArrayL extends OpArray[Long];
  implicit object OpArrayF extends OpArray[Float];
  implicit object OpArrayD extends OpArray[Double];
  implicit object OpArrayB extends OpArray[Boolean];

  implicit def opIndexedSeq[@specialized V] =
    new OpIndexedSeq[V];

  class OpIndexedSeq[@specialized V] extends CanGetValue[IndexedSeq[V], Int, V] {
    def apply(coll : IndexedSeq[V], key : Int) = coll(key);
  }

  implicit object OpIndexedSeqI extends OpIndexedSeq[Int];
  implicit object OpIndexedSeqC extends OpIndexedSeq[Char];
  implicit object OpIndexedSeqS extends OpIndexedSeq[Short];
  implicit object OpIndexedSeqL extends OpIndexedSeq[Long];
  implicit object OpIndexedSeqF extends OpIndexedSeq[Float];
  implicit object OpIndexedSeqD extends OpIndexedSeq[Double];
  implicit object OpIndexedSeqB extends OpIndexedSeq[Boolean];

  implicit def opMap[K,V] =
    new OpMap[K,V];

  class OpMap[K,V] extends CanGetValue[scala.collection.Map[K,V], K, V] {
    def apply(coll : scala.collection.Map[K,V], key : K) = coll(key);
  }
}
