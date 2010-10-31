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
package mutable;

import domain._;
import generic.collection._;

/**
 * Implementation trait for a Tensor that is also a tensor.Vector.
 *
 * @author dramage.
 */
trait VectorLike[@specialized(Int,Long,Float,Double,Boolean) B, +Repr<:Vector[B]]
extends tensor.VectorLike[B,Repr] with Tensor1Like[Int,B,IndexDomain,Repr] {

  def := (seq : Seq[B]) = {
    checkDomain(IndexDomain(seq.length));

    var i = 0;
    for (v <- seq) {
      this(i) = v;
      i += 1;
    }
  }

  def :=[O] (seq : Seq[O])(implicit tf : O=>B) = {
    checkDomain(IndexDomain(seq.length));

    var i = 0;
    for (v <- seq) {
      this(i) = v;
      i += 1;
    }
  }

  /** In-place quick-sort of the values in this sequence. */
  def sort(implicit ord : Ordering[B]) : Unit =
    quickSort(0, size);

  /** Adapted from scala.util.Sorting.sort1 */
  private def quickSort[K](off: Int, len: Int)(implicit ord: Ordering[B]) {
    import ord._
    val x = this;
    @inline def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    @inline def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a, b)
        i += 1
        a += 1
        b += 1
      }
    }
    @inline def med3(a: Int, b: Int, c: Int) = {
      if (x(a) < x(b)) {
        if (x(b) < x(c)) b else if (x(a) < x(c)) c else a
      } else {
        if (x(b) > x(c)) b else if (x(a) > x(c)) c else a
      }
    }
    @inline def sort2(off: Int, len: Int) {
      // Insertion sort on smallest arrays
      if (len < 7) {
        var i = off
        while (i < len + off) {
          var j = i
          while (j > off && x(j-1) > x(j)) {
            swap(j, j-1)
            j -= 1
          }
          i += 1
        }
      } else {
        // Choose a partition element, v
        var m = off + (len >> 1)        // Small arrays, middle element
        if (len > 7) {
          var l = off
          var n = off + len - 1
          if (len > 40) {        // Big arrays, pseudomedian of 9
            var s = len / 8
            l = med3(l, l+s, l+2*s)
            m = med3(m-s, m, m+s)
            n = med3(n-2*s, n-s, n)
          }
          m = med3(l, m, n) // Mid-size, med of 3
        }
        val v = x(m)

        // Establish Invariant: v* (<v)* (>v)* v*
        var a = off
        var b = a
        var c = off + len - 1
        var d = c
        var done = false
        while (!done) {
          while (b <= c && x(b) <= v) {
            if (x(b) == v) {
              swap(a, b)
              a += 1
            }
            b += 1
          }
          while (c >= b && x(c) >= v) {
            if (x(c) == v) {
              swap(c, d)
              d -= 1
            }
            c -= 1
          }
          if (b > c) {
            done = true
          } else {
            swap(b, c)
            c -= 1
            b += 1
          }
        }

        // Swap partition elements back to middle
        val n = off + len
        var s = math.min(a-off, b-a)
        vecswap(off, b-s, s)
        s = math.min(d-c, n-d-1)
        vecswap(b,   n-s, s)

        // Recursively sort non-partition-elements
        s = b - a
        if (s > 1)
          sort2(off, s)
        s = d - c
        if (s > 1)
          sort2(n-s, s)
      }
    }
    sort2(off, len)
  }
}

/**
 * A Vector is a Tensor indexed by Ints on the IndexDomain.
 *
 * @author dramage
 */
trait Vector[@specialized(Int,Long,Float,Double,Boolean) B]
extends tensor.Vector[B] with Tensor1[Int,B]
with VectorLike[B,Vector[B]];


object Vector extends VectorCompanion[Vector] with dense.DenseVectorConstructors;

trait VectorCompanion[Bound[V]<:Vector[V]]
extends tensor.VectorCompanion[Bound] with IndexedTensorCompanion[Int,Bound];
