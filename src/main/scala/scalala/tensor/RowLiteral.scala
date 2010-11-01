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

/**
 * Marker trait for a row literal used in Matrix construction.
 *
 * @author dramage
 */
trait RowLiteral[-R, @specialized V] {
  def foreach[X](row : R, fn : ((Int,V) => X));
  def length(row : R) : Int;
}

object RowLiteral {
  implicit def array[V] : RowLiteral[Array[V],V] = new RowLiteral[Array[V],V] {
    def foreach[X](arr : Array[V], fn : ((Int,V) => X)) = {
      for (i <- 0 until arr.length) {
        fn(i, arr(i));
      }
    }

    def length(arr : Array[V]) = arr.length;
  }

  implicit def row[V] : RowLiteral[VectorRow[V],V] = new RowLiteral[VectorRow[V],V] {
    def foreach[X](row : VectorRow[V], fn : ((Int,V) => X)) = {
      row.foreachNonZero(fn);
    }

    def length(row : VectorRow[V]) = row.size;
  }

  implicit def tuple2[V] : RowLiteral[Tuple2[V,V],V] = new RowLiteral[Tuple2[V,V],V] {
    def foreach[X](tup : Tuple2[V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
    }

    def length(tup : Tuple2[V,V]) = 2;
  }


  implicit def tuple3[V] : RowLiteral[Tuple3[V,V,V],V] = new RowLiteral[Tuple3[V,V,V],V] {
    def foreach[X](tup : Tuple3[V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
    }

    def length(tup : Tuple3[V,V,V]) = 3;
  }


  implicit def tuple4[V] : RowLiteral[Tuple4[V,V,V,V],V] = new RowLiteral[Tuple4[V,V,V,V],V] {
    def foreach[X](tup : Tuple4[V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
    }

    def length(tup : Tuple4[V,V,V,V]) = 4;
  }


  implicit def tuple5[V] : RowLiteral[Tuple5[V,V,V,V,V],V] = new RowLiteral[Tuple5[V,V,V,V,V],V] {
    def foreach[X](tup : Tuple5[V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
    }

    def length(tup : Tuple5[V,V,V,V,V]) = 5;
  }


  implicit def tuple6[V] : RowLiteral[Tuple6[V,V,V,V,V,V],V] = new RowLiteral[Tuple6[V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple6[V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
    }

    def length(tup : Tuple6[V,V,V,V,V,V]) = 6;
  }


  implicit def tuple7[V] : RowLiteral[Tuple7[V,V,V,V,V,V,V],V] = new RowLiteral[Tuple7[V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple7[V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
    }

    def length(tup : Tuple7[V,V,V,V,V,V,V]) = 7;
  }


  implicit def tuple8[V] : RowLiteral[Tuple8[V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple8[V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple8[V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
    }

    def length(tup : Tuple8[V,V,V,V,V,V,V,V]) = 8;
  }


  implicit def tuple9[V] : RowLiteral[Tuple9[V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple9[V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple9[V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
    }

    def length(tup : Tuple9[V,V,V,V,V,V,V,V,V]) = 9;
  }


  implicit def tuple10[V] : RowLiteral[Tuple10[V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple10[V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple10[V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
    }

    def length(tup : Tuple10[V,V,V,V,V,V,V,V,V,V]) = 10;
  }


  implicit def tuple11[V] : RowLiteral[Tuple11[V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple11[V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple11[V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
    }

    def length(tup : Tuple11[V,V,V,V,V,V,V,V,V,V,V]) = 11;
  }


  implicit def tuple12[V] : RowLiteral[Tuple12[V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple12[V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple12[V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
    }

    def length(tup : Tuple12[V,V,V,V,V,V,V,V,V,V,V,V]) = 12;
  }


  implicit def tuple13[V] : RowLiteral[Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
    }

    def length(tup : Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V]) = 13;
  }


  implicit def tuple14[V] : RowLiteral[Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
    }

    def length(tup : Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 14;
  }


  implicit def tuple15[V] : RowLiteral[Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
    }

    def length(tup : Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 15;
  }


  implicit def tuple16[V] : RowLiteral[Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
    }

    def length(tup : Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 16;
  }


  implicit def tuple17[V] : RowLiteral[Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
      fn(16, tup._17);
    }

    def length(tup : Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 17;
  }


  implicit def tuple18[V] : RowLiteral[Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
      fn(16, tup._17);
      fn(17, tup._18);
    }

    def length(tup : Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 18;
  }


  implicit def tuple19[V] : RowLiteral[Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
      fn(16, tup._17);
      fn(17, tup._18);
      fn(18, tup._19);
    }

    def length(tup : Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 19;
  }


  implicit def tuple20[V] : RowLiteral[Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
      fn(16, tup._17);
      fn(17, tup._18);
      fn(18, tup._19);
      fn(19, tup._20);
    }

    def length(tup : Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 20;
  }


  implicit def tuple21[V] : RowLiteral[Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
      fn(16, tup._17);
      fn(17, tup._18);
      fn(18, tup._19);
      fn(19, tup._20);
      fn(20, tup._21);
    }

    def length(tup : Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 21;
  }


  implicit def tuple22[V] : RowLiteral[Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new RowLiteral[Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1);
      fn(1, tup._2);
      fn(2, tup._3);
      fn(3, tup._4);
      fn(4, tup._5);
      fn(5, tup._6);
      fn(6, tup._7);
      fn(7, tup._8);
      fn(8, tup._9);
      fn(9, tup._10);
      fn(10, tup._11);
      fn(11, tup._12);
      fn(12, tup._13);
      fn(13, tup._14);
      fn(14, tup._15);
      fn(15, tup._16);
      fn(16, tup._17);
      fn(17, tup._18);
      fn(18, tup._19);
      fn(19, tup._20);
      fn(20, tup._21);
      fn(21, tup._22);
    }

    def length(tup : Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 22;
  }
}
