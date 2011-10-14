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
package scalala;
package library;
package plotting;

import java.awt.{Color,Paint,TexturePaint};
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

/**
 * Constructs a PaintScale for the given type T by examining a set of its
 * values.
 *
 * @author dramage
 */
trait PaintScaleFactory[T] extends (Traversable[T] => PaintScale[T]);

/**
 * Creates a GradientPaintScale from the min and max of a set of data points.
 * bound are supplied.
 *
 * @author dramage
 */
case class GradientPaintScaleFactory[T]
(gradient : Array[Color] = PaintScale.WhiteToBlack)
(implicit view : T=>Double)
extends PaintScaleFactory[T] {
  override def apply(items : Traversable[T]) : PaintScale[T] = {
    var min = items.head;
    var max = items.head;
    for (item <- items) {
      if (!view(item).isNaN) {
        if (item < min) min = item;
        if (item > max) max = item;
      }
    }
    GradientPaintScale(min, max, gradient);
  }
}

/**
 * Creates a categorical paint scale using the Category20 palette borrowed from
 * Protovis. http://vis.stanford.edu/protovis/docs/color.html
 *
 * Beware that category colors can be reused if the number of distinct items
 * is greater than 20.
 *
 * @author dramage
 */
case class CategoricalPaintScaleFactory[T]() extends PaintScaleFactory[T] {
  override def apply(items : Traversable[T]) : PaintScale[T] = {
    val distinct = items.toList.distinct;
    CategoricalPaintScale[T](Map() ++ (distinct zip Stream.continually(PaintScale.Category20.toList).flatten));
  }
}

object PaintScaleFactory {

  /**
   * Ignores incoming data, instead returns the provided PaintScale when
   * queried as a PaintScaleFactory.
   */
  implicit def singletonFactoryForPaintScale[S,T](paintScale : S)
  (implicit view : S=>PaintScale[T])
  : PaintScaleFactory[T] = new PaintScaleFactory[T] {
    def apply(items : Traversable[T]) = view(paintScale);
  }
}

