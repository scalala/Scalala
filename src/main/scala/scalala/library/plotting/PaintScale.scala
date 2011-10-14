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
 * Maps items of type T to a well defined Paint (usually a color).
 *
 * An implicit conversion exists to make a singleton PaintScaleFactory from
 * a PaintScale instance, which means that PaintScales can be provided
 * directly whenever a PaintScaleFactory is required.
 *
 * @author dramage
 */
sealed trait PaintScale[T] extends (T => Paint);

/**
 * A simple numeric paint scale for mapping a number within a range to a
 * corresponding element of a pre-computed color gradient.  Colors from the
 * given gradient array are used linearly to represent values between
 * lower and upper.
 *
 * @author dramage
 */
case class GradientPaintScale[T]
(lower : T, upper : T, gradient : Array[Color] = PaintScale.WhiteToBlack)
(implicit view : T=>Double)
extends PaintScale[T] {
  def apply(value : T) : Paint = {
    if (view(value).isNaN) {
      PaintScale.nanPaint
    } else {
      val index = gradient.length * (value - lower) / (upper - lower);
      gradient(math.min(gradient.length-1, math.max(0, index.toInt)));
    }
  }
}

/**
 * Maps items to colors using the given partial function.  If no color
 * is provided for the given item, then returns PaintScale.nanPaint.
 *
 * @author dramage
 */
case class CategoricalPaintScale[T]
(categories : PartialFunction[T,Paint])
extends PaintScale[T] {
  def apply(value : T) : Paint = {
    if (!categories.isDefinedAt(value)) {
      PaintScale.nanPaint
    } else {
      categories(value)
    }
  }
}

object PaintScale {

  /** Creates a GradientPaintScale automatically for the given range. */
  implicit def gradientTuple[T](vLowerUpper : (T,T))(implicit view : T=>Double)
  : GradientPaintScale[T] =
    GradientPaintScale[T](vLowerUpper._1, vLowerUpper._2);

  /** Creates a CategoricalPaintScale from the provided partial function. */
  implicit def literalColorMap[T](map : PartialFunction[T,Paint])
  : CategoricalPaintScale[T] =
    CategoricalPaintScale[T](map);


  //
  // Default colors and patterns.
  //

  /** For painting NaN. */
  val nanPaint = {
    val img = new BufferedImage(5,5,BufferedImage.TYPE_INT_ARGB);
    
    val gfx = img.getGraphics;
    gfx.setColor(Color.gray);
    gfx.drawLine(0,0,4,4);
    gfx.dispose();
    
    new TexturePaint(img, new Rectangle2D.Double(0,0,5,5));
  }

  /** The Category10 palette from Protovis http://vis.stanford.edu/protovis/docs/color.html */
  lazy val Category10 : Array[Color] = Array(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ).map(Color.decode);

  /** The Category20 palette from Protovis http://vis.stanford.edu/protovis/docs/color.html */
  lazy val Category20 : Array[Color] = Category10 ++ Array(
    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
    "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5"
  ).map(Color.decode);

  /** Produces a gradient using the University of Minnesota's school colors, from maroon (low) to gold (high) */
  lazy val MaroonToGold = createGradient(new Color(0xA0, 0x00, 0x00), new Color(0xFF, 0xFF, 0x00), 256);

  /** Produces a gradient from blue (low) to red (high) */
  lazy val BlueToRed= createGradient(Color.BLUE, Color.RED, 500);

  /** Produces a gradient from black (low) to white (high) */
  lazy val BlackToWhite = createGradient(Color.BLACK, Color.WHITE, 500);

  /** Produces a gradient from white (low) to black (high) */
  lazy val WhiteToBlack = createGradient(Color.WHITE, Color.BLACK, 500);

  /** Produces a gradient from red (low) to green (high) */
  lazy val RedToGreen = createGradient(Color.RED, Color.GREEN, 500);

  /** Produces a gradient through green, yellow, orange, red */
  lazy val GreenYelloOrangeRed = createMultiGradient(
    Array(Color.green, Color.yellow, Color.orange, Color.red), 500);

  /** Produces a gradient through the rainbow: violet, blue, green, yellow, orange, red */
  lazy val Rainbow = createMultiGradient(
    Array(new Color(181, 32, 255), Color.blue, Color.green, Color.yellow, Color.orange, Color.red), 500);

  /** Produces a gradient for hot things (black, red, orange, yellow, white) */
  lazy val Hot = createMultiGradient(
    Array(Color.black, new Color(87, 0, 0), Color.red, Color.orange, Color.yellow, Color.white), 500);

  /** Produces a different gradient for hot things (black, brown, orange, white) */
  lazy val Heat = createMultiGradient(
    Array(Color.black, new Color(105, 0, 0), new Color(192, 23, 0), new Color(255, 150, 38), Color.white), 500);

  /** Produces a gradient through red, orange, yellow */
  lazy val RedOrangeYellow = createMultiGradient(
    Array(Color.red, Color.orange, Color.yellow), 500);

  /**
   * Creates an array of Color objects for use as a gradient, using a linear
   * interpolation between the two specified colors.
   *
   * From http://www.mbeckler.org/heatMap/heatMap.html
   *
   * @param one Color used for the bottom of the gradient
   * @param two Color used for the top of the gradient
   * @param numSteps The number of steps in the gradient. 250 is a good number.
   */
  def createGradient(one : Color, two : Color, numSteps : Int) : Array[Color] = {
    val r1 = one.getRed();
    val g1 = one.getGreen();
    val b1 = one.getBlue();

    val r2 = two.getRed();
    val g2 = two.getGreen();
    val b2 = two.getBlue();

    val gradient = new Array[Color](numSteps);
    var iNorm : Double = 0;
    for (i <- 0 until numSteps) {
      iNorm = i / numSteps.toDouble; //a normalized [0:1] variable
      val newR = (r1 + iNorm * (r2 - r1)).toInt;
      val newG = (g1 + iNorm * (g2 - g1)).toInt;
      val newB = (b1 + iNorm * (b2 - b1)).toInt;
      gradient(i) = new Color(newR, newG, newB);
    }

    return gradient;
  }

  /**
   * Creates an array of Color objects for use as a gradient, using an array
   * of Color objects. It uses a linear interpolation between each pair of
   * points.
   *
   * From http://www.mbeckler.org/heatMap/heatMap.html
   *
   * @param colors An array of Color objects used for the gradient. The
   *   Color at index 0 will be the lowest color.
   *
   * @param numSteps The number of steps in the gradient. 250 is a good number.
   */
  def createMultiGradient(colors : Array[Color], numSteps : Int) : Array[Color] = {
    //we assume a linear gradient, with equal spacing between colors
    //The final gradient will be made up of n 'sections', where n = colors.length - 1
    val numSections = colors.length - 1;
    var gradientIndex = 0; //points to the next open spot in the final gradient
    val gradient = new Array[Color](numSteps)

    require(numSections > 0, "Array must have at least two colors");

    for (section <- 0 until numSections) {
      //we divide the gradient into (n - 1) sections, and do a regular gradient for each
      val temp = createGradient(colors(section), colors(section+1), numSteps / numSections);
      for (i <- 0 until temp.length) {
        //copy the sub-gradient into the overall gradient
        gradient(gradientIndex) = temp(i);
        gradientIndex += 1
      }
    }

    if (gradientIndex < numSteps) {
      //The rounding didn't work out in our favor, and there is at least
      // one unfilled slot in the gradient[] array.
      //We can just copy the final color there
      while (gradientIndex < numSteps) {
        gradient(gradientIndex) = colors(colors.length - 1);
        gradientIndex += 1
      }
    }

    return gradient;
  }
}

