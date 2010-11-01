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

import java.awt.Color;

/**
 * Paint scale for for a color-coded dataset.
 *
 * @author dramage
 */
sealed trait PaintScale;

/**
 * Static, pre-specified paint scale from lower to upper using the given gradient.
 * Colors from the given gradient are used to linearly represent values between
 * lower and upper.
 *
 * @author dramage
 */
case class StaticPaintScale(lower : Double, upper : Double, gradient : Array[Color] = PaintScale.BlueToRed)
extends PaintScale {
  def color(value : Double) = {
    val index = gradient.length * (value - lower) / (upper - lower);
    gradient(math.min(gradient.length-1, math.max(0, index.toInt)));
  }
}

/**
 * Specifes a gradient, and returns a StaticPaintScale when a lower and upper
 * bound are supplied.
 *
 * @author dramage
 */
case class DynamicPaintScale(gradient : Array[Color] = PaintScale.BlueToRed)
extends PaintScale {
  def apply(lower : Double, upper : Double) =
    StaticPaintScale(lower, upper, gradient);
}

/**
 * Color graident code from http://www.mbeckler.org/heatMap/heatMap.html
 */
object PaintScale {

  implicit def paintScaleFromRange(vLowerUpper : (Double,Double)) : PaintScale =
    StaticPaintScale(vLowerUpper._1, vLowerUpper._2);

  /** Produces a gradient using the University of Minnesota's school colors, from maroon (low) to gold (high) */
  lazy val MaroonToGold = createGradient(new Color(0xA0, 0x00, 0x00), new Color(0xFF, 0xFF, 0x00), 256);

  /** Produces a gradient from blue (low) to red (high) */
  lazy val BlueToRed= createGradient(Color.BLUE, Color.RED, 500);

  /** Produces a gradient from black (low) to white (high) */
  lazy val BlackToWhite = createGradient(Color.BLACK, Color.WHITE, 500);

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

    if (numSections <= 0) {
        throw new IllegalArgumentException("You must pass in at least 2 colors in the array!");
    }

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
