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

import scala.collection.mutable.ArrayBuffer;

/** Class that holds a collection of Figure instances */
class Figures {
  private val figures = ArrayBuffer[Option[Figure]]();

  /** Returns the number of the given figure */
  def number(figure : Figure) : Int = figures.indexOf(Some(figure));

  /** Returns the current figure */
  var figure_ : Int = 0;
  def figure : Figure = figures(figure_).get;
  def figure_=(number : Int) : Unit = {
    while (figures.length <= number) {
      figures += None;
    }
    if (figures(number) == None) {
      figures(number) = Some(new Figure(this));
    }
    figure_ = number;
  }

  /** Returns the current figure's current plot */
  def plot : XYPlot = figure.plot;

  // Set the current figure to figure 0
  figure = 0;
}

object Figures {
  lazy val global = new Figures();
}
