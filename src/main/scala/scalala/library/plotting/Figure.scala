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

import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.Graphics2D;
import javax.swing.WindowConstants;

/** A Figure holds a collection of XYPlot instances */
class Figure(figures : Figures) {
  /** List of plots in the figure. */
  protected val plots = ArrayBuffer[Option[XYPlot]]();

  /** How many rows of plots are in the figure */
  private var rows_ = 1;
  def rows = rows_;
  def rows_=(newrows : Int) : Unit = {
    rows_ = newrows;
    refresh();
  }

  /** How many cols of plots are in the figure */
  private var cols_ = 1 ;
  def cols = cols_;
  def cols_=(newcols : Int) : Unit = {
    cols_ = newcols;
    refresh();
  }

  /** Visibility state of the plot */
  private var visible_ = true;
  def visible = visible_;
  def visible_=(newvis : Boolean) : Unit = {
    visible_ = newvis;
    frame.setVisible(visible_);
  }

  /** JPanel holding for drawing subplots in this figure. */
  val contents = {
    val _c = new JPanel();
    _c.setSize(600,400);
    _c;
  }

  /** The Swing frame for this plot */
  lazy val frame : JFrame = {
    val f = new JFrame("Figure "+(figures.number(this)));
    f.setSize(600,400);
    f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    f.setLayout(new java.awt.BorderLayout());
    f.add(contents, java.awt.BorderLayout.CENTER);

    // we use visible_ to avoid an infinite loop
    f.setVisible(visible_);

    f
  }

  /** Returns the number of the given plot in this container */
  def number(plot : XYPlot) : Int =
    plots.indexOf(plot);

  private var plot_ = 0;
  /** Returns the current plot in the figure */
  def plot : XYPlot = plots(plot_).get;

  /** Uses the given plot number. */
  def plot_=(number : Int) : Unit = {
    assert(number >= 0, "Plot number must be non-negative");
    while (plots.length <= number) {
      plots += None;
    }
    if (plots(number) == None) {
      plots(number) = Some(new XYPlot(this));
    }
    plot_ = number;
  }

  // create the initial plot (don't call clear lest we pop up a window)
  plot = 0;

  /** Clears the current plot */
  def clear() {
    contents.removeAll();
    plots.clear();
    plot = 0;
    rows = 1;
    cols = 1;
    refresh();
  }

  /** Redraws the figure */
  def refresh() : Unit = {
    while (plots.length < rows * cols) {
      plots += None;
    }
    while (plots.length > rows * cols) {
      plots.remove(plots.length-1);
    }

    contents.removeAll;
    contents.setLayout(new java.awt.GridLayout(rows,cols));
    for (plot <- plots) {
      contents.add(plot match { case Some(plot) => plot.panel; case None => new JPanel() });
    }

    frame.repaint();
    frame.setVisible(visible);
  }

  def drawPlots(g2d : Graphics2D) {
    val plotWidth  = contents.getWidth / cols;
    val plotHeight = contents.getHeight / rows;
    var px = 0; var py = 0;
    for (opt <- plots) {
      opt match {
        case Some(plot) =>
          plot.chart.draw(g2d, new java.awt.Rectangle(px*plotWidth, py*plotHeight, plotWidth, plotHeight));
        case None => {}
      }
      px = (px +1)%cols;
      if(px == 0) py = (py + 1)%rows;
    }
  }
}

