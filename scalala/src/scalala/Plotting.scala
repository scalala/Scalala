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

import org.jfree.chart._
import org.jfree.chart.plot._

import java.awt.Container
import javax.swing._
  
import org.jfree.chart._
import org.jfree.chart.axis._
import org.jfree.chart.plot._
  
import org.jfree.data.xy.{XYDataset, XYZDataset}
import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer

/** Plotting operations */
object Plotting {
  
  /** Returns a JFreeChart XYDataset for plotting y vs x. */
  def Dataset(x : Vector, y : Vector) : XYDataset = {
    assert(x.size == y.size)
    new org.jfree.data.xy.AbstractXYDataset() {
      override def getX(series : Int, item : Int) : Number = {
        if (series == 0) { java.lang.Double.valueOf(x.get(item)) } else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) { java.lang.Double.valueOf(y.get(item)) } else null
      }
      override def getItemCount(series : Int) = x.size
      override def getSeriesKey(series : Int) = "1"
      override def getSeriesCount() = 1
    }
  }
  
  /** Returns a JFreeChart XYZDataset for plotting all elements of the matrix */
  def Dataset(m : Matrix) : XYZDataset = {
    def getCol(item : Int) = (item - (item % m.rows)) / m.rows
    def getRow(item : Int) = item % m.rows
    
    new org.jfree.data.xy.AbstractXYZDataset() {
      override def getX(series : Int, item : Int) : Number = {
        if (series == 0) getCol(item) else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) m.rows-getRow(item) else null
      }
      override def getZ(series : Int, item : Int) : Number = {
        if (series == 0) m.get(getRow(item), getCol(item)) else null
      }
      override def getItemCount(series : Int) = m.rows * m.cols
      override def getSeriesKey(series: Int) = "1"
      override def getSeriesCount() = 1
    }
  }
  
  /** Returns an XYZ dataset co-indexed based on the given vectors */
  def Dataset(x : Vector, y : Vector, z : Vector) : XYZDataset = {
    assert(x.size == y.size)
    assert(y.size == z.size)
    
    new org.jfree.data.xy.AbstractXYZDataset {
      override def getX(series : Int, item : Int) : Number = {
        if (series == 0) x.get(item) else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) y.get(item) else null
      }
      override def getZ(series : Int, item : Int) : Number = {
        if (series == 0) z.get(item) else null
      }
      override def getItemCount(series : Int) = x.size
      override def getSeriesKey(series: Int) = "1"
      override def getSeriesCount() = 1
    }
  }
  
  /** Returns an XYZW dataset co-indexed bysed on the given vectors */
  def Dataset(x : Vector, y : Vector, z : Vector, w : Vector) : XYZWDataset = {
    assert(x.size == y.size)
    assert(y.size == z.size)
    assert(z.size == w.size)
    
    new org.jfree.data.xy.AbstractXYZDataset with XYZWDataset {
      override def getX(series : Int, item : Int) : Number = {
        if (series == 0) x.get(item) else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) y.get(item) else null
      }
      override def getZ(series : Int, item : Int) : Number = {
        if (series == 0) z.get(item) else null
      }
      override def getW(series : Int, item : Int) : Number = {
        if (series == 0) w.get(item) else null
      }
      override def getItemCount(series : Int) = x.size
      override def getSeriesKey(series: Int) = "1"
      override def getSeriesCount() = 1
    }
  }
  
  /** A dataset with two numeric values associated with each point */
  trait XYZWDataset extends XYZDataset {
    def getW(series : Int, item : Int) : Number
    def getWValue(series : Int, item : Int) : Double = {
      val w = getW(series, item)
      if (w == null) Double.NaN else w.doubleValue
    }
  }
  
  def Tooltips(labels : Seq[String]) = {
    new XYToolTipGenerator() {
      override def generateToolTip(dataset : XYDataset, series : Int, item : Int) : String = {
        labels(item)
      }
    }
  }
  
  /** Class that holds a collection of Figure instances */
  class Figures {
    private val figures = new scala.collection.jcl.LinkedList[Figure]
    
    // Set the current figure to figure 0
    figure = 0
    
    /** Returns the number of the given figure */
    def number(figure : Figure) : Int = figures.indexOf(figure)
    
    /** Returns the current figure */
    var figure_ : Int = 0
    def figure : Figure = figures(figure_)
    def figure_=(number : Int) : Unit = {
      while (figures.length <= number) {
        figures.add(null)
      }
      if (figures(number) == null) {
        figures(number) = new Figure(this)
      }
      figure_ = number
    }
    
    /** Returns the current figure's current plot */
    def plot : XYPlot = figure.plot
  }
  
  /** A Figure holds a collection of XYPlot instances */
  class Figure(figures : Figures) {
    /** List of plots in the figure */
    private val plots = new scala.collection.jcl.LinkedList[XYPlot]

    // set the current plot to plot 0
    plot = 0

    /** The Swing frame for this plot */
    lazy val frame : JFrame = {
      val f = new JFrame("Figure "+(figures.number(this)+1))
      f.setSize(600,400)
      f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      f.setLayout(new java.awt.GridLayout(cols,rows))
      f
    }
    
    /** How many rows of plots are in the figure */
    private var rows_ = 1
    def rows = rows_
    def rows_=(newrows : Int) : Unit = {
      rows_ = newrows
      refresh()
    }
    
    /** How many cols of plots are in the figure */
    private var cols_ = 1 
    def cols = cols_
    def cols_=(newcols : Int) : Unit = {
      cols_ = newcols
      refresh()
    }
    
    def number(plot : XYPlot) : Int = plots.indexOf(plot)
    
    private var plot_ = 0
    def plot : XYPlot = plots(plot_)
    def plot_=(number : Int) : Unit = {
      assert(number >= 0)
      while (plots.length <= number) {
        plots.add(null)
      }
      if (plots(number) == null) {
        plots(number) = new XYPlot(this)
      }
      plot_ = number
    }
    
    /** Redraws the figure */
    def refresh() : Unit = {
      frame.getContentPane.removeAll
      frame.getContentPane.setLayout(new java.awt.GridLayout(rows,cols))
      for (plot <- plots) {
        frame.getContentPane.add(if (plot == null) new JPanel() else plot.panel)
      }
      frame.repaint()
      frame.setVisible(true)
    }
  }
  
  /** A two dimensional XY plot */
  class XYPlot(figure : Figure) {
    val plot  = new org.jfree.chart.plot.XYPlot()
    val xaxis : NumberAxis = new NumberAxis(null)
    val yaxis : NumberAxis = new NumberAxis(null)

    /** Adds to the current plot if true, else replaces */
    var hold : Boolean = false
    
    Array(xaxis,yaxis) foreach (axis => axis.setAutoRangeIncludesZero(false))
    plot.setDomainAxis(xaxis)
    plot.setRangeAxis(yaxis)
    
    /** The plot title */
    private var title_ : String = ""
    def title_=(str : String) : Unit = {
      title_ = str
      refresh()
    }
    def title : String = title_

    /** If we show a legend */
    private var legend_ : Boolean = false
    def legend_=(show : Boolean) : Unit = {
      legend_ = show
      refresh()
    }
    def legend : Boolean = legend_
    
    /** The current series */
    private var series_ : Int = -1
    def series = series_
    
    /** Returns the next series number as per the current hold policy */
    def nextSeries : Int = {
      if (hold) {
        series_ += 1
      } else {
        series_ = 0
        for (i <- 0 until plot.getDatasetCount()) {
          plot.setDataset(i,null)
        }
      }
      return series_
    }

    /** Returns a new ChartPanel for this chart */
    def panel = new ChartPanel(
          new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend))
    
    /** Shows the given chart */
    def refresh() = figure.refresh()
  }
}

/**
 * Color graident code from http://www.mbeckler.org/heatMap/heatMap.html
 */
object Gradients {
  import java.awt.Color
  
  /** Produces a gradient using the University of Minnesota's school colors, from maroon (low) to gold (high) */
  lazy val GRADIENT_MAROON_TO_GOLD = createGradient(new Color(0xA0, 0x00, 0x00), new Color(0xFF, 0xFF, 0x00), 256);

  /** Produces a gradient from blue (low) to red (high) */
  lazy val GRADIENT_BLUE_TO_RED = createGradient(Color.BLUE, Color.RED, 500);

  /** Produces a gradient from black (low) to white (high) */
  lazy val GRADIENT_BLACK_TO_WHITE = createGradient(Color.BLACK, Color.WHITE, 500);

  /** Produces a gradient from red (low) to green (high) */
  lazy val GRADIENT_RED_TO_GREEN = createGradient(Color.RED, Color.GREEN, 500);

  /** Produces a gradient through green, yellow, orange, red */
  lazy val GRADIENT_GREEN_YELLOW_ORANGE_RED = createMultiGradient(
    Array(Color.green, Color.yellow, Color.orange, Color.red), 500);
    
  /** Produces a gradient through the rainbow: violet, blue, green, yellow, orange, red */
  lazy val GRADIENT_RAINBOW = createMultiGradient(
    Array(new Color(181, 32, 255), Color.blue, Color.green, Color.yellow, Color.orange, Color.red), 500);

  /** Produces a gradient for hot things (black, red, orange, yellow, white) */
  lazy val GRADIENT_HOT = createMultiGradient(
    Array(Color.black, new Color(87, 0, 0), Color.red, Color.orange, Color.yellow, Color.white), 500);
    
  /** Produces a different gradient for hot things (black, brown, orange, white) */
  lazy val GRADIENT_HEAT = createMultiGradient(
    Array(Color.black, new Color(105, 0, 0), new Color(192, 23, 0), new Color(255, 150, 38), Color.white), 500);
  
  /** Produces a gradient through red, orange, yellow */
  lazy val GRADIENT_ROY = createMultiGradient(
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
