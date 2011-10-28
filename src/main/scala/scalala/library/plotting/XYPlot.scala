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

import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.DefaultDrawingSupplier;
import org.jfree.chart.axis.{NumberAxis,TickUnits,NumberTickUnit};

/** A two dimensional XY plot */
class XYPlot(figure : Figure) {

  val xaxis : NumberAxis = new NumberAxis(null);
  val yaxis : NumberAxis = new NumberAxis(null);

  // Save the default tick units.
  private val xaxisDefaultTickUnits = xaxis.getStandardTickUnits();
  private val yaxisDefaultTickUnits = yaxis.getStandardTickUnits();

  def setXAxisIntegerTickUnits() = xaxis.setStandardTickUnits(XYPlot.integerTickUnits);
  def setYAxisIntegerTickUnits() = yaxis.setStandardTickUnits(XYPlot.integerTickUnits);
  def setXAxisDecimalTickUnits() = xaxis.setStandardTickUnits(xaxisDefaultTickUnits);
  def setYAxisDecimalTickUnits() = yaxis.setStandardTickUnits(yaxisDefaultTickUnits);

  // set integer tick units by default
  Array(xaxis,yaxis) foreach (axis => {
    axis.setAutoRangeIncludesZero(false);
    axis.setStandardTickUnits(XYPlot.integerTickUnits);
  });

  /** The JFreeChart plot object. */  
  lazy val plot = {
    val rv = new org.jfree.chart.plot.XYPlot();
    rv.setDomainAxis(xaxis)
    rv.setRangeAxis(yaxis)
  
    rv.setDrawingSupplier(new DefaultDrawingSupplier(
      XYPlot.paints,
      XYPlot.fillPaints,
      XYPlot.outlinePaints,
      XYPlot.strokes,
      XYPlot.outlineStrokes,
      XYPlot.shapes));
    
    rv;
  }

  /** Adds to the current plot if true, else replaces */
  var hold : Boolean = false
  
  /** The plot title */
  def title_=(str : String) : Unit = chart.setTitle(str);
  def title : String = chart.getTitle.getText;

  /** If we show a legend */
  private var _legend : Boolean = false;
  def legend_=(show : Boolean) : Unit = {
    chart.removeLegend();
    if (show) {
      import org.jfree.chart.title._;
      import org.jfree.ui._;
      import org.jfree.chart.block._;
      import java.awt.Color;
      val legend = new LegendTitle(this.plot);
      legend.setMargin(new RectangleInsets(1.0, 1.0, 1.0, 1.0));
      legend.setFrame(new LineBorder());
      legend.setBackgroundPaint(Color.WHITE);
      legend.setPosition(RectangleEdge.BOTTOM);
      chart.addLegend(legend);
    }
    _legend = show
    refresh()
  }
  def legend : Boolean = _legend

  /** The current series */
  private var _series : Int = -1
  def series = _series

  /** Returns the next series number as per the current hold policy */
  def nextSeries : Int = {
    if (hold) {
      _series += 1
    } else {
      _series = 0
      for (i <- 0 until plot.getDatasetCount()) {
        plot.setDataset(i,null);
        plot.setRenderer(i,null);
      }
      // remove all subtitles, particularly so we can kill all legends
      while (chart.getSubtitleCount > 0) {
        chart.removeSubtitle(chart.getSubtitle(0));
      }
      // re-add the (now-empty) legend for consistency
      legend = _legend;
      // reset to integer ticks
      setXAxisIntegerTickUnits();
      setYAxisIntegerTickUnits();
    }
    return _series
  }

  /** The JFreeChart for this plot */
  lazy val chart = {
    val rv = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false);
    rv.setBackgroundPaint(XYPlot.transparent);
    rv.setPadding(new org.jfree.ui.RectangleInsets(5,0,0,0));
    rv;
  }

  /** The ChartPanel for this plot */
  lazy val panel =
    new org.jfree.chart.ChartPanel(chart);

  /** Shows the given chart */
  def refresh() = figure.refresh()
}

object XYPlot {
  import java.awt.{Color,Paint};
  import org.jfree.chart.ChartColor._;
  import org.jfree.chart.plot.DefaultDrawingSupplier;

  /** Units for integer ticks on axes. */
  val integerTickUnits = {
    val units = new TickUnits();
    val df = new java.text.DecimalFormat("0");
    for (b <- List(1,2,5); e <- List(0,1,2,3,4,5,6,7,8)) {
      units.add(new NumberTickUnit(b * math.pow(10,e).toInt, df));
    }
    units;
  }

  // color cycle ignoring bright colors
  val paints : Array[Paint] = PaintScale.Category20.values.asInstanceOf[Array[Paint]];
  
  def paint(series : Int) =
    paints(series % paints.length);

  val shapes = DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE;
  def shape(series : Int) =
    shapes(series % shapes.length);

  val strokes = DefaultDrawingSupplier.DEFAULT_STROKE_SEQUENCE;
  def stroke(series : Int) =
    strokes(series % strokes.length);

  val fillPaints = paints; // DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE;
  def fillPaint(series : Int) =
    fillPaints(series % fillPaints.length);

  val outlinePaints = paints; // DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE;
  def outlinePaint(series : Int) =
    outlinePaints(series % outlinePaints.length);

  val outlineStrokes = DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE;
  def outlineStroke(series : Int) =
    outlineStrokes(series % outlineStrokes.length);
 
  val transparent = new Color(255,255,255,0);

  //
  // shapes
  //
  
  val dot =
    new java.awt.geom.Ellipse2D.Double(-1,-1,2,2);

  val plus = {
    val shape = new java.awt.geom.GeneralPath();
    shape.moveTo(-3,0);
    shape.lineTo(3,0);
    shape.moveTo(0,-3);
    shape.lineTo(0,3);
    shape;
  };
}

