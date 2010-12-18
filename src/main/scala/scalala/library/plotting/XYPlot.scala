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
import org.jfree.chart.ChartPanel;
import org.jfree.chart.plot.DefaultDrawingSupplier;
import org.jfree.chart.axis.NumberAxis;

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

  plot.setDrawingSupplier(new DefaultDrawingSupplier(
    XYPlot.paints,
    XYPlot.fillPaints,
    XYPlot.outlinePaints,
    XYPlot.strokes,
    XYPlot.outlineStrokes,
    XYPlot.shapes));

  /** The plot title */
  // private var title_ : String = ""
  def title_=(str : String) : Unit = {
    chart.setTitle(str);
  }
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
      legend.setBackgroundPaint(Color.white);
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
    }
    return _series
  }

  /** The JFreeChart for this plot */
  lazy val chart =
    new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false);

  /** The ChartPanel for this plot */
  lazy val panel =
    new ChartPanel(chart);

  /** Shows the given chart */
  def refresh() = figure.refresh()
}

object XYPlot {
  import java.awt.{Color,Paint};
  import org.jfree.chart.ChartColor._;
  import org.jfree.chart.plot.DefaultDrawingSupplier;

  // color cycle ignoring bright colors
  val paints = Array[Paint](
     VERY_DARK_RED, VERY_DARK_BLUE, VERY_DARK_GREEN, VERY_DARK_YELLOW, VERY_DARK_MAGENTA, VERY_DARK_CYAN,
     Color.darkGray,
     DARK_RED, DARK_BLUE, DARK_GREEN, DARK_MAGENTA, DARK_CYAN
  );
  def paint(series : Int) =
    paints(series % paints.length);

  val shapes = DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE;
  def shape(series : Int) =
    shapes(series % shapes.length);

  val strokes = DefaultDrawingSupplier.DEFAULT_STROKE_SEQUENCE;
  def stroke(series : Int) =
    strokes(series % strokes.length);

  val fillPaints = DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE;
  def fillPaint(series : Int) =
    fillPaints(series % fillPaints.length);

  val outlinePaints = DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE;
  def outlinePaint(series : Int) =
    outlinePaints(series % outlinePaints.length);

  val outlineStrokes = DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE;
  def outlineStroke(series : Int) =
    outlineStrokes(series % outlineStrokes.length);

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
