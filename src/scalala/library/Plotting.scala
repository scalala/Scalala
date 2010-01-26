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
package scalala.library;

import scala.collection.mutable.ArrayBuffer;

import scalala.collection.PartialMap;
import scalala.tensor.{Tensor,Vector,Matrix};
import scalala.tensor.dense.{DenseVector,DenseMatrix};

/**
 * Matlab-like plotting routines.
 * 
 * @author dramage
 */
trait Plotting extends Library with Vectors with Matrices with Operators {
  import PlottingSupport._;
  
  /** Implicit graphics context */
  implicit var _scalala_figures = new Figures();
  implicit def _scalala_figure = _scalala_figures.figure;
  implicit def _scalala_xyplot = _scalala_figures.figure.plot;
  
  implicit def iFigure(figures : Figures) = figures.figure;
  implicit def iXYPlot(figures : Figures) = figures.figure.plot;
  implicit def iXYPlot(figure  : Figure)  = figure.plot;
  
  //
  // Plotting
  //

  /** Selects the given figure */
  def figure(select:Int)(implicit figures : Figures) : Figure = {
    figures.figure = select-1
    figures.figure.refresh
    return figures.figure
  }
  
  /** Clears the current figure */
  def clf()(implicit figure : Figure) = {
    figure.clear;
  }
  
  /** Saves the current figure at 72 dpi to the given filename. */
  def saveas(filename : String)(implicit figure : Figure) : Unit = {
    saveas(filename, 72)(figure);
  }
  
  /** Saves the current figure at the requested dpi to the given filename. */
  def saveas(filename : String, dpi : Int)(implicit figure : Figure) : Unit = {
    import java.io._;
    
    // make sure figure is visible or saved image will come up empty
    figure.refresh();
    
    lazy val fos = new FileOutputStream(filename);
    if (filename.toLowerCase.endsWith(".eps")) {
      figure.writeEPS(fos,dpi);
      fos.close();
    } else if (filename.toLowerCase.endsWith(".png")) {
      figure.writePNG(fos,dpi);
      fos.close();
    } else if (filename.toLowerCase.endsWith(".pdf")) {
      figure.writePDF(fos);
      fos.close();
    } else {
      throw new IOException("Unrecognized file extension: should be eps or png");
    }
  }
  
  /** Selects the given subplot */
  def subplot(rows:Int,cols:Int,select:Int)(implicit figure : Figure) : XYPlot = {
    figure.rows = rows
    figure.cols = cols
    figure.plot = select-1
    figure.refresh
    return figure.plot
  }

  /** Sets the title of this figure. */
  def title(title : String)(implicit xyplot : XYPlot) {
    xyplot.title = title;
  }
  
  /** Sets the label of the x axis */
  def xlabel(text : String)(implicit xyplot : XYPlot) {
    xyplot.xaxis.setLabel(text);
  }
  
  /** Sets the label of the y axis */
  def ylabel(text : String)(implicit xyplot : XYPlot) {
    xyplot.yaxis.setLabel(text);
  }
  
  /**
   * Sets tooltips for the last series plotted (assumes x,y series).
   * NB: This is not standard Matlab.
   */
  def tooltips(tips : Seq[String])(implicit xyplot : XYPlot) {
    xyplot.plot.getRenderer(xyplot.series).setBaseToolTipGenerator(Tooltips(tips))
  }
  
  /** Plots a histogram of the given data into 10 equally spaced bins */
  def hist[I](data : PartialMap[I,Double])(implicit xyplot : XYPlot) : Unit = {
    hist(data, 10)(xyplot)
  }
  
  
  /** Plots a histogram of the given data into the given number of bins */
  def hist[I](data : PartialMap[I,Double], nbins : Int)(implicit xyplot : XYPlot) : Unit = {
    hist(data, linspace(min(data),max(data),nbins))(xyplot)
  }
  
  /**
   * Plots a histogram of the given data into bins with centers at the given
   * positions.
   */
  def hist[I](data : PartialMap[I,Double], bins : Vector)(implicit xyplot : XYPlot) : Unit = {
    def bucket(point : Double, lower : Int, upper : Int) : Int = {
      val mid = (lower + upper) / 2;
      if (lower == upper) {
        return upper;
      } else if (point <= bins(mid)) {
        return bucket(point, lower, mid);
      } else {
        return bucket(point, mid+1, upper);
      }
    }
    
    val counts = new DenseVector(bins.size);
    for (point <- data.valuesIterator) {
      val bin = bucket(point, 0, bins.size-1);
      counts(bin) += 1;
    }
    
    // smallest gap between bins
    val width = { bins.valuesIterator zip (bins.valuesIterator drop 1) map
      (pair => Math.abs(pair._2 - pair._1)) reduceLeft Math.min };
    
    val dataset = new org.jfree.data.xy.XYBarDataset(
      Dataset(bins, counts), width);
    val series = xyplot.nextSeries;
    xyplot.plot.setDataset(series,dataset);
    xyplot.plot.setRenderer(series,new org.jfree.chart.renderer.xy.XYBarRenderer);
    xyplot.refresh();
  }

  private def manifestOf[T](implicit manifest: ClassManifest[T]) = manifest;
  
  /** Plots the given y versus 1 to y.size as x with line drawn */
  def plot(y : Seq[Double])(implicit xyplot : XYPlot) : Unit = {
    plot(Vector(y :_*))(xyplot);
  }
  
  /** Plots the given y versus 1 to y.size as x with line drawn */
  def plot(y : Vector)(implicit xyplot : PlottingSupport.XYPlot) : Unit = {
    plot(new DenseVector(Array.tabulate(y.size)(i => i+1.0)), y)(xyplot,manifestOf[Int]);
  }
  
  /** Plots the given y versus the given x with line drawn */
  def plot[I](x : PartialMap[I,Double], y : PartialMap[I,Double])(implicit xyplot : PlottingSupport.XYPlot, man: ClassManifest[I]) : Unit = {
    plot(x,y,'-')(xyplot,man);
  }
  
  /** Plots the given y versus the given x with the given style */
  def plot[I](x : PartialMap[I,Double], y : PartialMap[I,Double], style : Char)(implicit xyplot : PlottingSupport.XYPlot, man: ClassManifest[I]) : Unit = {
    lazy val shapeDot = new java.awt.geom.Ellipse2D.Double(0,0,2,2);
    lazy val shapePlus = {
      val shape = new java.awt.geom.GeneralPath();
      shape.moveTo(-3,0);
      shape.lineTo(3,0);
      shape.moveTo(0,-3);
      shape.lineTo(0,3);
      shape;
    };
  
    
    // initialize dataset and series
    val dataset = Dataset(x,y);
    val series = xyplot.nextSeries;
    
    xyplot.plot.setDataset(series, dataset);
    
    // set the renderer
    val renderer = new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer();
    renderer.setPaint(xyplot.color(series));
    
    style match {
    case '-' => {
        renderer.setLinesVisible(true);
        renderer.setShapesVisible(false);
      }
    case '.' => {
        renderer.setLinesVisible(false);
        renderer.setShapesVisible(true);
        renderer.setShape(shapeDot);
      }
    case '+' => {
        renderer.setLinesVisible(false);
        renderer.setShapesVisible(true);
        renderer.setShape(shapePlus);
      }
    case _ => { }
    }
    
    xyplot.plot.setRenderer(series, renderer);
    xyplot.refresh();
  }
  
  /**
   * Displays a scatter plot of x versus y, each point drawn at the given
   * size and mapped with the given color.
   */
  def scatter(x : Vector, y : Vector, s : Vector, c : Vector)(implicit xyplot : XYPlot) {
    assert(x.size == y.size);
    assert(y.size == s.size, y.size + " != " + s.size);
    assert(s.size == c.size, s.size + " != " + c.size);
    
    val dataset = Dataset(x,y,s,c);
    val series = xyplot.nextSeries;
    xyplot.plot.setDataset(series, dataset);
    
    val gradient = Gradients.GRADIENT_BLUE_TO_RED;
    
    val paintscale = new org.jfree.chart.renderer.PaintScale {
      override def getLowerBound = 0.0;
      override def getUpperBound = 1.0;
      override def getPaint(value : Double) = {
        val index = gradient.length * (value - getLowerBound) / (getUpperBound - getLowerBound);
        gradient(Math.min(gradient.length-1, Math.max(0, index.toInt)));
      }
    }
    
    // set the renderer
    import java.awt.Graphics2D
    import java.awt.geom.Rectangle2D
    import org.jfree.data.xy.XYDataset
    import org.jfree.ui.RectangleEdge
    import org.jfree.chart.axis.ValueAxis
    import org.jfree.chart.renderer.xy.AbstractXYItemRenderer
    import org.jfree.chart.renderer.xy.XYBubbleRenderer
    import org.jfree.chart.renderer.xy.XYItemRendererState
    
    val renderer = new XYBubbleRenderer(XYBubbleRenderer.SCALE_ON_DOMAIN_AXIS) {;
      val stroke = new java.awt.BasicStroke(0f);
      override def getItemPaint(series : Int, item : Int) : java.awt.Paint = {
        paintscale.getPaint(c(item));
      }
      override def getItemStroke(series : Int, item : Int) = stroke;
    }
    
    xyplot.plot.setRenderer(series, renderer);
    xyplot.refresh;
  }
  
  // TODO: reinstate
  /*
  def scatter(x : Vector, y : Vector, s : Double, c : Vector)(implicit xyplot : Plotting.XYPlot) {
    scatter(x,y,ones(x.size)*s,c)(xyplot);
  }
  
  def scatter(x : Vector, y : Vector, s : Vector, c : Double)(implicit xyplot : Plotting.XYPlot) {
    scatter(x,y,s,ones(x.size)*c)(xyplot);
  }
  */
  
  /** An XY stacked area chart. */
  def stacked(x : Vector, y : Seq[Vector], names : Seq[String])(implicit xyplot : XYPlot) {
    import org.jfree.data.xy.{AbstractXYDataset, TableXYDataset};
    val dataset = new AbstractXYDataset with TableXYDataset {
      override def getX(series : Int, item : Int) = x(item);
      override def getY(series : Int, item : Int) = y(series)(item);
      override def getSeriesKey(series : Int) = names(series);
      override def getSeriesCount = y.size;
      override def getItemCount = x.size;
      override def getItemCount(series : Int) = y(series).size;
    };
    
    import org.jfree.chart.renderer.xy.StackedXYAreaRenderer2;
    
    val renderer = new StackedXYAreaRenderer2(null,null);
    renderer.setOutline(true);

    // add dataset and renderer
    val series = xyplot.nextSeries;
    xyplot.plot.setDataset(series,dataset);
    xyplot.plot.setRenderer(series, renderer);
    xyplot.refresh;
  }
  
  /** An XY stacked area chart with default names. */
  def stacked(x : Vector, y : Seq[Vector])(implicit xyplot : PlottingSupport.XYPlot) {
    stacked(x, y, y.iterator.zipWithIndex.map(_._2.toString).toSeq)(xyplot);
  }
  
  /**
   * Displays an image in the current figure, where each cell in the matrix provides
   * color for one square of the image.
   * 
   * @param x The range on the x axis for drawing the image.  If x._1 < x._2, the matrix column numbers are inverted.
   * @param y The range on the y axis for drawing the image.  If y._1 < y._2, the matrix row numbers are inverted.
   */
  private def image(x : (Double,Double), y : (Double,Double), c : Matrix, lower : Double, upper : Double)(implicit xyplot : XYPlot) {
    import org.jfree.chart.axis.NumberAxis;
    
    val (minX,maxX) = (Math.min(x._1,x._2),Math.max(x._1,x._2));
    val (minY,maxY) = (Math.min(y._1,y._2),Math.max(y._1,y._2));
    
    val dataset = Dataset(c, x, y);
    val series = xyplot.nextSeries;
    
    xyplot.plot.setDataset(series, dataset);
    
    // initialize paint scale
    val gradient = Gradients.GRADIENT_BLUE_TO_RED;
    val paintscale = new org.jfree.chart.renderer.PaintScale {
      override def getLowerBound = lower;
      override def getUpperBound = upper;
      override def getPaint(value : Double) = {
        val index = gradient.length * (value - getLowerBound) / (getUpperBound - getLowerBound);
        gradient(Math.min(gradient.length-1, Math.max(0, index.toInt)));
      }
    }
    
    // initialize renderer
    import org.jfree.chart.renderer.xy.XYBlockRenderer
    val renderer = new XYBlockRenderer();
    renderer.setPaintScale(paintscale);
    renderer.setBlockAnchor(org.jfree.ui.RectangleAnchor.BOTTOM_LEFT);
    renderer.setBlockWidth((maxX - minX) / c.cols);
    renderer.setBlockHeight((maxY - minY) / c.rows);
    
    xyplot.plot.getRangeAxis.setInverted(true);
    xyplot.plot.getRangeAxis.setLowerBound(minY);
    xyplot.plot.getRangeAxis.setUpperBound(maxY);
    if (maxY - minY == c.rows && minY == minY.intValue) {
      xyplot.plot.getRangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
    }
    xyplot.plot.getDomainAxis.setLowerBound(Math.min(x._1,x._2));
    xyplot.plot.getDomainAxis.setUpperBound(Math.max(x._1,x._2));
    if (maxX - minX == c.cols && minX == minX.intValue) {
      xyplot.plot.getDomainAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
    }
    xyplot.plot.setRenderer(series, renderer);
    xyplot.refresh();
  }
  
  /** Plots the given matrix as an image. */
  def image(c : Matrix)(implicit xyplot : XYPlot) {
    image((0,c.cols),(0,c.rows),c,0.0,1.0)(xyplot);
  }
  
  /** Plots the given matrix as an image. */
  def image(x : (Double,Double), y : (Double,Double), c : Matrix)(implicit xyplot : XYPlot) {
    image(x,y,c,0.0,1.0)(xyplot);
  }
  
  /** Plots the given matrix as an image. */
  def imagesc(c : Matrix)(implicit xyplot : XYPlot) {
    image((0,c.cols),(0,c.rows),c,min(c),max(c))(xyplot);
  }
  
  /** Plots the given matrix as an image. */
  def imagesc(x : (Double,Double), y : (Double,Double), c : Matrix)(implicit xyplot : XYPlot) {
    image(x, y, c, min(c), max(c))(xyplot);
  }
  
  /** Sets the lower and upper bounds of the current plot. */
  def xlim(xmin : Double, xmax : Double)(implicit xyplot : XYPlot) {
    xyplot.plot.getDomainAxis.setLowerBound(xmin);
    xyplot.plot.getDomainAxis.setUpperBound(xmax);
  }
  
  /** Sets the lower and upper bounds of the current plot. */
  def ylim(ymin : Double, ymax : Double)(implicit xyplot : XYPlot) {
    xyplot.plot.getRangeAxis.setLowerBound(ymin);
    xyplot.plot.getRangeAxis.setUpperBound(ymax);
  }
  
  /** For re-plotting to same figure */
  def hold(state : Boolean)(implicit figures : Figures) : Unit = {
    val xyplot = figures.figure.plot;
    xyplot.hold = state;
  }
  
}


/** Plotting operations */
object PlottingSupport {
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
  
  /** Returns a JFreeChart XYDataset for plotting y vs x. */
  def Dataset[I:ClassManifest](x : PartialMap[I,Double], y : PartialMap[I,Double]) : XYDataset = {
    if (x.activeDomain != y.activeDomain)
      throw new IllegalArgumentException("Active domains do not match");
    val domain = x.activeDomain.toArray;
    new org.jfree.data.xy.AbstractXYDataset() {
      override def getX(series : Int, item : Int) : Number = {
        if (series == 0) { java.lang.Double.valueOf(x(domain(item))) } else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) { java.lang.Double.valueOf(y(domain(item))) } else null
      }
      override def getItemCount(series : Int) = domain.size;
      override def getSeriesKey(series : Int) = "1"
      override def getSeriesCount() = 1
    }
  }
  
  /** Returns a JFreeChart XYZDataset for plotting all iterator of the matrix */
  def Dataset(m : Matrix) : XYZDataset = {
    Dataset(m, (0., 0.+m.cols), (0., 0.+m.rows));
  }
  
  def Dataset(m : Matrix, x : (Double,Double), y : (Double,Double)) : XYZDataset = {
    val (minX,maxX) = (Math.min(x._1,x._2),Math.max(x._1,x._2));
    val (minY,maxY) = (Math.min(y._1,y._2),Math.max(y._1,y._2));
    
    val scaleX = (maxX - minX) / m.cols;
    val scaleY = (maxY - minY) / m.rows;
    
    val invertX = x._1 > x._2;
    val invertY = y._1 > y._2;
    
    def getCol(item : Int) = (item - (item % m.rows)) / m.rows;
    def getRow(item : Int) = item % m.rows;
    
    new org.jfree.data.xy.AbstractXYZDataset() {
      override def getX(series : Int, item : Int) : Number = {
        if (series != 0) return null;
        (if (invertX) (m.cols - getCol(item) - 1) else (getCol(item))) * scaleX + minX;
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series != 0) return null;
        (if (invertY) (m.rows - getRow(item) - 1) else (getRow(item))) * scaleY + minY;
      }
      override def getZ(series : Int, item : Int) : Number = {
        if (series != 0) return null;
        m(getRow(item), getCol(item));
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
        if (series == 0) x(item) else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) y(item) else null
      }
      override def getZ(series : Int, item : Int) : Number = {
        if (series == 0) z(item) else null
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
        if (series == 0) x(item) else null
      }
      override def getY(series : Int, item : Int) : Number = {
        if (series == 0) y(item) else null
      }
      override def getZ(series : Int, item : Int) : Number = {
        if (series == 0) z(item) else null
      }
      override def getW(series : Int, item : Int) : Number = {
        if (series == 0) w(item) else null
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
  
  /** A Figure holds a collection of XYPlot instances */
  class Figure(figures : Figures) {
    /** List of plots in the figure */
    private val plots = ArrayBuffer[XYPlot]();

    /** Clears the current plot */
    def clear() {
      plots.clear();
      plot = 0;
      rows = 1;
      cols = 1;
    }
    
    /** The Swing frame for this plot */
    lazy val frame : JFrame = {
      val f = new JFrame("Figure "+(figures.number(this)+1));
      f.setSize(600,400);
      f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
      f.setLayout(new java.awt.GridLayout(cols,rows));
      f
    }
    
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
    
    def number(plot : XYPlot) : Int = plots.indexOf(plot);
    
    private var plot_ = 0;
    def plot : XYPlot = plots(plot_);
    def plot_=(number : Int) : Unit = {
      assert(number >= 0);
      while (plots.length <= number) {
        plots += null;
      }
      if (plots(number) == null) {
        plots(number) = new XYPlot(this);
      }
      plot_ = number;
    }
    
    /** Redraws the figure */
    def refresh() : Unit = {
      frame.getContentPane.removeAll;
      frame.getContentPane.setLayout(new java.awt.GridLayout(rows,cols));
      for (plot <- plots) {
        frame.getContentPane.add(if (plot == null) new JPanel() else plot.panel);
      }
      frame.repaint();
      frame.setVisible(true);
    }
    
    // create the initial plot (don't call clear lest we pop up a window)
    plot = 0;
    
    // export the plot
    def writeEPS(out : java.io.OutputStream, dpi : Int) {
      // default dpi is 72
      val scale = dpi / 72.0;
      val width  = (frame.getContentPane.getSize.width  * scale).toInt;
      val height = (frame.getContentPane.getSize.height * scale).toInt;

      // an attempt at getting the renderer to do high-res correctly. failed.
      /*
      for (plot <- plots) {
        import java.awt.RenderingHints._;
        val hints = new java.awt.RenderingHints(KEY_DITHERING, VALUE_DITHER_DISABLE);
        for (pair <- List((KEY_ANTIALIASING, VALUE_ANTIALIAS_OFF),
                          (KEY_STROKE_CONTROL, VALUE_STROKE_PURE),
                          (KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_OFF))) {
          hints.put(pair._1,pair._2);
        }
        plot.chart.setRenderingHints(hints);
      }
      */
      
      // create an eps document at the appropriate dpi
      import org.apache.xmlgraphics.java2d.ps.EPSDocumentGraphics2D;
      val g2d = new EPSDocumentGraphics2D(false);
      g2d.setGraphicContext(new org.apache.xmlgraphics.java2d.GraphicContext);
      g2d.setupDocument(out, width, height);
      g2d.scale(scale, scale);
      frame.getContentPane.paintAll(g2d);
      // plot.chart.draw(g2d, new java.awt.geom.Rectangle2D.Double(0,0,width,height));
      g2d.finish();
    }
    
    def writePNG(out : java.io.OutputStream, dpi : Int) {
      // default dpi is 72
      val scale = dpi / 72.0;
      val width  = (frame.getContentPane.getSize.width  * scale).toInt;
      val height = (frame.getContentPane.getSize.height * scale).toInt;
      
      import java.awt.image.BufferedImage;
      val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB);
      val g2d = image.createGraphics();
      g2d.scale(scale, scale);
      frame.getContentPane.paintAll(g2d);
      g2d.dispose;
      
      javax.imageio.ImageIO.write(image, "png", out);
      
      //org.jfree.chart.ChartUtilities.writeChartAsPNG(
      //  out, plot.chart, frame.getSize.width, frame.getSize.height);
    }

    /** Contributed by Robby McKilliam */
    def writePDF(out : java.io.OutputStream) {
      import com.lowagie.text.Document;
      import com.lowagie.text.DocumentException;
      import com.lowagie.text.Paragraph;
      import com.lowagie.text.Rectangle;
      import com.lowagie.text.pdf.PdfContentByte;
      import com.lowagie.text.pdf.PdfImportedPage;
      import com.lowagie.text.pdf.PdfReader;
      import com.lowagie.text.pdf.PdfTemplate;
      import com.lowagie.text.pdf.PdfWriter;

      val width  = (frame.getContentPane.getSize.width).toInt;
      val height = (frame.getContentPane.getSize.height).toInt;

      // step 1: creation of a document-object
      val document = new Document();

      try {
        document.setPageSize(new Rectangle(width, height));

        // step 2: creation of the writer
        val writer = PdfWriter.getInstance(document, out);
        // step 3: we open the document
        document.open();

        val cb = writer.getDirectContent();

        val tp = cb.createTemplate(width, height);
        val g2d = tp.createGraphics(width, height);

        val plotwidth = width/cols;
        val plotheight = height/rows;
        var px = 0; var py = 0;
        for(plot <- plots) {
          if (plot != null) {
            plot.chart.draw(g2d, new java.awt.Rectangle(px*plotwidth, py*plotheight, plotwidth, plotheight));
          }
          px = (px +1)%cols;
          if(px == 0) py = (py + 1)%rows;
        }
        //frame.getContentPane.paintAll(g2d)
        g2d.dispose;

        cb.addTemplate(tp, 1, 0, 0, 1, 0, 0);
      }

      document.close();
    }    
  }
  
  object XYPlot {
    import java.awt.Color;
    import java.awt.Paint;
    import org.jfree.chart.ChartColor._;
    
    // color cycle ignoring bright colors
    val colors = Array[Paint](
       VERY_DARK_RED, VERY_DARK_BLUE, VERY_DARK_GREEN, VERY_DARK_YELLOW, VERY_DARK_MAGENTA, VERY_DARK_CYAN,
       Color.darkGray,
       DARK_RED, DARK_BLUE, DARK_GREEN, DARK_MAGENTA, DARK_CYAN
    );
  }
  
  /** A two dimensional XY plot */
  class XYPlot(figure : Figure) {
    import org.jfree.chart.plot.DefaultDrawingSupplier;
    
    val plot  = new org.jfree.chart.plot.XYPlot()
    val xaxis : NumberAxis = new NumberAxis(null)
    val yaxis : NumberAxis = new NumberAxis(null)

    /** Adds to the current plot if true, else replaces */
    var hold : Boolean = false
    
    Array(xaxis,yaxis) foreach (axis => axis.setAutoRangeIncludesZero(false))
    plot.setDomainAxis(xaxis)
    plot.setRangeAxis(yaxis)
    
    plot.setDrawingSupplier(new DefaultDrawingSupplier(
      XYPlot.colors,
      DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_STROKE_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE));
    
    /** The plot title */
    // private var title_ : String = ""
    def title_=(str : String) : Unit = {
      chart.setTitle(str);
      //title_ = str
      //refresh()
    }
    def title : String = chart.getTitle.getText;

    /** If we show a legend */
    private var legend_ : Boolean = false
    def legend_=(show : Boolean) : Unit = {
      if (show != legend_) {
        chart.removeLegend();
        if (show) {
          import org.jfree.chart.title._
          import org.jfree.ui._
          import org.jfree.chart.block._
          import java.awt.Color;
          val legend = new LegendTitle(this.plot);
          legend.setMargin(new RectangleInsets(1.0, 1.0, 1.0, 1.0));
          legend.setFrame(new LineBorder());
          legend.setBackgroundPaint(Color.white);
          legend.setPosition(RectangleEdge.BOTTOM);
          chart.addSubtitle(legend);
        }
      }
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
          plot.setDataset(i,null);
        }
      }
      return series_
    }

    def color(series : Int) : java.awt.Paint = {
      XYPlot.colors(series % XYPlot.colors.length);
    } 
    
    /** The JFreeChart for this plot */
    lazy val chart =
      new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false);
    
    /** The ChartPanel for this plot */
    lazy val panel = new ChartPanel(chart);
    
    /** Shows the given chart */
    def refresh() = figure.refresh()
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
}

/**
 * An object with access to the Plotting trait members.
 * 
 * @author dramage
 */
object Plotting extends Plotting { }
