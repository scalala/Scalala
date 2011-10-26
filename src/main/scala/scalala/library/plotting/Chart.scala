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

import java.awt.{Graphics2D,Shape,RenderingHints};
import java.awt.{Paint,Color,Stroke,BasicStroke,Font};
import java.awt.font.{TextLayout};
import java.awt.geom.{AffineTransform,Point2D,Line2D,Ellipse2D,Rectangle2D,Arc2D,Path2D};
import javax.swing.{JFrame,JScrollPane,WindowConstants};
import java.io.File;

/**
 * Represents an optional value from D to V.  Whenever an Attribute is
 * required, you may either provide a literal value of V, a function
 * from D to V, or a partial function (such as from a case statement
 * or a scala Map) from D to V.  These conversions are provided by the
 * Attribute companion object and do not need to be imported to be in
 * scope.
 *
 * @author dramage 
 */
trait Attribute[-D,+V] extends PartialFunction[D,V];

/**
 * Companion object for Attribute providing implicit conversions from
 * constants, functions, and partial functions.
 *
 * @author dramage
 */
object Attribute {
  implicit def constant[V](const : V) : Attribute[Any,V]
  = new Attribute[Any,V] {
    override def isDefinedAt(item : Any) = true;
    override def apply(item : Any) = const;
  }
  
  implicit def function[D,V](f : (D=>V)) : Attribute[D,V]
  = new Attribute[D,V] {
    override def isDefinedAt(item : D) = true;
    override def apply(item : D) = f(item);
  }
  
  implicit def partial[D,V](f : PartialFunction[D,V]) : Attribute[D,V]
  = new Attribute[D,V] {
    override def isDefinedAt(item : D) = f.isDefinedAt(item);
    override def apply(item : D) = f(item);
  }
}

class Chart {
  val plotters = new scala.collection.mutable.ArrayBuffer[Plotter]();

  private var _width : ()=>Int = ()=>600;
  def width : Int = _width();
  def width_=(w : =>Int) = _width = ()=>w;
  
  private var _height : ()=>Int = ()=>600;
  def height : Int = _height();
  def height_=(h : =>Int) = _height = ()=>h;

  def show(title : String = "Scalala", panel : ChartPanel = new ChartPanel(this, width, height)) : JFrame = {
    val frame = new JFrame(title);
    frame.add(new JScrollPane(panel));
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    frame.pack();
    frame.setVisible(true);
    frame;
  }
  
  def saveas(path : File, dpi : Int = 72) : Unit =
    ExportGraphics.writeFile(path, this.render, width=width, height=height, dpi=dpi);
  
  def add(plotters : Plotter*) : Chart =
    this.addAll(plotters);
  
  def addAll(plotters : Traversable[Plotter]) : Chart = {
    this.plotters ++= plotters;
    this;
  }
  
  def render(g : Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                       RenderingHints.VALUE_ANTIALIAS_ON);
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                       RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    
		for (plotter <- plotters) {
		  plotter.render(g);
		}
  }
}

object Chart {
  def apply(width : =>Int, height : =>Int) = {
    var chart = new Chart();
    chart.width = width;
    chart.height = height;
    chart;
  }
}

trait Plotter {
  def render(g : Graphics2D);
}

object Plotter {
  val defaultLinePaint = Color.DARK_GRAY;
  val defaultFillPaint = Color.LIGHT_GRAY;
  val defaultStroke = new BasicStroke(1,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
  val defaultLabelFont = new Font(Font.SANS_SERIF, Font.PLAIN, 12);
  val defaultLabelColor = Color.BLACK;
}

/**
 * Draws a shape for every data item in the given set of data.
 */
case class Shapes[D](
  data : Iterable[D],
  x : D=>Double, y : D=>Double,
  shape : Attribute[D,Shape],
  stroke : Attribute[D,Paint],
  fillPaint : Attribute[D,Paint]
) extends Plotter {
  def render(g : Graphics2D) = {
    val originalPaint = g.getPaint;
    for (item <- data) {
      var ix = x(item);
      var iy = y(item);
      val ishape = shape(item);
      g.setPaint(fillPaint(item));
      g.fill(ishape);
      g.setPaint(stroke(item));
      g.draw(ishape);
    }
    g.setPaint(originalPaint);
  }
}

/**
 * A vertical line drawn with a particular stroke.
 *
 * @author dramage
 */
case class VRule(left : Double, top : Double, height : Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke : Stroke = Plotter.defaultStroke)
extends Plotter {
  val line2d = new Line2D.Double(left, top, left, top + height)
  def render(g : Graphics2D) = {
    val originalPaint  = g.getPaint;
    val originalStroke = g.getStroke;
    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    g.draw(line2d);
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * A horizontal line drawn with a particular stroke.
 *
 * @author dramage
 */
case class HRule(left : Double, top : Double, width : Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke  : Stroke = Plotter.defaultStroke)
extends Plotter {
  val line2d = new Line2D.Double(left, top, left + width, top)
  def render(g : Graphics2D) = {
    val originalPaint  = g.getPaint;
    val originalStroke = g.getStroke;
    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    g.draw(line2d);
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}


case class Dots[D](
  data : Iterable[D],
  x : D=>Double, y : D=>Double,
  r : Attribute[D,Double],
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke  : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Plotter {
  def render(g : Graphics2D) = {
    val shape = new Ellipse2D.Double();
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      val ir = r(item);
      val ix = x(item) - ir;
      val iy = y(item) - ir;
      val id = 2 * ir;

      shape.setFrame(ix,iy,id,id);
      
      g.setStroke(lineStroke(item));
      g.setPaint(fillPaint(item));
      g.fill(shape);
      g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }

  /** Returns the displayed positions of all data items with respect to the requested anchor. */
  def anchors(anchor : Anchor) : Iterable[(Double,Double)] = {
    for (item <- data) yield {
      var ix = x(item);
      var iy = y(item);
      val ir = r(item);
      if (anchor.isTop) {
        iy -= ir;
      } else if (anchor.isBottom) {
        iy += ir;
      }
      
      if (anchor.isLeft) {
        ix -= ir;
      } else if (anchor.isBottom) {
        ix += ir;
      }
      
      (ix,iy);
    }
  }
}

case class Bars[D](
  data : Iterable[D],
  x : D=>Double, y : D=>Double,
  width : Attribute[D,Double],
  height : Attribute[D,Double],
  anchor : Attribute[D,Anchor],
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke  : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Plotter {
  def render(g : Graphics2D) = {
    val shape = new Rectangle2D.Double();
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      var ix = x(item);
      var iy = y(item);
      val iwidth = width(item);
      val iheight = height(item);
      val ianchor = anchor(item);
      
      if (ianchor.isCenter) {
        ix -= iwidth / 2.0;
      } else if (ianchor.isRight) {
        ix -= iwidth;
      }
      
      if (ianchor.isMiddle) {
        iy -= iheight / 2.0;
      } else if (ianchor.isBottom) {
        iy -= iheight;
      }

      shape.setFrame(ix,iy,iwidth,iheight);

      g.setStroke(lineStroke(item));
      g.setPaint(fillPaint(item));
      g.fill(shape);
      g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

case class Wedges[D](
  data : Iterable[D],
  x : Attribute[D,Double], y : Attribute[D,Double],
  startAngle : Attribute[D,Double], angle : Attribute[D,Double],
  innerRadius : Attribute[D,Double], outerRadius : Attribute[D,Double],
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke  : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Plotter {
  def render(g : Graphics2D) = {
    val arc1 = new Arc2D.Double();
    val arc2 = new Arc2D.Double()
    val shape = new Path2D.Double();
    
    def prepare(x : Double, y : Double, innerRadius : Double, outerRadius : Double, start : Double, angle : Double) = {
      shape.reset();
      val startDegrees = start*180/math.Pi;
      val angleDegrees = angle*180/math.Pi;
      arc1.setArcByCenter(x,y,innerRadius,startDegrees,angleDegrees,Arc2D.OPEN);
      arc2.setArcByCenter(x,y,outerRadius,startDegrees+angleDegrees,-angleDegrees,Arc2D.OPEN);
      shape.append(arc1,false);
      shape.append(arc2,true);
      shape.closePath();
    }
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      prepare(x(item),y(item),innerRadius(item),outerRadius(item),startAngle(item),angle(item));
      g.setStroke(lineStroke(item));
      g.setPaint(fillPaint(item));
      g.fill(shape);
      g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * Draws a line connecting consecutive items in the given data using
 * the given linePaint and lineStroke.
 *
 * @author dramage
 */
case class Line[D](data : Iterable[D],
  x : D=>Double, y : D=>Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke  : Stroke = Plotter.defaultStroke
) extends Plotter {
  def render(g : Graphics2D) = {
    val shape = new Line2D.Double();
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    for ((i1,i2) <- data.iterator zip data.iterator.drop(1)) {
      shape.setLine(x(i1),y(i1),x(i2),y(i2));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * Renders the text for each data items at the given x, y
 * positions.  The text is rendered relative to the given
 * anchor, i.e. so that the x,y position given is at the
 * BottomLeft of the rendered text, TopRight, etc.  For
 * a left or right anchor, adds hpadding pixels between the
 * text and the x coordinate.  For a top or bottom anchor,
 * adds vpadding pixels between the text and the y coordinate.
 * Both padding values default to 4.  The text is rendered at
 * the given angle.
 *
 * @author dramage
 */
case class Labels[D](data : Iterable[D],
  text : D=>String,
  x : D=>Double, y : D=>Double,
  anchor : Attribute[D,Anchor],
  angle : Attribute[D,Double] = 0.0,
  color : Attribute[D,Color] = Plotter.defaultLabelColor,
  font : Font = Plotter.defaultLabelFont,
  hoffset : Double = 0,
  voffset : Double = 0
) extends Plotter {
  def render(g : Graphics2D) = {
    val originalColor = g.getColor;
    
    val originalFont = g.getFont;
    g.setFont(font);
    val frc = g.getFontRenderContext();

    val originalTransform = g.getTransform();
    val transform = new AffineTransform();
    
    for (item <- data) {
      val itext = text(item);
      var ix = x(item);
      var iy = y(item);
      val ianchor = anchor(item);
      val iangle = angle(item);

      val layout = new TextLayout(itext, font, frc);
      val width = layout.getBounds.getWidth;
      val height = layout.getAscent;

      val pivotx =
        if (ianchor.isLeft) 0.0
        else if (ianchor.isCenter) - width / 2.0
        else /* if (ianchor.isRight) */ - width.toDouble;
      
      val pivoty =
        if (ianchor.isTop) layout.getAscent.toDouble;
        else if (ianchor.isMiddle) layout.getAscent / 2.0;
        else /* if (ianchor.isBottom) */ 0.0;
      
      transform.setToIdentity();
      transform.translate(ix+hoffset,iy+voffset);
      transform.rotate(iangle);
      transform.translate(pivotx,pivoty);
      g.transform(transform);

      g.setColor(color(item));
      layout.draw(g, 0f, 0f);
      
      g.setTransform(originalTransform);
    }
    
    g.setFont(originalFont);
    g.setColor(originalColor);
  }
}

/** Assigns values of type V to a position on an axis. */
trait Scale[@specialized I] extends (I=>Double) {
  /** The minimum value in the output scale. */
  def pxMin : Double;
  
  /** The maximum value in the output scale. */
  def pxMax : Double;
  
  /** The length of the output scale. */
  final def pxLength : Double = pxMax - pxMin;
  
  /** Returns the pixel coordinate of the given item. */
  def apply(item : I) : Double;
  
  require(pxMin < pxMax, "pxMin must be less than pxMax");
}

/**
 * Constructs a scasle for the given ordinal items, evenly dividing
 * the range pxMin to pxMax into items.size bins.  The width allocated
 * to each item within its bin is controlled by the band parameter.
 *
 * @param band Determines the width of each band in the scale.  Normally,
 *   this number is a percentage between 0 and 1 reflecting the fraction
 *   of the segment allocated to the band.  If negative, band is interpreted
 *   as a literal number of pixels as a margin between successive elements.
 *
 * @author dramage
 */
case class OrdinalScale[I](
  items : IndexedSeq[I],
  override val pxMin : Double, override val pxMax : Double,
  val band : Double = 0.8
) extends Scale[I] {

  /** The width of each band in pixels. */
  val pxBand = {
    if (band < 0)
      (pxLength / items.size) + band
    else
      (pxLength / items.size) * band;
  }
  
  private val itemToIndex = items.zipWithIndex.toMap;
  
  def apply(item : I) =
    pxMin + (itemToIndex(item) + 0.5) * (pxLength / items.size)
    
  def horizontal(top : Double,
    angle : Attribute[I,Double] = 0.0, anchor : Attribute[I,Anchor] = Anchor.TopCenter,
    labelText : (I=>String) = (item : I) => item.toString,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[I,Color] = Plotter.defaultLabelColor,
    hoffset : Double = 0.0,
    voffset : Double = 0.0,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new {
    val baseline =
      HRule(left = pxMin, width = pxLength, top = top, linePaint = linePaint, lineStroke = lineStroke);
      
    val labels =
      Labels(data = items,
        x = (v : I) => apply(v),
        y = (v : I) => top,
        text = labelText,
        angle = angle,
        anchor = anchor,
        hoffset = hoffset,
        voffset = voffset,
        font = labelFont,
        color = labelColor
      );
  } with Plotter {
    def render(g : Graphics2D) {
      baseline.render(g);
      labels.render(g);
    }
  }
  
  def vertical(left : Double,
    angle : Attribute[I,Double] = 0.0, anchor : Attribute[I,Anchor] = Anchor.MiddleRight,
    labelText : (I=>String) = (item : I) => item.toString,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[I,Color] = Plotter.defaultLabelColor,
    hoffset : Double = 0.0,
    voffset : Double = 0.0,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new {
    val baseline =
      VRule(left = left, top = pxMin, height = pxLength, linePaint = linePaint, lineStroke = lineStroke);
      
    val labels =
      Labels(data = items,
        x = (v : I) => left,
        y = (v : I) => apply(v),
        text = labelText,
        angle = angle,
        anchor = anchor,
        hoffset = hoffset,
        voffset = voffset,
        font = labelFont,
        color = labelColor
      );
  } with Plotter {
    def render(g : Graphics2D) {
      baseline.render(g);
      labels.render(g);
    }
  }
}

/**
 * A numeric scale mapping numbers of type I to a position on an axis.
 *
 * @author dramage
 */
trait NumericScale extends Scale[Double] {
  /** The minimum value. */
  def min : Double;
  
  /** The maximum value. */
  def max : Double;
  
  /** Maps from a pixel value back to the underlying numeric scale. */
  def unapply(pxValue : Double) : Double;
  
  /** Returns the distance in pixels of the pixel coordinate from the min */
  def extent(value : Double) =
    (apply(value) - apply(math.min(min,max))).abs;
  
  /** Default formatter. */
  def format(segments : Int) : (Double => String) =
    NumericScale.fudge(min, max, segments);
  
  /** 
   * Returns the tick positions (in domain space) of this axis for n segments.
   * n + 1 tick positions are returned.
   */
  def domainTicks(n : Int) : Array[Double] = {
    val smaller = math.min(min,max);
    val larger = math.max(min,max);
    val delta = (larger - smaller) / n;
    Array.tabulate(n + 1)(i => smaller + i * delta);
  }
  
  /** 
   * Returns the tick positions (in range, or pixel space) of this axis for n
   * segments.  n + 1 tick positions are returned.
   */
  def rangeTicks(n : Int) : Array[Double] =
    domainTicks(n).map(this.apply);
  
  def horizontal(top : Double, segments : Int = 10,
    labelText : (Double=>String) = null,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[Double,Color] = Plotter.defaultLabelColor,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new Plotter {
    private val _labelText = if (labelText == null) format(segments) else labelText;
  
    val _baseline =
      HRule(left = pxMin, width = pxLength, top = top, linePaint = linePaint, lineStroke = lineStroke);
      
    val _ticklines =
      rangeTicks(segments).map(tick => VRule(left = tick, top = top - 4, height = 8, linePaint = linePaint, lineStroke = lineStroke));
      
    val _labels =
      Labels(data = domainTicks(segments),
        x = (v : Double) => apply(v),
        y = (v : Double) => top,
        text = _labelText,
        voffset = 6.0,
        anchor = Anchor.TopCenter,
        font = labelFont,
        color = labelColor
      );
  
    def render(g : Graphics2D) {
      _baseline.render(g);
      _ticklines.foreach(_.render(g));
      _labels.render(g);
    }
  }
  
  def vertical(left : Double, segments : Int = 10,
    labelText : (Double=>String) = null,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[Double,Color] = Plotter.defaultLabelColor,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new Plotter {
    private val _labelText = if (labelText == null) format(segments) else labelText;
    
    val _baseline =
      VRule(left = left, top = pxMin, height = pxLength, linePaint = linePaint, lineStroke = lineStroke);
    
    val _ticklines =
      rangeTicks(segments).map(tick => HRule(left = left - 4, top = tick, width = 8, linePaint = linePaint, lineStroke = lineStroke));
      
    val _labels =
      Labels(data = domainTicks(segments),
        x = (v : Double) => left,
        y = (v : Double) => apply(v),
        text = _labelText,
        hoffset = -8.0,
        anchor = Anchor.MiddleRight,
        font = labelFont,
        color = labelColor
      );
  
    def render(g : Graphics2D) {
      _baseline.render(g);
      _ticklines.foreach(_.render(g));
      _labels.render(g);
    }
  }
}

object NumericScale {
  def scaleOf(x : Double) = ((x.abs-1).signum * (math.log(x.abs)/math.log(10)).abs.ceil);
  
  def floor(x : Double, decimal : Int) =
    (x * math.pow(10,decimal)).floor * math.pow(10,-decimal);
    
  def ceil(x : Double, decimal : Int) =
    (x * math.pow(10,decimal)).ceil * math.pow(10,-decimal);
    
  val maxAbsoluteError = java.lang.Double.MIN_VALUE * 1e3;
  val maxRelativeError = 1e-10;
  
  /** Based on http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm */
  def approxEq(a : Double, b : Double) : Boolean = {
    if (a == b || math.abs(a - b) < maxAbsoluteError) return true;
    
    val relativeError =
      if (math.abs(b) > math.abs(a)) math.abs((a - b) / b) else math.abs((a - b) / a);
    
    return relativeError <= maxRelativeError;
  }
  
  def fudge(min : Double, max : Double, segments : Int = 10) = new (Double=>String) {
    val decimal : Int = math.max(-scaleOf((max - min).abs / segments).toInt,0);
    val step = ceil((max - min).abs / segments, decimal);
    val betterMin = if (min < max) floor(min,decimal) else max + step * segments;
    val betterMax = if (min < max) min + step * segments else floor(max,decimal);
    val format = {
      // if fudged min and max are close enough
      if (approxEq(min, betterMin) && approxEq(max, betterMax))
        "%."+decimal+"f"
      else
        "%."+(decimal+1)+"f";
    };
    def apply(v : Double) = format.format(v);
  }
}

/**
 * A linear scale from min to max.
 *
 * @author dramage
 */
case class LinearScale(
  override val min : Double,   override val max : Double,
  override val pxMin : Double, override val pxMax : Double
) extends NumericScale {
  val scale = pxLength / (max - min);
  
  override def apply(value : Double) =
    pxMin + (value - min) * scale;

  override def unapply(px : Double) =
    min + (px - pxMin) / scale;
  
  /** Rounds the scale to something reasonable. */
  def rounded(segments : Int) = {
    val fudged = NumericScale.fudge(min, max, segments);
    LinearScale(fudged.betterMin, fudged.betterMax, pxMin, pxMax);
  }
}

/**
 * A logarithmic scale from min to max of the given base.
 *
 * @author dramage
 */
case class LogScale(
  override val min : Double, override val max : Double,
  override val pxMin : Double, override val pxMax : Double,
  val base : Double = 10.0
) extends NumericScale {
  private val logbase = math.log(base);

  val linear = LinearScale(math.log(min) / logbase, math.log(max) / logbase, pxMin, pxMax);
  
  override def apply(value : Double) =
    linear(math.log(value) / logbase);
  
  override def unapply(px : Double) =
    math.pow(base, linear.unapply(px));
}

sealed trait Anchor {
  val isLeft   : Boolean = getClass.getName.contains("Left");
  val isCenter : Boolean = getClass.getName.contains("Center");
  val isRight  : Boolean = getClass.getName.contains("Right");
  val isTop    : Boolean = getClass.getName.contains("Top");
  val isMiddle : Boolean = getClass.getName.contains("Middle");
  val isBottom : Boolean = getClass.getName.contains("Bottom");
}

object Anchor {
  object TopLeft extends Anchor
  object TopCenter extends Anchor
  object TopRight extends Anchor
  object MiddleLeft extends Anchor
  object MiddleCenter extends Anchor
  object MiddleRight extends Anchor
  object BottomLeft extends Anchor
  object BottomCenter extends Anchor
  object BottomRight extends Anchor
}

object ChartMain {
  def hist[K:Ordering](dist : Map[K,Int]) = {
    val xaxis = OrdinalScale(dist.keys.toList.sorted.toIndexedSeq,
      pxMin = 40, pxMax = 560);
    val yaxis = LinearScale(max = 0, min = dist.values.max,
      pxMin = 40, pxMax = 560);
    
    val bars = Bars[(K,Int)](dist,
      x = ((tup : (K,Int)) => tup._1) andThen xaxis,
      y = (tup : (K,Int)) => yaxis.pxMax,
      width = xaxis.pxBand,
      height = ((tup : (K,Int)) => tup._2.toDouble) andThen yaxis.extent,
      anchor = Anchor.BottomCenter);
      
    Chart(600, 600).
      add(bars,
          xaxis.horizontal(yaxis.pxMax, voffset=4),
          yaxis.vertical(xaxis.pxMin, dist.values.max)).
      show("Histogram");
  }

  def main(args : Array[String]) {
    val xaxis = LinearScale(min = 0, max = 10, pxMin = 40, pxMax = 560);
    val yaxis = LogScale(min = 100, max = 1, pxMin = 40, pxMax = 560);
    
    val data = Array.tabulate(21)(i => (i / 2.0, i * i / 4.0));
    
    val dots : Dots[(Double,Double)] = Dots(data,
      x = ((t : (Double,Double)) => t._1) andThen xaxis,
      y = ((t : (Double,Double)) => t._2) andThen yaxis,
      r = 4.0,
      linePaint = PaintScale.blue,
      lineStroke = Plotter.defaultStroke,
      fillPaint = PaintScale.gray
    );
    
    val line = Line(data,
      x = dots.x,
      y = dots.y
    );
    
    val bars = Bars(data,
      x = dots.x,
      y = dots.y,
      width = 4.0,
      height = (t : (Double,Double)) => yaxis.extent(t._2),
      anchor = Anchor.TopCenter,
      linePaint = PaintScale.green.darker,
      lineStroke = Plotter.defaultStroke,
      fillPaint = PaintScale.green
    )
    
    val labels = Labels[((Double,Double),(Double,Double))](
      data = for (((d,r),i) <- (dots.data zip dots.anchors(Anchor.MiddleLeft)).zipWithIndex; if i > 0 && i % 5 == 0) yield (d,r),
      x = _._2._1,
      y = _._2._2,
      text = _._1.toString,
      anchor = Anchor.MiddleRight
    );
    
    Chart(600, 600).
      add(dots, line, bars, labels,
          Wedges(Array(0.5,1.2,2.1),
            x = 100.0, y = 100.0,
            innerRadius = 20.0, outerRadius = 60.0,
            startAngle = (d : Double) => d,
            angle = math.Pi/8),
          xaxis.horizontal(yaxis.pxMax),
          yaxis.vertical(xaxis.pxMin)).
      show();
    
    hist(Map("Tofu"->2,"Cats"->7,"Linux"->4));
  }
}

