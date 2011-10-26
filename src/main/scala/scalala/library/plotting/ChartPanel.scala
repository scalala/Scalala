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

import java.awt.{Graphics,Graphics2D,Toolkit};
import java.awt.datatransfer.{Clipboard,ClipboardOwner,DataFlavor,Transferable,UnsupportedFlavorException};
import java.awt.{Paint,Color,Stroke,BasicStroke,Font};
import java.awt.geom.{AffineTransform,Point2D,Line2D,Ellipse2D};
import java.awt.event.{MouseEvent,MouseWheelEvent,MouseAdapter};
import java.awt.image.BufferedImage;
import javax.swing.{JComponent,JViewport,WindowConstants};

/**
 * ChartPanel provides a rich swing JComponent for viewing the contents of 
 * a chart.
 *
 * @author dramage
 */
class ChartPanel(chart : Chart, width : =>Int, height : =>Int, background : Color = Color.WHITE) extends JComponent {
  val transform = new AffineTransform;
  var zoom = 1.0;

  this.setPreferredSize(new java.awt.Dimension(width,height));

  def zoomTopLeftX =
    getParent.asInstanceOf[JViewport].getViewPosition.getX;
  
  def zoomTopLeftY =
    getParent.asInstanceOf[JViewport].getViewPosition.getY;

  val clipboard = new ClipboardOwner {
    override def lostOwnership(clip : Clipboard, trans : Transferable) = {}
    
    def copy() {
      val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB);
      val g = image.createGraphics();
      g.setColor(background);
      g.fillRect(0,0,width,height);
      g.translate(-zoomTopLeftX,-zoomTopLeftY);
      g.scale(zoom, zoom);
      chart.render(g);
      g.dispose;
      
      val transfer = new Transferable {
        override def getTransferData(flavor : DataFlavor) = {
          if (flavor == DataFlavor.imageFlavor) image
          else throw new UnsupportedFlavorException(flavor)
        }
        
        override val getTransferDataFlavors =
          Array(DataFlavor.imageFlavor);
        
        override def isDataFlavorSupported(flavor : DataFlavor) =
          flavor == DataFlavor.imageFlavor;
      }
      
      Toolkit.getDefaultToolkit().getSystemClipboard().setContents(transfer,this);
    }
  }

  val adapter = new MouseAdapter {
    val coord = new Point2D.Double(0,0);
  
    override def mousePressed(e : MouseEvent) = {
      if (e.isPopupTrigger) {
        showContextPopup(e.getX, e.getY);
      }
    }
  
    override def mouseReleased(e : MouseEvent) = {
      if (e.isPopupTrigger) {
        showContextPopup(e.getX, e.getY);
      }
    }
  
    override def mouseMoved(e : MouseEvent) = {
      // println(e.getX + " " + e.getY);
    }
    
    override def mouseWheelMoved(e : MouseWheelEvent) {
      this.synchronized {
        val scale = if (e.getWheelRotation() < 0) 1.1 else 1.0 / 1.1;

        val oldZoom = zoom;
        zoom *= scale;
        
        // don't zoom out past 1
        if (zoom < 1) {
          zoom = 1;
        }

        val dimension = new java.awt.Dimension((zoom * width).round.toInt, (zoom * height).round.toInt);
        setPreferredSize(dimension);
        revalidate();
    
        if (getParent.isInstanceOf[JViewport]) {
          val vp = getParent.asInstanceOf[JViewport];
          
          // println(e.getX, e.getY);
          
          val oldTopLeft  = vp.getViewPosition;
          val oldTopLeftX = oldTopLeft.getX;
          val oldTopLeftY = oldTopLeft.getY;
          val oldPointerX  = oldTopLeftX + (oldZoom * e.getX);
          val oldPointerY  = oldTopLeftY + (oldZoom * e.getY);
          //val oldScreenW  = vp.getWidth * oldZoom;
          //val oldScreenH  = vp.getHeight * oldZoom;
          val newPointerX  = oldPointerX * scale;
          val newPointerY  = oldPointerY * scale;
          //val newScreenW  = vp.getWidth * zoom;
          //val newScreenH  = vp.getHeight * zoom;
          val newTopLeftX = newPointerX - scale * (oldPointerX - oldTopLeftX);
          val newTopLeftY = newPointerY - scale * (oldPointerY - oldTopLeftY);
          
          // println("!" + (oldTopLeftX, oldTopLeftY) + " " + (newTopLeftX, newTopLeftY));
          // println("!" + (oldPointerX, oldPointerY) + " " + (newPointerX, newPointerY));
          val p = new java.awt.Point(newTopLeftX.round.toInt, newTopLeftY.round.toInt);

          vp.setViewSize(dimension);
          vp.setViewPosition(p);
        }
        
        transform.setToIdentity;
        transform.scale(zoom,zoom);
        // transform.translate(zoomx*width*zoom,zoomy*width*zoom);
        // transform.translate(-zoomx*width,-zoomy*width);
      }
      repaint();
    }
  }
  
  this.addMouseListener(adapter)
  this.addMouseMotionListener(adapter)
  this.addMouseWheelListener(adapter)
  
	override def paintComponent(g : Graphics) = this.synchronized {
		super.paintComponent(g);
		val g2 = g.asInstanceOf[Graphics2D];

		// clear rectangle
		val defaultPaint = g2.getPaint;
		g2.setPaint(background);
		g2.fillRect(0,0,getWidth,getHeight);
		g2.setPaint(defaultPaint);
		
		// render chart
		val originalTransform = g2.getTransform();
    g2.transform(transform);
		chart.render(g2);
		g2.setTransform(originalTransform);
	}
	
	def showContextPopup(x : Int, y : Int) {
	  import java.awt.event.{ActionEvent,ActionListener};
	  import javax.swing.{JPopupMenu,JMenuItem};
	  
	  def createMenuItem(name : String, onClick : ()=>Unit) = {
	    val item = new JMenuItem(name);
	    item.addActionListener(new ActionListener {
	      def actionPerformed(e : ActionEvent) = onClick();
      });
      item;
    }
	  
	  val popup = new JPopupMenu {
	    add(createMenuItem("Copy", () => clipboard.copy()));
	    add(createMenuItem("Save as...", () => showSaveChartDialog()));
	  };
	  
	  popup.show(this, x, y);
  }
	
	def showSaveChartDialog() {
    import javax.swing.JFileChooser;
	  import javax.swing.filechooser.FileNameExtensionFilter;
	  
	  val chooser = new JFileChooser();
	  val filter = new FileNameExtensionFilter("PNG, EPS, and PDF Files", "png", "eps", "pdf");
	  chooser.setFileFilter(filter);
	  chooser.showSaveDialog(this) match {
	    case JFileChooser.APPROVE_OPTION =>
	      chart.saveas(chooser.getSelectedFile())
	    case _ =>
	      /* do nothing */ ()
	  }
  }
}

