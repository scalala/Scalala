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
package scalala.library

trait IO extends Library {
  
  import java.lang.Double.parseDouble;
  
  def dlmread(file : String) : Matrix = {
    val numCols = scala.io.Source.fromFile(file).getLines.next.trim.split("\\s+").length;
    val numRows = scala.io.Source.fromFile(file).getLines.map(_ => 1).reduceLeft(_+_);
    
    val m = DenseMatrix(numRows, numCols);
    
    var i = 0;
    for (line <- scala.io.Source.fromFile(file).getLines) {
      var j = 0;
      for (entry <- line.trim.split("\\s+").map(parseDouble)) {
        m(i,j) = entry;
        j += 1;
      }
      i += 1;
    }
    
    return m;
  }
}
