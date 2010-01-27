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
package scalala

import scalala.Scalala._;
  

/**
 * Profiling extensions of ScalalaTest.
 * 
 * @author dramage
 */
trait ScalalaProfiling {
  /**
   * Runs the given code block 2*n times.  The last n
   * times are averaged to compute the average time
   * to run the code block, returned in millesconds.
   */
  def profile[E](n : Int)(func : => Unit) : Double = {
    for (i <- 0 until n) { func; }
    mean(
      for (i <- 0 until n) yield {
        val start = System.currentTimeMillis;
        func;
        (System.currentTimeMillis - start).toDouble;
      }
    );
  }
}

object ScalalaProfiling extends ScalalaProfiling {
}
