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

import random.MersenneTwisterFast

/**
 * <p>Random number generation.  This class uses the MersenneTwisterFast
 * implementation by Sean Luke http://www.cs.gmu.edu/~sean/research/
 * as its underlying random number generator.  The Mersenne Twister
 * is very fast with an excellent pseudo-random distribution, but it
 * is not cryptographically strong.</p>
 *
 * <p>Each random number generating method accepts a
 * MersenneTwisterFast implementation as an implicit argument, defaulting
 * to the <code>Random.mt</code> instance.  That instance's seed is set
 * with the long value held in the scalala.library.random.seed System property.
 * If the property is not defined, the current time in milliseconds is used
 * as the random seed.</p>
 *
 * <p>The MersenneTwisterFast implementation is not thread-safe, so all
 * accessors to an instance (<code>mt</code>) wrap calls in a mt.synchronized
 * block.  Therefore, calling a vector constructor is substantially faster
 * than calling rand() many times.</p>
 *
 * <p>The seed can be set with the system property during Java invocation, e.g. by
 * -Dscalala.library.random.seed=1l</p>
 *
 * @author dramage,afwlehmann
 */
object Random {
  /**
   * Returns a pseudo-random number from [0,1).
   */
  def rand()(implicit mt: MersenneTwisterFast): Double = mt.synchronized {
    mt.nextDouble()
  }

  /**
   * Returns a pseudo-random gaussian variable.
   * */
  def randn()(implicit mt: MersenneTwisterFast) = mt.synchronized {
    mt.nextGaussian()
  }

  /**
   * Returns a pseudo-random integer between 0 (incl.) and `max` (excl.).
   */
  def randi(max: Int)(implicit mt: MersenneTwisterFast) = mt.synchronized {
    mt.nextInt(max)
  }

  lazy val seed : Long = {
    val prop = System.getProperty("scalala.library.random.seed");
    if (prop != null) prop.toLong else System.currentTimeMillis;
  }

  implicit val mt : MersenneTwisterFast =
    new MersenneTwisterFast(seed);
}
