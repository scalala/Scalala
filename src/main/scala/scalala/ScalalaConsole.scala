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

/**
 * Runs Scalala in a console.
 * 
 * @author dlwh,dramage
 */
object ScalalaConsole {
  def main(args : Array[String]) {
    // read scalala.scala to a tmp file
    val stream = this.getClass.getClassLoader.getResourceAsStream("scalala.scala");
    val file = java.io.File.createTempFile("scalala-startup-", ".scala");
    file.deleteOnExit;
    val fos = new java.io.PrintStream(new java.io.FileOutputStream(file));
    for (line <- scala.io.Source.fromInputStream(stream).getLines()) {
      fos.print(line);
    }
    fos.close();

    // redirect to invoking the standard scala main method
    val method = Class.forName("scala.tools.nsc.MainGenericRunner").
      getMethod("main", classOf[Array[String]]);

    // augmented arguments
    val aurg : Object = (
      List[String](
        "-nocompdaemon",
        "-classpath", System.getProperty("java.class.path"),
        "-no-specialization",
        "-usejavacp",
        "-i", file.getAbsolutePath
      ) ++ args
    ).toArray[String];

    method.invoke(null, aurg);
  }
}
