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


import scalala.tensor.{::, Matrix}
import scalala.tensor.dense.DenseMatrix
import java.io._
import util.matching.Regex

trait Storage {

  /**
   * Deserializes a DenseMatrix from the given input stream.
   *
   * @param is
   *  the input stream
   * @param skipRows
   *  number of lines to skip at the beginning
   * @param columns
   *  indices of the columns to be used, or null for all columns (default), e.g.
   *  List(1,3) for columns 2 and 4, respectively
   * @param delimiter
   *  a regular expression defining the delimiter that is used to separate the
   *  values
   * @param comments
   *  a regular expression defining the start of comments which will be removed
   *  throughout the process
   */
  def loadtxt(is: InputStream,
              skipRows: Int = 0,
              columns: Seq[Int] = null,
              delimiter: Regex = Storage.defaultDelimiter,
              comments: Regex = Storage.defaultComments):
    DenseMatrix[Double] =
  {
    require(skipRows >= 0)
    require(columns == null || columns.size > 0)

    val it = new Iterator[Array[String]] {
      val br = new BufferedReader(new InputStreamReader(is))
      def hasNext = br.ready
      def next =
        // Regex.split actually returns an Array("") in case of an empty
        // input string yet we want Array.empty instead, thus:
        delimiter split (comments replaceFirstIn (br.readLine, "") trim) match {
          case Array("") => Array.empty[String]
          case r => r
        }
    }

    for (i <- 0 until skipRows if it.hasNext) { it.next() }

    var numCols = -1
    val rows: Array[Array[Double]] =
      (for (tokens <- it if !tokens.isEmpty) yield {
        if (tokens.size != numCols) {
          if (numCols == -1)
            numCols = tokens.size
          else
            throw new RuntimeException("All lines must have the same number of columns!")
        }
        if (columns == null)
          tokens.map(_.toDouble).toArray[Double]
        else
          ((for (colIdx <- columns) yield
            tokens(colIdx).toDouble)).toArray[Double]
      }).toArray

    if (rows.size > 0)
      DenseMatrix(rows:_*)
    else
      null
  }


  /**
   * Serializes the given matrix to the given output stream. The given delimiter
   * is used to separate the values of each row.
   */
  def storetxt(os: OutputStream, m: Matrix[Double], delimiter: String = "\t") {
    val bw = new BufferedWriter(new OutputStreamWriter(os))
    for (i <- 0 until m.numRows) {
      bw.write(m(i,::).values.iterator map ("%.15e".format(_)) mkString delimiter)
      bw.newLine()
    }
    bw.flush()
  }


}

object Storage extends Storage {

  val defaultDelimiter: Regex = """\s+""".r
  val defaultComments: Regex = """#.*$""".r

}
