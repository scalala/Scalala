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

import scalala.tensor.{Tensor,Vector,Matrix,DiagonalMatrix};
import scalala.tensor.dense.{DenseVector,DenseMatrix};
import scalala.tensor.sparse.{SparseVector};

/**
 * Basic data type construction and manipulation.
 * 
 * @author dramage
 */
trait Library {
  def Vector(values : Array[Double]) : Vector =
    new DenseVector(values);
  
  def Vector(values : Double*) : Vector =
    Vector(values.asInstanceOf[Collection[Double]]);
  
  def Vector(values : Collection[Double]) : Vector = values match {
    case array : Array[Double] => new DenseVector(array);
    case _ => new DenseVector(values.toArray);
  }
  
  def DenseVector(values : Array[Double]) : Vector =
    new DenseVector(values);
  
  def DenseVector(values : Double*) : DenseVector =
    new DenseVector(values.asInstanceOf[Collection[Double]].toArray);
  
  def DenseVector[T<:AnyVal](values : Seq[T])(implicit manifest : scala.reflect.Manifest[T]) : DenseVector = {
    import scalala.tensor.TensorImplicits._;
    val x = new DenseVector(values.size);
    x := values;
    x;
  }
  
//  def SparseVector(size : Int) : Vector =
//    new SparseVector(size);
//  
//  def SparseVector(size : Int, nonzeros : Int) : Vector =
//    new SparseVector(size, nonzeros);
  
  def Matrix(rows : Int, cols : Int)(values : Double*) : Matrix =
    new DenseMatrix(rows, cols, values.toArray);
  
  def DenseMatrix(rows : Int, cols : Int, values : Collection[Double]) : Matrix = {
    values match {
      case array : Array[Double] => new DenseMatrix(rows, cols, array);
      case _ => new DenseMatrix(rows, cols, values.toArray);
    }
  }
  
//  def DenseMatrix(rows : Int, cols : Int) : Matrix =
//    new DenseMatrix(rows,cols);
  
  def DiagonalMatrix(diagonal : Vector) : Matrix =
    new DiagonalMatrix(diagonal);
  
  //
  // basic scala ops from Math.
  //
  
  /** Log a value. */
  final def log(v : Double) : Double = Math.log(v);
  
  /** Exponentiate a value. */
  final def exp(v : Double) : Double = Math.exp(v);

  final def pow(base : Double, exponent : Double) : Double =
    Math.pow(base, exponent);
  
  /** Alias for Double.NaN */
  final val NaN = Double.NaN;
  
  /** Alias for Double.NaN */
  final val nan = NaN;
  
  /** Alias for Double.PositiveInfinity */
  final val Inf = Double.PositiveInfinity;
  
  /** Alias for Double.PositiveInfinity */
  final val inf = Inf;
  
  /** Alias for x.isNaN. */
  final def isnan(x : Double) = x.isNaN;

  /** Alias for Math.sqrt. */
  final def sqrt(x : Double) = Math.sqrt(x);
}
