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
package tensor;
package adaptive;

import java.util.Arrays;

import collection.{MergeableSet,IntSpanSet,DomainException};

import Tensor.CreateException;
import dense.DenseVector;

import operators.TensorShapes._;
import operators.TensorSelfOp;
import sparse.SparseVector;
import sparse.SparseHashVector;
import sparse.SparseHashMatrix;

/**
 * An adaptive vector switches between a SparseVector and a DenseVector depending on sparsity.
 * @author dlwh
 **/
class AdaptiveVector(private var vec: Vector) extends Vector with TensorSelfOp[Int,AdaptiveVector,Shape1Col] {

  /**
   * Computes the point at which we switch to using a DenseVector
   */
  protected def densityThreshold = 0.5;

  def this(domainSize:Int) = this(new SparseVector(domainSize));


  final def innerVector = vec;

  override def size = vec.size;
  override def default = vec.default;
  override def default_=(d: Double) = vec.default = d;

  override def activeDomain = vec.activeDomain;
  override def activeElements = vec.activeElements;
  override def activeKeys = vec.activeKeys;
  override def activeValues = vec.activeValues;
  override def zero() = {
    vec = new SparseVector(vec.size);
  }

  override def apply(i : Int) : Double = vec(i);
  override def update(i: Int, value: Double) = vec match {
    case sparse: SparseHashVector if( sparse.used >= densityThreshold * sparse.size)  =>
      vec = switchRepr();
      vec.update(i,value);
    case sparse: SparseVector if( sparse.used >= densityThreshold * sparse.size)  =>
      vec = switchRepr();
      vec.update(i,value);
    case _ =>
      vec.update(i,value);
  }

  override def +=(c: Double) = vec += c;
  override def -=(c: Double) = vec -= c;
  override def *=(c: Double) = vec *= c;
  override def /=(c: Double) = vec /= c;

  override def copy = {
    new AdaptiveVector(vec.copy);

  }

  def like = new AdaptiveVector(size);

  /**
   * Creates a vector "like" this one, but with zeros everywhere.
   */
  def vectorLike(size:Int) = new AdaptiveVector(size);

  /**
   * Creates a SparseHashMatrix. May change if we get a new kind of sparse matrix.
   */
  def matrixLike(rows:Int, cols:Int) = new SparseHashMatrix(rows,cols);

  /** Uses optimized implementations. */
  override def dot(other : Tensor1[Int]) : Double = vec.dot(other);

  def densify() {
    vec match {
      case x: DenseVector =>
      case _ => vec = switchRepr();
    }
  }

  protected def switchRepr() = {
    val newVec = new DenseVector(size);
    newVec := vec;
    newVec;
  }
}

object AdaptiveVector {
  def apply(size : Int)(default : Double) = {
    val sv = new AdaptiveVector(size);
    sv.default = default;
    sv;
  }
}
