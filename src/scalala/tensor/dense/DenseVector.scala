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
package scalala.tensor.dense;

import scalala.collection.{MergeableSet, IntSpanSet};
import scalala.collection.domain.{Domain, IntSpanDomain, DomainException};

import scalala.tensor.Vector;
import scalala.tensor.sparse.{SparseVector,SparseBinaryVector};
import scalala.tensor.operators._;

import scalala.tensor.Tensor.CreateException;

/**
 * A vector backed by a dense array of doubles.
 * 
 * @author dramage
 */
class DenseVector(data : Array[Double]) extends
  DoubleArrayData(data) with Vector with DenseTensor[Int] with MatrixVectorSolver[Int] {
  
  def this(size : Int) = this(new Array[Double](size));
  
  override def size = data.size;
  
  override def apply(i : Int) = data(i);
  override def update(i : Int, value : Double) = data(i) = value;
  
  override def activeDomain : MergeableSet[Int] = IntSpanSet(0, size);
  
  override def copy = new DenseVector(data.toArray).asInstanceOf[DenseVector.this.type];
  
  override def dot(other : Tensor1[Int]) : Double = other match {
    case dense  : DenseVector  => dot(dense);
    case binary : SparseBinaryVector => binary.dot(this);
    case sparse : SparseVector => sparse.dot(this);
    case _ => super.dot(other);
  }
  
  def dot(that : DenseVector) = {
    if (this.size != that.size) throw new DomainException();
    var i = 0;
    var sum = 0.0;
    while (i < data.length) {
      sum += this.data(i) * that.data(i);
      i += 1;
    }
    sum;
  }
  
  /** Assigns each element in this map to the corresponding value as returned by the given operation. */
  override def :=  (op : TensorOp[Int]) : Unit = {
    def isDense[QI](op : TensorOp[QI]) : Boolean = op match {
      case MatrixTranspose(aT) => isDense(aT);
      case _ => op.value.isInstanceOf[DenseTensor[_]];
    }
    
    op match {
      case MatrixSolveVector(a, b) if isDense(a) && isDense(b) =>
        import scalala.Scalala._;
        val _b = b.working.asInstanceOf[DenseVector];
        val _B = new DenseMatrix(_b.size, 1, _b.data);
        val _X = new DenseMatrix(this.size, 1, this.data);
        _X := MatrixSolveMatrix(a.asInstanceOf[MatrixOp[Int,Int]], _B);
      case MatrixSolveVector(a, b) =>
        throw new UnsupportedOperationException("DenseMatrix solution requires both arguments to be dense");
      case _ => super.:=(op);
    }
  }
  
  override def toString() = new DenseMatrix(size, 1, data).toString();
}
