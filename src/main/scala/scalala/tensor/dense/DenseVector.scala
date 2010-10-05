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
package dense;

import java.util.Arrays;
import scalala.collection.{MergeableSet, IntSpanSet, DomainException, PartialMap};
import sparse._;
import operators._;
import TensorShapes._;
import Tensor.CreateException;

/**
 * A vector backed by a dense array of doubles.
 * 
 * @author dramage
 */
class DenseVector(data : Array[Double]) extends
  DoubleArrayData(data) with Vector with DenseTensor[Int]
  with TensorSelfOp[Int,DenseVector,Shape1Col] {
  
  /** Constructor for a vector of zeros of the given size. */
  def this(size : Int) = this(new Array[Double](size));
  
  override def size = data.length;
  
  override def apply(i : Int) = data(i);
  override def update(i : Int, value : Double) = data(i) = value;

 /** 
  * Creates a tensor "like" this one, but with zeros everywhere.
  */
  def like = new DenseVector(size);

  override def copy = {
    val arr = new Array[Double](size);
    System.arraycopy(data,0,arr,0,size);
    new DenseVector(arr);
  }
  
  override def dot(other : Tensor1[Int]) : Double = other match {
    case singleton : SingletonBinaryVector => this.apply(singleton.singleIndex)
    case dense  : DenseVector  => dot(dense);
    case binary : SparseBinaryVector => binary.dot(this);
    case sparse : SparseVector => sparse.dot(this);
    case hash   : SparseHashVector => hash.dot(this);
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
  
  override def toString() = new DenseMatrix(size, 1, data).toString();

  override def +=(c: Double) {
    var i = 0;
    while(i < data.length) {
      data(i) += c;
      i += 1;
    }
  }

  override def -=(c: Double) {
    var i = 0;
    while(i < data.length) {
      data(i) -= c;
      i += 1;
    }
  }

  override def *=(c: Double) {
    var i = 0;
    while(i < data.length) {
      data(i) *= c;
      i += 1;
    }
  }

  override def /=(c: Double) {
    var i = 0;
    while(i < data.length) {
      data(i) /= c;
      i += 1;
    }
  }

  override def :=  (t : PartialMap[Int,Double]) = t match {
    case v: DenseVector =>
      ensure(v)
      System.arraycopy(v.data,0,this.data,0,this.data.length);
    case s: SparseVector =>
      Arrays.fill(this.data,s.default);
      var offset = 0;
      while(offset < s.used) {
        data(s.index(offset)) = s.data(offset);
        offset+=1;
      }
    case _ => super.:=(t);
  }

  override def :+=  (t : PartialMap[Int,Double]) = t match {
    case v: Vector =>
      ensure(t);
      var i = 0;
      while(i < size) {
        this(i) += v(i);
        i += 1;
      }
    case _ => super.:+=(t);
  }


  /** Make Scala happy about inheritance */
  def :+= [V<:Tensor[Int]]  (op : Vector) { this.:+=(op:PartialMap[Int,Double]); }

  /** Increments each element in this map by the corresponding value as returned by the given operation. */
  override def :+= [V<:Tensor[Int]] (op : TensorOp[V,_]) : Unit = {
    op match {
      case TensorMultScalar(tt, s) if tt.isInstanceOf[DenseVector] && (s != 0) => {
        val t = tt.asInstanceOf[DenseVector]
        ensure(t);
        var i = 0;
        while(i < size) {
          this(i) += t(i) * s;
          i += 1;
        }
      }
      case t:DenseVector => {
        ensure(t);
        var i = 0;
        while(i < size) {
          this(i) += t(i);
          i += 1;
        }
      }
      case _ => super.:+=(op);
    }
  }

  /** Make Scala happy about inheritance */
  def :-= [V<:Tensor[Int]]  (op : Vector) { this.:-=(op:PartialMap[Int,Double]); }

  /** Decrements each element in this map by the corresponding value as returned by the given operation. */
  override def :-= [V<:Tensor[Int]] (op : TensorOp[V,_]) : Unit = {
    op match {
      case TensorMultScalar(tt, s) if tt.isInstanceOf[DenseVector] && (s != 0) => {
        val t = tt.asInstanceOf[DenseVector]
        ensure(t);
        var i = 0;
        while(i < size) {
          this(i) -= t(i) * s;
          i += 1;
        }
      }
      case t:DenseVector => {
        ensure(t);
        var i = 0;
        while(i < size) {
          this(i) -= t(i);
          i += 1;
        }
      }
      case _ => super.:-=(op);
    }
  }

}

object DenseVector {
  /**
   * Static constructor that creates a dense vector of the given size
   * initialized by elements from the given values list (looping if
   * necessary).
   */
  def apply(size : Int)(values : Double*) =
    new DenseVector(Array.tabulate(size)(i => values(i % values.length)));
  
  def apply(map : PartialMap[Int,Double]) =
    new DenseVector(Array.tabulate(map.size)(i => map(i)));
}
