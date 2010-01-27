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
package scalala.tensor.adaptive

import java.util.Arrays;

import scalala.tensor.{Tensor1,Vector};
import scalala.collection.{MergeableSet,IntSpanSet,DomainException};

import scalala.tensor.Tensor.CreateException;
import scalala.tensor.dense.DenseVector;

import scalala.tensor.operators.TensorShapes._;
import scalala.tensor.operators.TensorSelfOp;
import scalala.tensor.sparse.SparseVector;
import scalala.tensor.sparse.SparseHashMatrix;

/**
 * An adaptive vector switches between a SparseVector and a DenseVector depending on sparsity.
 * @author dlwh
 **/
class AdaptiveVector(private var vec: Vector) extends Vector with TensorSelfOp[Int,AdaptiveVector,Shape1Col] {

  /**
   * Computes the point at which we switch to using a DenseVector
   */
  protected def densityThreshold = 0.6;

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
    case sparse: SparseVector if( sparse.used >= densityThreshold)  =>
      vec = switchRepr();
      vec.update(i,value);
    case _ =>
      vec.update(i,value);
  }

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

  protected def switchRepr() = {
    val arr = new Array[Double](size);
    Arrays.fill(arr,default);
    val newVec = new DenseVector(arr);
    newVec := this;
    newVec;
  }
}

trait AdaptiveVectorTest extends scalala.library.Library with scalala.library.Random with scalala.ScalalaTest {
  test("AdaptiveVector:General") {
    val sparse = new AdaptiveVector(10);
    val dense  = new scalala.tensor.dense.DenseVector(10);

    val values = List((2,2),(4,4),(3,3),(0,-1),(5,5),(1,1),(9,9),(7,7),(8,8),(3,3));

    for ((index,value) <- values) {
      sparse(index) = value;
      dense(index) = value;
      assertEquals(dense, sparse);
    }
  }

  test("AdaptiveVector:Dot") {
    val x = new AdaptiveVector(10);
    val y = new AdaptiveVector(10);
    val d = rand(10);

    def densedot(a : Vector, b : Vector) =
      new DenseVector(a.toArray) dot new DenseVector (b.toArray);

    def checks() = {
      assertEquals(densedot(x,y), x dot y);
      assertEquals(densedot(y,x), y dot x);
      assertEquals(densedot(x,d), x dot d);
      assertEquals(densedot(d,x), d dot x);
      assertEquals(densedot(y,d), y dot d);
      assertEquals(densedot(d,y), d dot y);
    }

    x(2) = 3;
    y(2) = 4;
    checks();

    x(7) = 2;
    y += 1;
    checks();

    y -= 1;
    y(7) = .5;
    checks();

    x += 1;
    y(8) = 2;
    checks();

    y += 1;
    checks();
  }
}

object AdaptiveVector {
  def apply(size : Int)(default : Double) = {
    val sv = new AdaptiveVector(size);
    sv.default = default;
    sv;
  }

}
