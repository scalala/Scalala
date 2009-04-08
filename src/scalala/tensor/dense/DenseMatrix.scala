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

import scalala.tensor.{Tensor,Matrix};
import scalala.collection.{MergeableSet, ProductSet, IntSpanSet};
import scalala.collection.domain.{Domain, Domain2, IntSpanDomain};

import scalala.tensor.Tensor.CreateException;

import scalala.tensor.operators._;

/**
 * A vector backed by a dense array of doubles, with each column stored
 * before the next column begins.
 * 
 * @author dramage
 */
class DenseMatrix(data : Array[Double], nRows : Int, nCols : Int) extends
  DoubleArrayData(data) with Matrix with MatrixMatrixSolver[Int,Int] with DenseTensor[(Int,Int)] {
  
  if (nRows * nCols != data.length) throw new Predef.IllegalArgumentException;
  
  def this(nRows : Int, nCols : Int) =
    this(new Array[Double](nRows * nCols), nRows, nCols);
  
  @inline final def index(row : Int, col : Int) : Int = {
    check(row,col);
    row + col * rows;
  }
  
  override def rows = nRows;
  override def cols = nCols;
  
  override def apply(row : Int, col : Int) : Double =
    data(index(row,col));
  
  override def update(row : Int, col : Int, value : Double) =
    data(index(row,col)) = value;

  override def activeDomain : MergeableSet[(Int,Int)] = ProductSet(IntSpanSet(0, rows), IntSpanSet(0, cols));
  override def activeDomainInRow(row : Int) = IntSpanSet(0, cols);
  override def activeDomainInCol(col : Int) = IntSpanSet(0, rows);
  
  override def copy = new DenseMatrix(data.toArray, rows, cols).asInstanceOf[this.type];
  
  /** Assigns each element in this map to the corresponding value as returned by the given operation. */
  override def :=[T<:Tensor[(Int,Int)]]  (op : TensorOp[(Int,Int),T]) : Unit = {
    def isDense[QI,QJ,QT<:Tensor2[QI,QJ]](op : MatrixOp[QI,QJ,QT]) : Boolean = op match {
      case MatrixTranspose(aT) => isDense(aT);
      case _ => op.value.isInstanceOf[DenseMatrix];
    }
    
    op match {
      case MatrixSolveMatrix(a, b) if isDense(a) && isDense(b) =>
        if (a.domain._1 == a.domain._2) {
          // LUSolve
          val _A = a.working.asInstanceOf[DenseMatrix]; // will be overwritten
          val _B = b.value.asInstanceOf[DenseMatrix];   // won't be overwritten
          
          if (_A.rows != _B.rows)
            throw new IllegalArgumentException("Matrix arguments must have same number of rows");
          if (this.rows != _A.cols)
            throw new IllegalArgumentException("This matrix must have same number of rows as first argument has cols");
          if (this.cols != _B.cols)
            throw new IllegalArgumentException("This matrix must have same number of cols as second argument has rows");
          
          this := _B;
          val piv = new Array[Int](_A.rows);
          val info = Numerics.lapack.gesv(_A.rows, _B.cols, _A.data, piv, this.data);
          if (info > 0)
            throw new MatrixSingularException();
          else if (info < 0)
            throw new IllegalArgumentException();
        } else {
          // QRSolve
          val (trans,_A) = a match {
            case MatrixTranspose(aT) => (true,aT.working.asInstanceOf[DenseMatrix]);
            case _ => (false, a.working.asInstanceOf[DenseMatrix]);
          }
          val _B = b.value.asInstanceOf[DenseMatrix];
          
          // allocate temporary solution matrix
          val nrhs = _B.cols;
          val _Xtmp = new DenseMatrix(Math.max(rows,cols),nrhs);
          val M = if (!trans) _A.rows else _A.cols;
          for (j <- 0 until nrhs; i <- 0 until M) { _Xtmp(i,j) = _B(i,j); }
          
          // query optimal workspace
          val queryWork = new Array[Double](1);
          val queryInfo = Numerics.lapack.gels(trans, _A.rows, _A.cols, nrhs, _A.data, _Xtmp.data, queryWork, -1);
          
          // allocate workspace
          val work = {
            val lwork = {
              if (queryInfo != 0)
                Math.max(1, Math.min(_A.rows, _A.cols) + Math.max(Math.min(_A.rows, _A.cols), nrhs));
              else
                Math.max(queryWork(0).asInstanceOf[Int], 1);
            }
            new Array[Double](lwork);
          }
          
          // compute factorization
          val info = Numerics.lapack.gels(trans, _A.rows, _A.cols, nrhs, _A.data, _Xtmp.data, work, work.length);
          
          if (info < 0)
            throw new IllegalArgumentException;
          
          // extract solution
          val N = if (!trans) _A.cols else _A.rows;
          for (j <- 0 until nrhs; i <- 0 until N) this(i,j) = _Xtmp(i,j);
        }
      case MatrixSolveMatrix(b, x) =>
        throw new UnsupportedOperationException("DenseMatrix solution requires both arguments to be dense");
      case _ => super.:=(op);
    }
  }
}

trait DenseMatrixSolveTest {
  import scalala.ScalalaTest._;
  import scalala.Scalala._;
  
  def _solve_test() {
    val _A = new DenseMatrix(Array(1.0, 2.0, 3.0, 4.0), 2, 2);
    val _B = new DenseMatrix(Array(2.0, 0.0, 3.0, 3.0), 2, 2);
    assertEquals(new DenseMatrix(Array(-4.0, 2.0, -1.5, 1.5), 2, 2), (_A \ _B) value);
    
    val _C = new DenseMatrix(Array(1.0, 2.0, 3.0, 4.0, 5.0, -1.0), 2, 3);
    val _D = new DenseMatrix(Array(2.0, 0.0, 3.0, 3.0), 2, 2);
    assertEquals(DenseMatrix(Array(0.0091743, 0.0825688, 0.3486239, 0.2935780, 0.6422018, 0.1559633), 3, 2), (_C \ _D).value, 1e-7);
    
    assertEquals(Vector(0.0091743, 0.0825688, 0.3486239), (_C \ Vector(2, 0)).value, 1e-7);
  }
}
