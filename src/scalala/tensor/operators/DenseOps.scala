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
package scalala.tensor.operators;

import scalala.collection.{MergeableSet,ProductSet};
import scalala.tensor.{Vector,Matrix};
import scalala.tensor.dense.{DenseVector,DenseMatrix};

import VectorTypes._;
import MatrixTypes._;

/** Type aliases supporting DenseVector operators. */
object DenseVectorTypes {
  type ColDenseVectorOp[V<:DenseVector] =
    ColVectorOp[V];
  
  type RowDenseVectorOp[V<:DenseVector] =
    RowVectorOp[V];
}

import DenseVectorTypes._;

/** Type aliases supporting DenseMatrix operators. */
object DenseMatrixTypes {
  type DenseMatrixOp[M<:DenseMatrix] =
    MatrixOp[M];

  type DenseMatrixTranspose[M<:DenseMatrix] =
    MatrixTranspose[M,Matrix];
}

import DenseMatrixTypes._;

/** Implicits supporting DenseVector operations. */
trait DenseVectorOps {
//  implicit def iColDenseVectorOpTopRichColVectorOp[V<:DenseVector]
//  (op : ColDenseVectorOp[V]) =
//    new RichColDenseVectorOp(op);
//
//  implicit def iRowDenseVectorOpToRichRowDenseVectorOp[V<:DenseVector]
//  (op : RowDenseVectorOp[V]) =
//    new RichRowDenseVectorOp(op);
  
//  implicit def iArrayToColDenseVectorOp(array : Array[Double])
//  : ColDenseVectorOp[DenseVector] =
//    new DenseVector(array);
//  
//  implicit def iArrayToRichColVectorOp(array : Array[Double]) =
//    new RichColDenseVectorOp(new DenseVector(array));
}

/** Singleton instance of DenseVectorOps trait. */
object DenseVectorOps extends DenseVectorOps;

class RichColDenseVectorOp[V<:DenseVector](base : ColDenseVectorOp[V])
extends RichColVectorOp[V,DenseMatrix](base);

class RichRowDenseVectorOp[V<:DenseVector](base : RowDenseVectorOp[V])
extends RichRowVectorOp[V](base);


/** Implicits supporting DenseMatrix operations. */
trait DenseMatrixOps {
//  import DenseVectorOps._;
//  
//  implicit def iDenseMatrixOpToRichDenseMatrixOp[M<:DenseMatrix]
//  (op : DenseMatrixOp[M]) =
//    new RichDenseMatrixOp(op);
}

/** Singleton instance of DenseMatrixOps trait. */
object DenseMatrixOps extends DenseMatrixOps;

/** Extra operators for DenseMatrices. */
class RichDenseMatrixOp[M<:DenseMatrix,V<:DenseVector](base : DenseMatrixOp[M]) {
  def \ [V<:DenseVector] (op : ColDenseVectorOp[V]) =
    DenseMatrixSolveDenseVector(base, op);
  
  def \ [M2<:DenseMatrix] (op : DenseMatrixOp[M2]) =
    DenseMatrixSolveDenseMatrix(base, op);
}

/** Matrix solve vector, like matlab's "\", uses DenseMatrixSolveDenseMatrix. */
case class DenseMatrixSolveDenseVector[M<:DenseMatrix,V<:DenseVector]
(m : DenseMatrixOp[M], v : ColDenseVectorOp[V])
extends ColDenseVectorOp[DenseVector] {
  override def domain = v.domain;
  
  import OperatorImplicits._;
  
  /** Solves via repurposing the DenseMatrixSolveDenseMatrix code. */
  override lazy val value = {
    val _b = v.working;
    val _B = new DenseMatrix(_b.size, 1, _b.data);
    val _X = DenseMatrixSolveDenseMatrix[M,DenseMatrix](m, _B).value;
    
    new DenseVector(_X.data);
  }
  
}

class MatrixSingularException extends RuntimeException;

/**
 * DenseMatrix solver based on code from MTJ 0.9.9.
 */
case class DenseMatrixSolveDenseMatrix[M1<:DenseMatrix,M2<:DenseMatrix]
(a : DenseMatrixOp[M1], b : DenseMatrixOp[M2])
extends TensorReferenceOp(a) {
  import scalala.tensor.dense.Numerics;
  
  override lazy val value = {
    val domain = ProductSet(a.domain.asInstanceOf[ProductSet[Int,Int]]._2,
                            b.domain.asInstanceOf[ProductSet[Int,Int]]._2);
    
    val rows = domain._1.size;
    val cols = domain._2.size;
    
    
    // from MTJ 0.9.9
    if (a.domain.asInstanceOf[ProductSet[Int,Int]]._1 == a.domain.asInstanceOf[ProductSet[Int,Int]]._2) {
      // LUSolve
      val _A : DenseMatrix = a.working//.asInstanceOf[DenseMatrix]; // will be overwritten
      val _B : DenseMatrix = b.value//.asInstanceOf[DenseMatrix];   // won't be overwritten
      val rv = _A.matrixLike(rows,cols);

      if (_A.rows != _B.rows)
        throw new IllegalArgumentException("Matrix arguments must have same number of rows");
      if (rv.rows != _A.cols)
        throw new IllegalArgumentException("This matrix must have same number of rows as first argument has cols");
      if (rv.cols != _B.cols)
        throw new IllegalArgumentException("This matrix must have same number of cols as second argument has rows");
          
      rv := _B;
      val piv = new Array[Int](_A.rows);
      val info = Numerics.lapack.gesv(_A.rows, _B.cols, _A.data, piv, rv.data);
      if (info > 0)
        throw new MatrixSingularException();
      else if (info < 0)
        throw new IllegalArgumentException();
      rv.asInstanceOf[M1];
    } else {
      // QRSolve
      val (trans,_A) = a match {
        case Tensor2Transpose(aT) => (true,aT.working.asInstanceOf[M1]);
        case _ => (false, a.working.asInstanceOf[M1]);
      }
      val _B = b.value;
      val rv = _A.matrixLike(rows,cols);
          
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
      for (j <- 0 until nrhs; i <- 0 until N) rv(i,j) = _Xtmp(i,j);
      rv.asInstanceOf[M1];
    }
    
  }
}

//c <- alpha * a * b + beta * c
//case class GEMM(alpha : Double, a : MatrixOp[DenseMatrix], b : MatrixOp[DenseMatrix],
//                beta : Double, c : MatrixOp[DenseMatrix])
//extends TensorReferenceOp(a) {
//  if ((a.rows != c.rows) ||
//      (a.cols != b.rows) ||
//      (b.cols != c.cols))
//    throw new IllegalArgumentException("Wrong dimensions for matrix multiplication");
//    
//  override def domain = c.domain;
//  override def value = {
//    val (transA,_A) = a match {
//      case aa : (Tensor2Transpose[i1,i2,m1,m2,v1,v2] with MatrixOp) => (true, aa.op.value.asInstanceOf[DenseMatrix]);
//      case _ =>                   (false, a.value.asInstanceOf[DenseMatrix]);
//    }
//        
//    val (transB,_B) = b match {
//      case bb : (Tensor2Transpose[i1,i2,m1,m2,v1,v2] with MatrixOp) => (true, bb.op.value.asInstanceOf[DenseMatrix]);
//      case _ =>                   (false, b.value.asInstanceOf[DenseMatrix]);
//    }
//      
//    val _C = c.working;
//      
//    Numerics.blas.gemm(transA, transB, _C.rows, _C.cols, _A.cols,
//                       alpha, _A.data, _A.rows, _B.data, _B.rows,
//                       beta, _C.asInstanceOf[DenseMatrix].data, _C.rows);
//      
//    _C;
//  }
//}

