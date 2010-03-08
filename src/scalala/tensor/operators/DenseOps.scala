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
import scalala.tensor.{Vector,Matrix,Tensor1,Tensor2};
import scalala.tensor.dense.{DenseVector,DenseMatrix};
import scalala.library.Matrices;

import TensorShapes._;

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
import Tensor1Types._;
import Tensor2Types._;

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
  implicit val denseVectorColArith = new Tensor1Arith[Int,DenseVector,Tensor1[Int],Shape1Col];
  implicit val denseVectorRowArith = new Tensor1Arith[Int,DenseVector,Tensor1[Int],Shape1Row];


  implicit val denseVectorPBuilder: TensorProductBuilder[DenseVector,Vector,DenseMatrix,Shape1Col,Shape1Row,Shape2] =
  new TensorProductBuilder[DenseVector,Vector,DenseMatrix,Shape1Col,Shape1Row,Shape2] {
    def create(t: DenseVector, t2: Vector) = new DenseMatrix(t.size,t2.size);
    def makeProduct(t: ColTensor1Op[DenseVector], t2: RowTensor1Op[Vector]) = {
      Tensor1OuterMultTensor1[Int,Int,DenseVector,Vector,DenseMatrix](t,t2);
    }
  }

  class DenseVectorMPBuilder extends TensorProductBuilder[DenseVector,Matrix,DenseVector,Shape1Row,Shape2,Shape1Row] {
    def create(t: DenseVector,t2: Matrix) = t.vectorLike(t2.cols);
    def makeProduct(t: RowTensor1Op[DenseVector], t2: Tensor2Op[Matrix]) = {
      RowTensor1MultTensor2[Int,Int,DenseVector,DenseVector,Matrix](t,t2);
    }
  }

  implicit val denseVectorMPBuilder = new DenseVectorMPBuilder;
}

/** Singleton instance of DenseVectorOps trait. */
object DenseVectorOps extends DenseVectorOps;

/** Implicits supporting DenseMatrix operations. */
trait DenseMatrixOps {
  implicit val denseMatrixArith = new TensorArith[(Int,Int),DenseMatrix,Tensor2[Int,Int],Shape2];

  implicit val dmtranspose = new MatrixTranspose[DenseMatrix,DenseMatrix] {
    def makeTranspose(op: Tensor2Op[DenseMatrix]) = DenseMatrixTransOp(op);
  }

  implicit val dmCholesky = new CholeskyDecomposer[DenseMatrix,DenseMatrix] {
    def decompose(op: Tensor2Op[DenseMatrix]) = DenseCholeskyDecomposition[DenseMatrix](op);
  }

  implicit val dmPower = new TensorPower[DenseMatrix,DenseMatrix] {
    def power(op: Tensor2Op[DenseMatrix], scale:Double) = DenseMatrixPower(op,scale);
  }


  implicit val denseMatrixSolveDenseVector: TensorSolver[DenseMatrix,DenseVector,DenseVector,Shape1Col,Shape1Col] =
    new TensorSolver[DenseMatrix,DenseVector,DenseVector,Shape1Col,Shape1Col] {
      def solve(m: TensorOp[DenseMatrix,Shape2], v: TensorOp[DenseVector,Shape1Col]): TensorOp[DenseVector,Shape1Col] =  {
        DenseMatrixSolveDenseVector(m,v);
      }
    }

  implicit val denseMatrixSolveDenseMatrix = new TensorSolver[DenseMatrix,DenseMatrix,DenseMatrix,Shape1Col,Shape2] {
    def solve(m: TensorOp[DenseMatrix,Shape2], m2: TensorOp[DenseMatrix,Shape2]): TensorOp[DenseMatrix,Shape2] =  {
      DenseMatrixSolveDenseMatrix(m,m2);
    }
  }

  implicit val denseMatrixPBuilder: TensorProductBuilder[DenseMatrix,Matrix,DenseMatrix,Shape2,Shape2,Shape2] = {
    new TensorProductBuilder[DenseMatrix,Matrix,DenseMatrix,Shape2,Shape2,Shape2] {
      def create(t: DenseMatrix, t2: Matrix) = new DenseMatrix(t.rows,t2.cols);
      def makeProduct(t: Tensor2Op[DenseMatrix], t2: Tensor2Op[Matrix]) = {
        Tensor2MultTensor2[Int,Int,Int,DenseMatrix,DenseMatrix,Matrix](t,t2);
      }
    }
  }

  implicit val denseMatrixVPBuilder : TensorProductBuilder[DenseMatrix,Vector,DenseVector,Shape2,Shape1Col,Shape1Col] =
    new TensorProductBuilder[DenseMatrix,Vector,DenseVector,Shape2,Shape1Col,Shape1Col] {
      def create(t: DenseMatrix, t2: Vector) = new DenseVector(t.rows);
      def makeProduct(t: Tensor2Op[DenseMatrix], t2: ColTensor1Op[Vector]) = {
        Tensor2MultColTensor1[Int,Int,Vector,DenseMatrix,DenseVector](t,t2);
      }
  }

}

/** Singleton instance of DenseMatrixOps trait. */
object DenseMatrixOps extends DenseMatrixOps;


/** Type-safe transposes of a Matrix. */
case class DenseMatrixTransOp
(op : Tensor2Op[DenseMatrix])
extends MatrixOp[DenseMatrix] {
  override lazy val value = {
    val opVal = op.value;
    val lk: DenseMatrix = opVal.matrixLike(opVal.cols,opVal.rows);
    lk := opVal.transpose;
    lk;
  }
  override def working = {
    val opVal = op.working;
    val lk: DenseMatrix = opVal.matrixLike(opVal.cols,opVal.rows);
    lk := opVal.transpose;
    lk;
  }
}


case class DenseCholeskyDecomposition[M<:DenseMatrix](m: DenseMatrixOp[M]) 
    extends TensorReferenceOp[M,Shape2](m) {
  
  override lazy val value = {
    val r = m.working;
    val (rows,cols) = r.dimensions;
    require(rows == cols, "Matrix is not square!");

    scalala.tensor.dense.Numerics.lapack.potrf("L", rows, r.data);
    // Now zero out the rest.
    for(k <- 1 until cols) {
      java.util.Arrays.fill(r.data, k * rows, k * rows + k,0.0);
    }
    
    r;
  }
}

/*
// Based on "Efficient Cholesky" http://www.maths.ed.ac.uk/~s0455378/EfficientCholesky.pdf
// Returns a lower triangular "square root" of this matrix
// http://www.hpjava.org/papers/HPJava/HPJava/node33.html
case class DenseCholeskyDecomposition[M<:DenseMatrix](m: DenseMatrixOp[M]) 
    extends TensorReferenceOp[M,Shape2](m) {
  
  override lazy val value = {
    val r = m.working;
    val (rows,cols) = r.dimensions;
    require(rows == cols, "Matrix is not square!");
    for(i <- 0 until cols) {
      // divide rows i+1 to rows by r(i,i);

      // make lower triangular:
      for(k <- i+1 until cols) {
        r(i,k) = 0.0;
      }

      r(i,i) = Math.sqrt(r(i,i));

      for(k <- i+1 until rows) {
        r(k,i) /= r(i,i);
      }

      for(j <- i+1 until cols;
          // col j := col j - r(j,i) * r(k,i)
          k <- j until rows) {
        r(k,j) -= r(j,i) * r(k,i);
      }
    }

    r;
  }
}
*/

/** Matrix solve vector, like matlab's "\", uses DenseMatrixSolveDenseMatrix. */
case class DenseMatrixPower
(m : DenseMatrixOp[DenseMatrix], p : Double)
extends DenseMatrixOp[DenseMatrix] {
  import OperatorImplicits._;

  override lazy val value = {
    val b = m.working;
    val (_V,_D) = Matrices.eig(b);
    val result = (_V.t \ (_V*Matrices.diag(_D:^p)).t).t value;
    result;
  }

}



/** Matrix solve vector, like matlab's "\", uses DenseMatrixSolveDenseMatrix. */
case class DenseMatrixSolveDenseVector
(m : DenseMatrixOp[DenseMatrix], v : ColDenseVectorOp[DenseVector])
extends ColDenseVectorOp[DenseVector] {
  import OperatorImplicits._;
  
  /** Solves via repurposing the DenseMatrixSolveDenseMatrix code. */
  override lazy val value = {
    val _b = v.working;
    val _B = new DenseMatrix(_b.size, 1, _b.data);
    val _X = DenseMatrixSolveDenseMatrix(m, _B).value;
    
    new DenseVector(_X.data);
  }
  
}

class MatrixSingularException extends RuntimeException;

/**
 * DenseMatrix solver based on code from MTJ 0.9.9.
 */
case class DenseMatrixSolveDenseMatrix
(a : DenseMatrixOp[DenseMatrix], b : DenseMatrixOp[DenseMatrix])
extends TensorReferenceOp[DenseMatrix,Shape2](a) {
  import scalala.tensor.dense.Numerics;
  
  override lazy val value : DenseMatrix = {
    

    val aWorking = a.working;
    val _B = b.value; 
    
    val domain = ProductSet(aWorking.domain._2,
                            _B.domain._2);

    val rows = domain._1.size;
    val cols = domain._2.size;

    // from MTJ 0.9.9
    if (aWorking.domain._1 == aWorking.domain._2) { // square?
      // LUSolve
      val _A = aWorking; // will be overwritten
      val _B : DenseMatrix = b.value;   // won't be overwritten
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
      rv;
    } else {
      // QRSolve
      val (trans,_A) = a match {
        case DenseMatrixTransOp(aT) => (true,aT.working.asInstanceOf[DenseMatrix]);
        case _ => (false, aWorking);
      }
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
            Math.max(queryWork(0).toInt, 1);
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
      rv;
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

